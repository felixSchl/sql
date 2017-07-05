{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude
import Data.Maybe (catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (for_)
import Data.Traversable (sequence)
import Data.List (nub)
import Data.Text (Text)
import Data.Text.IO (hPutStrLn)
import qualified Data.Text as T
import Data.Aeson
import Data.Semigroup ((<>))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Safe (headMay)
import Control.Monad (filterM, when)
import Control.Monad.Trans.Either (EitherT, runEitherT, left, right, hoistEither)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent
import qualified Control.Arrow as A (left)
import System.IO (stderr, stdout)
import System.Directory (getHomeDirectory, getXdgDirectory, XdgDirectory(..),
                        doesFileExist)
import System.Process (runInteractiveProcess, waitForProcess)
import System.FilePath ((</>))
import System.Exit (exitWith, ExitCode(..))
import Options.Applicative
import Pipes ((<-<), (>->), (>~))
import System.Process
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Concurrent as P
import qualified Pipes.Text.IO as Text

data Args = Args
    { argsAll     :: Bool
    , argsTargets :: [String] }

data Database = Database
    { dbMAppServer :: Maybe Text
    , dbMServer    :: Maybe Text
    , dbName       :: Text
    , dbMUser      :: Maybe Text
    , dbMPass      :: Maybe Text
    } deriving (Show)

instance FromJSON Database where
    parseJSON = withObject "Database" $ \v -> Database
        <$> v .:? "appServer"
        <*> v .:? "dbServer"
        <*> v .:  "dbName"
        <*> v .:? "user"
        <*> v .:? "pass"

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (args <**> helper) (fullDesc)
    args = Args
        -- note: It would be nice if this could be `--all | [<target>]...`
        <$> (switch (long "all" <> short 'a' <> help "Run on all targets"))
        <*> (many (argument str (metavar "TARGET...")))
    run Args{..} = either die pure =<< do
        runEitherT $ do
            -- load database configurations. They are referred to by name later
            dbs <- loadDatabasesFromConfig

            -- find the requested datbases in the configuration file
            dbTargets <- if argsAll
                then pure $ snd <$> Map.toList dbs
                else sequence $ flip fmap argsTargets $ \target ->
                        case Map.lookup (T.pack target) dbs of
                            Nothing -> left $ "Target " <> (T.pack $ show target) <> " not found in configuration"
                            Just db -> right db

            -- run each computation in parallel, then wait for it's exit code
            procs <- sequence $ flip fmap dbTargets $ \db -> lift $ do
                (output, input) <- P.spawn P.unbounded
                v <- newEmptyMVar
                (output, v) <$ do
                    flip forkFinally (putMVar v . either (const $ ExitFailure 1) id) $ do
                        runSQL db $ P.fromInput input

            -- fan out all stdin input to all processes
            lift $ forkIO $ P.runEffect $ P.for Text.stdin $ \line ->
                lift $ do
                    for_ (fst <$> procs) (P.atomically . flip P.send line)

            rets <- lift $ traverse takeMVar $ snd <$> procs

            when (any (/= ExitSuccess) rets) $ do
                left "One or more databases reported an error"

    die msg = do
        hPutStrLn stderr $ "FATAL: " <> msg
        exitWith $ ExitFailure 1

    runSQL :: Database -> P.Producer Text IO () -> IO ExitCode
    runSQL Database{..} stdinP = do
        let args = T.unpack <$> catMaybes
                    [ pure "-Nsr"
                    , fmap ("-u" <>) dbMUser
                    , fmap ("-p" <>) dbMPass
                    , fmap ("-h" <>) dbMServer
                    , pure dbName
                    ]
        (inH, outH, errH, pH) <- 
            runInteractiveProcess "mysql" args Nothing Nothing

        forkIO $ P.runEffect $ Text.fromHandle outH >-> Text.toHandle stdout
        forkIO $ P.runEffect $ Text.fromHandle errH >-> Text.toHandle stderr
        forkIO $ P.runEffect $ P.for stdinP (lift . hPutStrLn inH)

        waitForProcess pH

    loadDatabasesFromConfig :: EitherT Text IO (Map Text Database)
    loadDatabasesFromConfig = do

        searchPaths <- lift $ fmap (</> "sql/.databases.json") . nub <$>
            sequence [ getHomeDirectory
                     , getXdgDirectory XdgConfig ""
                     ]

        configFile <- do
            (lift $ headMay <$> filterM doesFileExist searchPaths)
                >>= maybe (left $ "Couldn't find .databases.json in the following paths: " <> (T.pack $ show searchPaths))
                          right

        hoistEither =<< do
            A.left ((("Failed to parse config file JSON (" <> T.pack configFile <> "):\n") <>) . T.pack)
                . eitherDecode
                    <$> lift (BS.readFile configFile)
