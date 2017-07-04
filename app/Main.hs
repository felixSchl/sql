{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude
import Turtle hiding ((<>), stderr, switch, (</>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (for_)
import Data.Traversable (sequence)
import Data.List (nub)
import Data.Text (Text)
import Data.Text.Lazy.IO (getContents)
import qualified Data.Text as T
import Data.Aeson
import Data.Semigroup ((<>))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Safe (headMay)
import Control.Monad (filterM, join, void)
import Control.Monad (unless)
import Control.Monad.Trans.Either (EitherT, runEitherT, left, right, hoistEither)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent
import qualified Control.Arrow as A (left)
import System.Directory
import System.FilePath ((</>))
import System.Exit (exitWith, ExitCode(..))
import Options.Applicative

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
                            Nothing -> left $ "Target " <> show target <> " not found in configuration"
                            Just db -> right db

            -- run each computation in parallel, then wait for it's exit code
            input <- getContents
            rets <- lift $ traverse takeMVar =<< do
                sequence $ flip fmap dbTargets $ \db -> do
                    v <- newEmptyMVar
                    v <$ do
                        flip forkFinally (putMVar v . either (const $ ExitFailure 1) id) $ do
                            runSQL db input

            when (any (/= ExitSuccess) rets) $ do
                left "One or more databases reported an error"

    die msg = do
        err $ fromString $ "FATAL: " <> msg
        exitWith $ ExitFailure 1

    runSQL :: Database -> Text -> IO ExitCode
    runSQL Database{..} query = do
        let args = catMaybes
                    [ pure "-Nsr"
                    , fmap ("-u " <>) dbMUser
                    , fmap ("-p " <>) dbMPass
                    , fmap ("-h " <>) dbMServer
                    ]
        proc "mysql" args query

    loadDatabasesFromConfig :: EitherT String IO (Map Text Database)
    loadDatabasesFromConfig = do

        searchPaths <- lift $ fmap (</> "sql/.databases.json") . nub <$>
            sequence [ getHomeDirectory
                     , getXdgDirectory XdgConfig ""
                     ]

        configFile <- do
            (lift $ headMay <$> filterM doesFileExist searchPaths)
                >>= maybe (left $ "Couldn't find .databases.json in the following paths: " <> show searchPaths)
                          right

        hoistEither =<< do
            A.left ("Failed to parse config file JSON (" <> configFile <> "):\n" ++)
                . eitherDecode
                    <$> lift (BS.readFile configFile)
