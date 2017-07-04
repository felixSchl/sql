{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude
import Data.Maybe (catMaybes)
import Data.Traversable (sequence)
import Data.List (nub)
import Safe (headMay)
import Data.Semigroup ((<>))
import Control.Monad (filterM)
import System.Directory
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))
import Options.Applicative

data Args = Args
    { argsTargets :: [String] }

data Database = Database
    {
    } deriving (Show)

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (args <**> helper) (fullDesc)
    args = Args <$> many (argument str (metavar "TARGET..."))
    run Args{..} = do
        dbs <- either die pure =<< loadDatabasesFromConfig
        putStrLn $ show dbs
        pure ()
    die msg = do
        hPutStrLn stderr msg
        exitWith $ ExitFailure 1

    loadDatabasesFromConfig :: IO (Either String [Database])
    loadDatabasesFromConfig = do

        searchPaths <- fmap (</> "sql/.databases.json") . nub <$>
            sequence [ getHomeDirectory
                     , getXdgDirectory XdgConfig ""
                     ]

        mChosenConfigFile <- headMay <$>
            filterM (doesFileExist) searchPaths

        case mChosenConfigFile of
            Nothing -> do
                pure $ Left $ "Couldn't find .databases.json in the following paths: " <> show searchPaths
            Just configFile -> do
                databases <- pure []
                pure $ Right databases
