{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (main) where

import Data.List (isPrefixOf)
import Data.Time.Calendar (Day)
import Text.Read (readMaybe)
import Nix.Versions.Types (DBFile(..),GitHubUser(..), CachePath(..), Config(..), Task)
import Control.Monad (mapM_)
import Control.Monad.Log2 (runLoggerT, pretty)
import Control.Monad.Log (logInfo)
import Options.Applicative
    (info, option, auto, str, helper, switch, help, metavar, fullDesc, progDesc
    , header, long, showDefault, value, Parser, execParser, (<**>)
    )
import Data.Semigroup ((<>))
import System.Exit (exitFailure)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import App.Server (Port(..))

import qualified App.Server as Server
import qualified Data.Map as Map
import qualified Nix.Versions as V
import qualified Data.ByteString.Char8 as B

import Control.Monad.Revisions
import Control.Monad.LimitedConc


main :: IO ()
main = do
    CommandLineOptions{..} <- cliOptions
    if not cli_downloadVersions
        then runServer cli_port
        else do
            case (cli_downloadFrom , cli_downloadTo) of
                (Just from, Just to) -> downloadRevisions from to
                _ -> do
                    putStrLn $ unwords
                        [ "Error: To download revisions you need to specify the period"
                        , "to download revisions for. Use --from and --to for that."
                        ]
                    exitFailure

    doTest
    where
        downloadRevisions :: Day -> Day -> IO ()
        downloadRevisions from to = do
            hSetBuffering stdout LineBuffering
            config <- getConfig
            runLoggerT (putStrLn . pretty) $ do
                result <- V.savePackageVersionsForPeriod config from to
                mapM_ (logInfo . show) result

        runServer :: Port -> IO ()
        runServer port = Server.run port =<< getConfig

        doTest = do
            config <- getConfig
            runMonadLimitedConc (mempty :: Map.Map Task Int)
                $ runMonadRevisions
                $ V.saveP config (read "2020-01-01")
            return ()

-------------------------------------------------------------------------------------------
-- CLI

data CommandLineOptions = CommandLineOptions
    { cli_port :: Port
    , cli_downloadVersions :: Bool
    , cli_downloadFrom :: Maybe Day
    , cli_downloadTo   :: Maybe Day
    }
    deriving (Show, Eq)

cliOptions :: IO CommandLineOptions
cliOptions  = execParser $ info
        (options  <**> helper)
        (fullDesc
            <> progDesc "Serve information about older Nix package versions or download them"
            <> header "nix-package-versions - a server to search past versions of Nix packages"
        )
    where
        options :: Parser CommandLineOptions
        options = CommandLineOptions
            <$> option (Port <$> auto)
                  ( long "port"
                  <> metavar "PORT"
                  <> showDefault
                  <> value (Port 8080)
                  <> help "Port to run the server"
                  )
            <*> switch
                  ( long "update-versions"
                  <> help "Download new package versions from Nix instead of running a server"
                  )
            <*> option (readMaybe <$> str)
                  ( long "from"
                  <> help "The date to download data from. YYYY-MM-DD"
                  <> value Nothing
                  <> metavar "DATE"
                  )
            <*> option (readMaybe <$> str)
                  ( long "until"
                  <> help "The date to download data until. YYYY-MM-DD"
                  <> value Nothing
                  <> metavar "DATE"
                  )

-------------------------------------------------------------------------------------------
-- Configuration

getConfig :: IO Config
getConfig  = do
    !env <- readDotenv ".env"
    return $ Config
        { config_databaseFile   = DBFile "SQL_DATABASE.db"
        , config_cacheDirectory = CachePath "./saved-versions"
        , config_gitHubUser     =
            GitHubUser
                (B.pack $ env Map.! "GIT_USER")
                (B.pack $ env Map.! "GIT_TOKEN")
        }

readDotenv :: FilePath -> IO (Map.Map String String)
readDotenv  path = Map.fromList . fmap toEntry . filter isValid . lines <$> readFile path
    where
        toEntry = fmap tail . span (/= '=')

        isValid s = not ("#" `isPrefixOf` s) && not (null s)
