{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (main, getConfig, run) where

import Data.Foldable (fold)
import Data.Either (isRight, isLeft)
import Data.List (isPrefixOf)
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Text (unpack)
import Text.Read (readMaybe)
import Nix.Versions.Types
    ( DBFile(..)
    , GitHubUser(..)
    , CachePath(..)
    , Config(..)
    , Task(..)
    , Commit(..)
    , Hash(..))
import Control.Monad (mapM_)
import Control.Monad.Conc.Class (getNumCapabilities)
import Control.Monad.Log2 (runLoggerT, inTerminal, logInfoTimed)
import Control.Monad.Log (logInfo, Handler, WithSeverity)
import Control.Monad.IO.Class (liftIO)
import Options.Applicative
    (info, option, auto, str, helper, switch, help, metavar, fullDesc, progDesc
    , header, long, showDefault, value, Parser, execParser, (<**>), command
    , subparser
    )
import Data.Semigroup ((<>))
import System.Exit (exitFailure)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Posix.Files (fileExist, removeLink)
import Server (Port(..))

import qualified Server as Server
import qualified Data.Map as Map
import qualified Nix.Versions as V
import qualified Nix.Revision as Revision
import qualified Nix.Versions.Database as DB
import qualified Data.ByteString.Char8 as B

import Control.Monad.Revisions
import Control.Monad.LimitedConc

-- CLI
data CLIOptions
  = RunServer Port
  | UpdateVersions
    { cli_downloadFrom :: Day
    , cli_downloadTo   :: Day
    }
  deriving (Show, Eq)

cliOptions :: IO CLIOptions
cliOptions  = do
    today <- utctDay . systemToUTCTime <$> getSystemTime
    execParser $ info (options today <**> helper) $ fold
      [ fullDesc
      , progDesc "Serve information about older Nix package versions or download them"
      , header "nix-package-versions - a server to search past versions of Nix packages"
      ]
    where
    options :: Day -> Parser CLIOptions
    options today = subparser $ fold
      [ command "server" $ info (server <**> helper) $
          progDesc "Serve information about older Nix package versions"

      , command "update" $ info (update <**> helper) $
          progDesc "Download new package versions from Nix instead of running a server"
      ]

    server :: Parser CLIOptions
    server = RunServer
      <$> option (Port <$> auto)
          (  long "port"
          <> metavar "PORT"
          <> showDefault
          <> value (Port 8080)
          <> help "Port to run the server"
          )

    update :: Parser CLIOptions
    update = UpdateVersions
      <$> option auto
          ( long "from"
          <> help "The date to download data from. YYYY-MM-DD"
          <> metavar "DATE"
          )
      <*> option auto
          ( long "until"
          <> help "The date to download data until. YYYY-MM-DD"
          <> metavar "DATE"
          )

main :: IO ()
main = do
  options <- cliOptions
  case options of
    RunServer port -> runServer port
    UpdateVersions from to -> downloadRevisions from to
  where
  downloadRevisions :: Day -> Day -> IO ()
  downloadRevisions from to = do
    hSetBuffering stdout LineBuffering
    config <- getConfig
    run config inTerminal $ do
        result <- logInfoTimed "savePackageVersionsForPeriod" $ V.savePackageVersionsForPeriod config from to
        mapM_ (logInfo . show) result
        now <- liftIO $ getCurrentTime
        logInfo $ unlines
            [ ""
            , show now
            , "Saved package versions from " <> showGregorian from <> " to " <> showGregorian to
            , "Successes: " <> show (length $ filter isRight result)
            , "Failures:  " <> show (length $ filter isLeft result)
            ]

  --runServer :: Port -> IO ()
  runServer port = do
    config <- getConfig
    run config inTerminal $ Server.run port config

-- | Run our monad stack
run :: Config -> Handler IO (WithSeverity String) -> _ -> IO ()
run (Config dbFile cacheDir _) handler action = do
    capabilities <- getNumCapabilities

    runLoggerT handler
        $ runMonadLimitedConc (Map.fromList [(BuildNixRevision, max 1 $ capabilities - 3)])
        $ runMonadRevisions
        $ DB.withConnection cacheDir dbFile action


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
