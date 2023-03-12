{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Foldable (fold)
import Data.Either (isRight, isLeft)
import Data.Time.Calendar (Day)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Options.Applicative
    (info, option, auto, helper, help, metavar, fullDesc, progDesc
    , header, long, showDefault, value, Parser, execParser, (<**>), command
    , subparser, strOption
    )
import Prettyprinter (pretty)
import System.IO
  ( hSetBuffering
  , hPutStrLn
  , stdout
  , stderr
  , BufferMode(..))
import Server (Port(..))

import qualified Server as Server
import qualified Data.ByteString.Char8 as B

import Data.Time.Period (Period(..))
import qualified GitHub
import qualified App.Storage.SQLite as SQLite
import App.Update (updateDatabase, frequencyDays, Frequency)

-- CLI
data CLIOptions
  = RunServer Port
  | UpdateVersions
    { cli_downloadFrom :: Day
    , cli_downloadTo   :: Day
    , cli_interval     :: Int
    , cli_user         :: GitHub.AuthenticatingUser
    }
  deriving Show

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

    , command "update" $ info (update today <**> helper) $
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

  update :: Day -> Parser CLIOptions
  update today = UpdateVersions
    <$> option auto
        ( long "from"
        <> help "The date to download data from. YYYY-MM-DD"
        <> metavar "DATE"
        )
    <*> option auto
        ( long "until"
        <> help "The date to download data until. YYYY-MM-DD"
        <> showDefault
        <> value today
        <> metavar "DATE"
        )
    <*> option auto
        ( long "frequency"
        <> help "Collect versions for each DAYS days period between from and until dates"
        <> showDefault
        <> value 30
        <> metavar "DAYS"
        )
    <*> gitHubCredentials

  gitHubCredentials = pure GitHub.AuthenticatingUser
    <*> (B.pack <$> strOption
        ( long "github-user"
        <> help (unwords
          [ "GitHub user name to send in requests for commits."
          , "Without a GitHub login attempts to update the database will"
          , "fail by quickly getting rate limited by Github."
          ])
        <> metavar "USER"
        ))
    <*> (B.pack <$> strOption
        ( long "github-token"
        <> help "GitHub API access token"
        <> metavar "TOKEN"
        ))

main :: IO ()
main = do
  options <- cliOptions
  case options of
    RunServer port -> runServer port
    UpdateVersions from to interval user -> do
      let start = utcTimeToPOSIXSeconds $ UTCTime from 0
          end = utcTimeToPOSIXSeconds $ UTCTime to 0
          period = Period start end
      downloadRevisions user (frequencyDays interval) period
  where
  downloadRevisions :: GitHub.AuthenticatingUser -> Frequency -> Period -> IO ()
  downloadRevisions user frequency period = do
    hSetBuffering stdout LineBuffering
    dbPath <- getConfig
    SQLite.withDatabase dbPath $ \database -> do
      result <- updateDatabase database frequency user period
      mapM_ (hPutStrLn stderr . show) result
      now <- getCurrentTime
      putStrLn $ unlines
          [ ""
          , show now
          , "Saved package versions for " <> show (pretty period)
          , "Successes: " <> show (length $ filter isRight result)
          , "Failures:  " <> show (length $ filter isLeft result)
          ]

  runServer port = do
    dbPath <- getConfig
    SQLite.withDatabase dbPath $ \database ->
      Server.run database port

-------------------------------------------------------------------------------------------
-- Configuration

type Config = FilePath

getConfig :: IO Config
getConfig  = do
    return "./saved-versions/SQL_DATABASE.db"
