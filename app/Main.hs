{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Data.ByteString.Char8 as B
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
import System.FilePath ((</>))

import Data.Time.Period (Period(..))
import qualified GitHub
import qualified App.Storage.SQLite as SQLite
import App.Update (updateDatabase, frequencyDays, Frequency)

import qualified Server as Server

-- CLI
data CLIOptions
  = RunServer Server.Port DbRoot
  | UpdateVersions
    { cli_downloadFrom :: Day
    , cli_downloadTo   :: Day
    , cli_interval     :: Int
    , cli_user         :: GitHub.AuthenticatingUser
    , cli_dbRoot       :: DbRoot
    }
  deriving Show

newtype DbRoot = DbRoot FilePath
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
    <$> option (Server.Port <$> auto)
        (  long "port"
        <> metavar "PORT"
        <> showDefault
        <> value (Server.Port 8080)
        <> help "Port to run the server"
        )
    <*> dbroot

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
    <*> dbroot

  dbroot :: Parser DbRoot
  dbroot = DbRoot <$> strOption
    ( long "db-root"
    <> help (unwords
      [ "Directory of database files."
      , "It is the place where the database will be created in or loaded from."
      ])
    <> metavar "PATH"
    )

  gitHubCredentials = pure GitHub.AuthenticatingUser
    <*> (B.pack <$> strOption
        ( long "github-user"
        <> help (unwords
          [ "GitHub API user name to send in requests for commits."
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
    RunServer port root -> runServer root port
    UpdateVersions from to interval user root -> do
      let start = utcTimeToPOSIXSeconds $ UTCTime from 0
          end = utcTimeToPOSIXSeconds $ UTCTime to 0
          period = Period start end
      downloadRevisions root user (frequencyDays interval) period
  where
  downloadRevisions :: DbRoot -> GitHub.AuthenticatingUser -> Frequency -> Period -> IO ()
  downloadRevisions root user frequency period = do
    hSetBuffering stdout LineBuffering
    SQLite.withDatabase (dbPath root) $ \database -> do
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

  runServer root port = do
    SQLite.withDatabase (dbPath root) $ \database ->
      Server.run database port

dbPath :: DbRoot -> FilePath
dbPath (DbRoot path) = path </> "DATABASE.db"
