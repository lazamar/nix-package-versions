{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.Foldable (fold)
import Data.Either (isRight, isLeft)
import Data.List (isPrefixOf)
import Data.Time.Calendar (Day, showGregorian)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Options.Applicative
    (info, option, auto, helper, help, metavar, fullDesc, progDesc
    , header, long, showDefault, value, Parser, execParser, (<**>), command
    , subparser
    )
import System.IO
  ( hSetBuffering
  , hPutStrLn
  , stdout
  , stderr
  , BufferMode(..))
import Server (Port(..))

import qualified Server as Server
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B

import qualified Nix
import qualified GitHub
import qualified App.Storage.SQLite as SQLite
import App.Update (savePackageVersionsForPeriod)

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
    (dbPath, user) <- getConfig
    SQLite.withDatabase dbPath $ \database -> do
      result <- Nix.withDownloader Nothing $ \downloader ->
        savePackageVersionsForPeriod database downloader user from to
      mapM_ (hPutStrLn stderr . show) result
      now <- getCurrentTime
      putStrLn $ unlines
          [ ""
          , show now
          , "Saved package versions from " <> showGregorian from <> " to " <> showGregorian to
          , "Successes: " <> show (length $ filter isRight result)
          , "Failures:  " <> show (length $ filter isLeft result)
          ]

  runServer port = do
    (dbPath, _) <- getConfig
    SQLite.withDatabase dbPath $ \database ->
      Server.run database port

-------------------------------------------------------------------------------------------
-- Configuration

type Config = (FilePath, GitHub.AuthenticatingUser)

getConfig :: IO Config
getConfig  = do
    !env <- readDotenv ".env"
    let dbPath = "./saved-versions/SQL_DATABASE.db"
        user = GitHub.AuthenticatingUser
          (B.pack $ env Map.! "GIT_USER")
          (B.pack $ env Map.! "GIT_TOKEN")
    return (dbPath, user)

readDotenv :: FilePath -> IO (Map.Map String String)
readDotenv  path = Map.fromList . fmap toEntry . filter isValid . lines <$> readFile path
    where
        toEntry = fmap tail . span (/= '=')

        isValid s = not ("#" `isPrefixOf` s) && not (null s)
