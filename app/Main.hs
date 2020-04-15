{-# LANGUAGE OverloadedStrings #-}

module Main (main, config) where

import Control.Monad (forever)
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (encodeFile)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.List (intersperse, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Calendar (Day, fromGregorian, toGregorian, showGregorian)
import Data.Text (pack)
import System.TimeIt (timeItNamed)
import Nix.Versions.Types (DBFile(..),GitHubUser(..), CachePath(..), Config(..), Channel(..), Name(..), Hash(..), Commit(..))
import Text.Parsec (parse)
import Control.Monad (mapM_)

import qualified Data.HashMap.Strict as H
import qualified Nix.Revision as Revision
import qualified Nix.Versions.Database as Persistent

import qualified Nix.Versions as V
import qualified App.Server as Server

config :: Config
config = Config
    { config_databaseFile   = DBFile "SQL_DATABASE.db"
    , config_cacheDirectory = CachePath "./saved-versions"
    , config_gitHubUser     = GitHubUser "lazamar"
    }

from :: Day
from = read "2014-01-01"

to :: Day
to = read "2019-04-01"

main :: IO ()
main = Server.run config

downloadRevisions = do
    result <- V.savePackageVersionsForPeriod config from to
    mapM_ print result

findVersion = do
    conn <- Persistent.connect (config_cacheDirectory config) (config_databaseFile config)
    res <- Persistent.versions conn (Name "haskellPackages.hlint")
    showVersions res

getName :: IO Name
getName = Name . pack <$> getLine

showVersions :: Show a => [a] -> IO ()
showVersions = putStrLn . unlines . fmap show
