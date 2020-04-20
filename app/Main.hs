{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Data.List (isPrefixOf)
import Data.Time.Calendar (Day)
import Data.Text (pack)
import Nix.Versions.Types (DBFile(..),GitHubUser(..), CachePath(..), Config(..), Name(..))
import Control.Monad (mapM_)

import qualified App.Server as Server
import qualified Data.Map as Map
import qualified Nix.Versions.Database as Persistent
import qualified Nix.Versions as V
import qualified Data.ByteString.Char8 as B

from :: Day
from = read "2014-01-01"

to :: Day
to = read "2019-04-01"

main :: IO ()
main = do
    -- Server.run config
    downloadRevisions


downloadRevisions :: IO ()
downloadRevisions = do
    config <- getConfig
    result <- V.savePackageVersionsForPeriod config from to
    mapM_ print result

findVersion  :: IO ()
findVersion = do
    config <- getConfig
    conn <- Persistent.connect (config_cacheDirectory config) (config_databaseFile config)
    res <- Persistent.versions conn (Name "haskellPackages.hlint")
    showVersions res

getName :: IO Name
getName = Name . pack <$> getLine

showVersions :: Show a => [a] -> IO ()
showVersions = putStrLn . unlines . fmap show


getConfig :: IO Config
getConfig  = do
    !env <- dotenv
    return $ Config
        { config_databaseFile   = DBFile "SQL_DATABASE.db"
        , config_cacheDirectory = CachePath "./saved-versions"
        , config_gitHubUser     =
            GitHubUser
                (B.pack $ env Map.! "GIT_USER")
                (B.pack $ env Map.! "GIT_TOKEN")
        }

dotenv :: IO (Map.Map String String)
dotenv = Map.fromList . fmap toEntry . filter isValid . lines <$> readFile ".env"
    where
        toEntry = fmap tail . span (/= '=')

        isValid s = not ("#" `isPrefixOf` s) && not (null s)

