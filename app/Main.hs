{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (encodeFile)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.List (intersperse, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Calendar (Day, fromGregorian, toGregorian, showGregorian)
import Data.Text (pack)
import Nix.Versions.Json (nixpkgs, headAt, downloadVersionsInPeriod, downloadFromNix
                         , PackagesJSON(..), InfoJSON(..))
import System.TimeIt (timeItNamed)
import Nix.Versions.Types (Channel(..), Name(..), Hash(..), Commit(..))
import Text.Parsec (parse)

import qualified Data.HashMap.Strict as H
import qualified Nix.Versions.Json as Json
import qualified Nix.Versions.Database as DB
import qualified Nix.Versions.Database.Persistent as Persistent

import qualified Nix.Versions.Discover as Discover
import qualified Nix.Versions.Parsers as Parsers
import qualified Nix.Versions as V

from :: Day
from = read "2014-01-01"

to :: Day
to = read "2019-02-01"

main :: IO ()
main = do
    conn <- Persistent.connect Persistent.defaultDBFileName
    res <- Persistent.versions conn (Name "haskellPackages.hlint")
    showVersions res

saveEntireDatabase :: IO ()
saveEntireDatabase = do
    print "Loading database from JSON"
    Right db <- timeItNamed "Loading databases from JSON" $ V.loadDatabase from to
    print "Creating SQL DB"
    conn <- Persistent.connect Persistent.defaultDBFileName
    timeItNamed "Saving to SQL" $ Persistent.persist conn db
    forever $ do
        putStrLn "Finding versions in JSON"
        pkg <- getName
        res <- timeItNamed "JSON" $ inJSON db pkg
        showVersions res


        putStrLn "Finding versions in SQL"
        pkg <- getName
        res <- timeItNamed "SQL" $ Persistent.versions conn pkg
        showVersions res
    where
        inJSON db name = do
            res <- return $ fromMaybe [] $ DB.versions db name
            return $ seq res res

getName :: IO Name
getName = Name . pack <$> getLine

showVersions :: Show a => [a] -> IO ()
showVersions = putStrLn . unlines . fmap show



findPackages :: IO ()
findPackages = do
    Right db <- V.loadDatabase from to
    forever $ do
        putStrLn "Type a package name"
        pkg <- getLine
        putStrLn $ unlines $ fmap show $ fromMaybe [] $ DB.versions db (Name $ pack pkg)

download :: IO ()
download = do
    versions <- downloadVersionsInPeriod from to
    let (failures, successes) = partitionEithers versions
    putStrLn "Failures:"
    putStrLn $ unlines $ fmap show failures

    putStrLn "Succeses:"
    --putStrLn $ unlines successes


saveDatabase :: IO ()
saveDatabase = do
    commits <- commitsBetween from to
    print "Got commits"
    (errors, jsons) <- partitionEithers <$> mapConcurrently Json.load commits
    putStrLn $ unlines errors
    print $ "Loaded jsons: " <> show (length jsons)
    let db = foldMap DB.create jsons
    print "Saving database"
    V.saveDatabase from to db

commitsBetween :: Day -> Day -> IO [Commit]
commitsBetween from to = mapMaybe id <$> mapConcurrently (headAt nixpkgs) dates
    where
        (fromYear, _, _) = toGregorian from

        (toYear, _, _) = toGregorian to

        dates
            = takeWhile (<= to)
            $ dropWhile (< from)
            $ [fromGregorian year month 1
              | year <- [fromYear .. toYear]
              , month <- [1..12]
              ]
