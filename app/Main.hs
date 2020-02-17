{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (encodeFile)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.List (intersperse, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Time.Calendar (Day, fromGregorian, toGregorian, showGregorian)
import Nix.Versions.Json (nixpkgs, headAt, downloadVersionsInPeriod, downloadFromNix, PackagesJSON(..)
                         , InfoJSON(..))
import Nix.Versions.Types (Channel(..), Name(..), Hash(..), Commit(..))
import Text.Parsec (parse)

import qualified Data.HashMap.Strict as H
import qualified Nix.Versions.Json as Json
import qualified Nix.Versions.Database as DB

import qualified Nix.Versions.Discover as Discover
import qualified Nix.Versions.Parsers as Parsers

from :: Day
from = read "2014-01-01"

to :: Day
to = read "2019-02-01"

main :: IO ()
main = readCommits

download = do
    versions <- downloadVersionsInPeriod from to
    let (failures, successes) = partitionEithers versions
    putStrLn "Failures:"
    putStrLn $ unlines $ fmap show failures

    putStrLn "Succeses:"
    putStrLn $ unlines successes


readCommits :: IO ()
readCommits = do
    commits <- commitsBetween (read "2019-01-01") to
    print "Got commits"
    (errors, jsons) <- partitionEithers <$> mapConcurrently Json.load commits
    putStrLn $ unlines errors
    print $ "Loaded jsons: " <> show (length jsons)
    let db = foldMap DB.create jsons
    print "GHC Versions:"
    putStrLn $ unlines $ fmap show
        $ sortBy (compareBy $ DB.date . snd)
        $ fromMaybe []
        $ DB.versions db (Name "ghc")

    where
        compareBy f a b = compare (f a) (f b)

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
