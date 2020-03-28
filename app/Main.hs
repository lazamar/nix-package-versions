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
import Nix.Revision (nixpkgs, headAt, downloadVersionsInPeriod, downloadFromNix)
import System.TimeIt (timeItNamed)
import Nix.Versions.Types (Channel(..), Name(..), Hash(..), Commit(..))
import Text.Parsec (parse)

import qualified Data.HashMap.Strict as H
import qualified Nix.Revision as Revision
import qualified Nix.Versions.Database.Persistent as Persistent

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

getName :: IO Name
getName = Name . pack <$> getLine

showVersions :: Show a => [a] -> IO ()
showVersions = putStrLn . unlines . fmap show

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
