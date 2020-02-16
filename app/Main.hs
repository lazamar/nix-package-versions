{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (encodeFile)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Nix.Versions.Json (nixpkgs, headAt, downloadVersionsInPeriod, downloadFromNix, PackagesJSON(..), InfoJSON(..))
import Nix.Versions.Types (Channel(..), Name(..), Hash(..))

import qualified Data.HashMap.Strict as H

import qualified Nix.Versions.Discover as Discover

main :: IO ()
main = do
    files <- downloadVersionsInPeriod (read "2014-01-01") (read "2019-02-01")
    let (failures, successes) = partitionEithers files
    putStrLn $ unlines $ "Created files: ":successes
    putStrLn $ "Failures: " <>  unlines (intersperse "--------" $ fmap (uncurry mappend . first show) failures)
    return ()
