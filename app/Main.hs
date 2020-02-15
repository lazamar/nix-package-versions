{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (encodeFile)
import Data.Maybe (fromMaybe)
import Nix.Versions.Json (fetch, PackagesJSON(..), InfoJSON(..))
import Nix.Versions.Types (Channel(..), Name(..), Hash(..))

import qualified Data.HashMap.Strict as H

import qualified Nix.Versions.Discover as Discover

main :: IO ()
main = do
    pkgs <- fetch (Hash "05e5bd4e3abb69310cbf675192a7925a517f851c")
    print $ "Commit :" <> show (commit pkgs)
    encodeFile "result.txt"  pkgs
    print "Created file."
    --Just ghc <-return $ H.lookup (Name "ghc-6.10.4") (packages pkgs)
    --print ghc
    --versionsFound <- fromMaybe mempty $ Discover.allVersionsOf (Name "ghc") <$> nixpkgsPath ghc
    --print versionsFound
