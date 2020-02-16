{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (encodeFile)
import Data.Maybe (fromMaybe)
import Nix.Versions.Json (nixpkgs, headAt, downloadFromNix, PackagesJSON(..), InfoJSON(..))
import Nix.Versions.Types (Channel(..), Name(..), Hash(..))

import qualified Data.HashMap.Strict as H

import qualified Nix.Versions.Discover as Discover

main :: IO ()
main = do
    Just head <- headAt nixpkgs $ read "2017-06-01"

    pkgs <- downloadFromNix head
    --print $ "Commit :" <> show (commit pkgs)
    --encodeFile "result.txt"  pkgs
    print $ "Created file " <> pkgs
    --Just ghc <-return $ H.lookup (Name "ghc-6.10.4") (packages pkgs)
    --print ghc
    --versionsFound <- fromMaybe mempty $ Discover.allVersionsOf (Name "ghc") <$> nixpkgsPath ghc
    --print versionsFound
