{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Nix.Versions.Json (fetch, PackagesJSON(..), InfoJSON(..))
import Nix.Versions.Types (Channel(..), Name(..))

import qualified Data.HashMap.Strict as H

import qualified Nix.Versions.Discover as Discover

main :: IO ()
main = do
    pkgs <- fetch UnstableNixOS
    print $ "Commit :" <> show (commit pkgs)
    Just ghc <-return $ H.lookup (Name "ghc") (packages pkgs)
    print ghc
    versionsFound <- fromMaybe mempty $ Discover.allVersionsOf (Name "ghc") <$> nixpkgsPath ghc
    print versionsFound
    return ()
