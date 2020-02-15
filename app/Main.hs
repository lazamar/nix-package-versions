{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Nix.Versions.Json (fetch, PackagesJSON(..), InfoJSON(..))
import Nix.Versions.Types (Channel(..), Name(..))
import qualified Data.HashMap.Strict as H

main :: IO ()
main = do
    pkgs <- fetch UnstableNixOS
    print $ "Commit :" <> show (commit pkgs)
    print $ H.lookup (Name "ghc") $ packages pkgs
    return ()
