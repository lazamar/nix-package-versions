{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Monad (unless)
import Data.Aeson (encode)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Nix.Versions.Database (PackageInfo(..))
import System.TimeIt (timeItNamed)

import qualified Nix.Versions.Database
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as H

someFunc :: IO ()
someFunc = do
    -- (db, pathsDB) <- PackageDB.generate "./unstable.json"

    -- --let getPkg = do
    --         --print $ PackageDB.getInfo "ghc" db
    --         --print $ PackageDB.getInfo "haskellPackages.hoogle" db

    -- let names = PackageDB.packageNames db
    --     infos = mapMaybe (PackageDB.getInfo db) names
    --     inOneFile     = filter ((== 1) . pinfo_pathCount) infos
    --     inMoreThanOne = filter ((> 1)  . pinfo_pathCount) infos
    --     noFile        = filter ((== 0) . pinfo_pathCount) infos

    --     ranking =
    --         sortBy (\a b -> compare (snd b) (snd a))
    --         $ H.toList
    --         $ pathsDB

    -- --putStrLn "Type a package to find its versions: "
    -- --package <- getLine
    -- --version <- searchVersions package
    -- --sequence $ fmap (putStrLn . show) version
    -- putStrLn $ "Total number of packages loaded: " <> show (length names)
    -- putStrLn $ "Packages from files with one definition: " <> show (length inOneFile)
    -- putStrLn $ "Packages from files with multiple definition: " <> show (length inMoreThanOne)
    -- putStrLn $ "Packages without definition path: " <> show (length noFile)
    -- putStrLn $ "Paths with most packages"
    -- putStrLn $ unlines (fmap show $ take 30 ranking)
    return ()
