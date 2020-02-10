{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Monad (unless)
import Data.HashMap.Strict as H
import Version (searchVersions)
import qualified PackageDB

someFunc :: IO ()
someFunc = do
    db <- PackageDB.generate "./unstable.json"

    let getPkg = do
            print $ PackageDB.getInfo "ghc" db
            print $ PackageDB.getInfo "haskellPackages.hoogle" db
    getPkg

    --putStrLn "Type a package to find its versions: "
    --package <- getLine
    --version <- searchVersions package
    --sequence $ fmap (putStrLn . show) version
    putStrLn $ "Number of packages loaded: " <> show (PackageDB.packageCount db)
    return ()
