{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Control.Monad (unless)
import Data.HashMap.Strict as H
import qualified PackageDB
import System.TimeIt (timeItNamed)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BS

someFunc :: IO ()
someFunc = do
    db <- PackageDB.generate "./unstable.json"

    --let getPkg = do
            --print $ PackageDB.getInfo "ghc" db
            --print $ PackageDB.getInfo "haskellPackages.hoogle" db

    versions <- timeItNamed "Traversals"
                $ traverse (PackageDB.getVersions db) (take 5 $ PackageDB.packageNames db)
    BS.writeFile "./output.json" (encode versions)



    --putStrLn "Type a package to find its versions: "
    --package <- getLine
    --version <- searchVersions package
    --sequence $ fmap (putStrLn . show) version
    putStrLn $ "Number of packages loaded: " <> show (PackageDB.packageCount db)
    putStrLn $ "Versions found: " <> show (length versions)
    return ()
