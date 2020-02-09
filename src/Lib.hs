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
            putStrLn "Type a package name to find its nixpkgs path: "
            pname <- getLine
            print $ PackageDB.getInfo pname db
            putStrLn "Continue? (y/n)"
            v <- getLine
            unless ("n" == v) getPkg

    getPkg

    --putStrLn "Type a package to find its versions: "
    --package <- getLine
    --version <- searchVersions package
    --sequence $ fmap (putStrLn . show) version
    return ()
