module Lib
    ( someFunc
    ) where

import Data.HashMap.Strict as H
import Version (searchVersions)
import qualified PackageDB

someFunc :: IO ()
someFunc = do
    db <- PackageDB.generate "./unstable.json"

    putStrLn "Type a package name to find its nixpkgs path: "
    pname <- getLine
    let packagePath = PackageDB.getInfo pname db
    print packagePath

    --putStrLn "Type a package to find its versions: "
    --package <- getLine
    --version <- searchVersions package
    --sequence $ fmap (putStrLn . show) version
    return ()
