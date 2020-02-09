module Lib
    ( someFunc
    ) where

import Version (searchVersions)
import qualified PackageDB

someFunc :: IO ()
someFunc = do
    db <- PackageDB.generate "./unstable.json"
    putStrLn "Type a package to find its versions: "
    package <- getLine
    version <- searchVersions package
    sequence $ fmap (putStrLn . show) version
    return ()
