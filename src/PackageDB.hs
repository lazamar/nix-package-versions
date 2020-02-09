module PackageDB (generate) where

{-|
    This module takes care of finding the path of a package in the
    nixpkgs repo
-}

import Data.HashMap.Strict (HashMap)

generate :: FilePath -> IO PackageDB
generate filepath = do
    putStrLn "Generating package database ..."
    db <- return $ PackageDB mempty
    putStrLn "Package database generated."
    return db

newtype PackageDB = PackageDB (HashMap PackageName PackageContent)

type PackageName = String

data PackageContent = PackageContent
    { name :: PackageName
    , nixpkgsPath :: FilePath
    }
