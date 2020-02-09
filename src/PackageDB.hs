{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module PackageDB (generate, getInfo) where

{-|
    This module takes care of finding the path of a package in the
    nixpkgs repo
-}

import Data.Aeson (FromJSON(..), ToJSON, eitherDecodeFileStrict, withObject, (.:), (.:?))
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)

import qualified Data.HashMap.Strict as H

generate :: FilePath -> IO PackageDB
generate filePath = do
    putStrLn "Generating package database ..."
    db <- eitherDecodeFileStrict filePath
    putStrLn "Package database generated."
    return $ either error id db

getInfo :: String -> PackageDB -> Maybe PackageInfo
getInfo str = H.lookup str . unPackageDB

newtype PackageDB = PackageDB { unPackageDB :: HashMap PackageName PackageInfo }
    deriving (Generic)

instance ToJSON PackageDB

instance FromJSON PackageDB where
    parseJSON = withObject "PackageDB" $ \v ->
        PackageDB <$> (v .: "packages")

type PackageName = String

data PackageInfo = PackageInfo
    { packageNixpkgsPath :: Maybe FilePath
    } deriving (Show, Eq, Generic)

instance Hashable PackageInfo

instance ToJSON PackageInfo

instance FromJSON PackageInfo where
    parseJSON = withObject "PackageInfo" $ \v ->
        PackageInfo <$> (v .: "meta" >>= (.:? "position"))
