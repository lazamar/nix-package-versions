 {-# LANGUAGE DeriveGeneric #-}
 {-# LANGUAGE OverloadedStrings #-}

{-| This file handles the creation of a database of versions
   from package information coming from Nix
-}
module Nix.Versions.Database
    (create
    , versions
    ) where

import Data.Aeson (eitherDecodeFileStrict)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text, pack)
import Data.Time.Calendar (Day)
import Nix.Versions.Types (Hash, Version, Name, Commit(..))
import Nix.Versions.Json (PackagesJSON(PackagesJSON), InfoJSON)

import qualified Nix.Versions.Json as Json
import qualified Data.HashMap.Strict as HashMap

versions :: PackageDB -> Name -> Maybe [(Version, VersionInfo)]
versions (PackageDB db) name = HashMap.toList <$> HashMap.lookup name db

packageCount :: PackageDB -> Int
packageCount = HashMap.size . unPackageDB

packageNames :: PackageDB -> [Name]
packageNames = HashMap.keys . unPackageDB

-- PackageDB

newtype PackageDB = PackageDB { unPackageDB :: HashMap Name Versions }

instance Semigroup PackageDB where
    (<>) = merge

instance Monoid PackageDB where
    mempty = PackageDB mempty

type Versions = HashMap Version VersionInfo

data VersionInfo = VersionInfo
    { revision :: Hash
    , description :: Maybe Text
    , nixpath :: Maybe FilePath
    , date :: Day
    } deriving (Show)

create :: PackagesJSON -> PackageDB
create (PackagesJSON (Commit revision date) packages) = PackageDB $ HashMap.map toVersionInfo packages
    where
        toVersionInfo :: InfoJSON -> HashMap Version VersionInfo
        toVersionInfo info =
            HashMap.singleton (Json.version info) $ VersionInfo
                { revision = revision
                , description = Json.description info
                , nixpath = Json.nixpkgsPath info
                , date = date
                }

merge :: PackageDB -> PackageDB -> PackageDB
merge (PackageDB db1) (PackageDB db2) =
    PackageDB $ HashMap.unionWith (HashMap.unionWith mergeInfos) db1 db2
    where
        mergeInfos info1 info2 =
            if date info1 > date info2
               then info1
               else info2

