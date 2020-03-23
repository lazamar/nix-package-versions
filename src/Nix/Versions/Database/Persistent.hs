{-|
This file handles saving and retrieving Database types from and to persistent storage.
It uses SQLite.
-}

module Nix.Versions.Database.Persistent
    ( defaultDBFileName
    , connect
    , versions
    , persist
    ) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (catchJust)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day(..), toModifiedJulianDay)
import Database.SQLite.Simple (ToRow(toRow), FromRow(fromRow), SQLData(..))
import Nix.Versions.Database (PackageDB(..), VersionInfo(..))
import Nix.Versions.Types (Hash(..), Version(..), Name(..), Commit(..))

import qualified Data.HashMap.Strict as HashMap
import qualified Database.SQLite.Simple as SQL

newtype Connection = Connection SQL.Connection
-- Constants

defaultDBFileName :: Text
defaultDBFileName = "SQL_DATABASE.db"

db_PACKAGE_NAMES = "PACKAGE_NAMES"
db_PACKAGE_VERSIONS = "PACKAGE_VERSIONS"

-- | Get a connection and prepare database for usage
connect :: Text -> IO Connection
connect dbFileName = do
    conn <- SQL.open $ unpack dbFileName
    -- Enable foreign key constraints.
    -- It's really weird that they would otherwise just not work.
    SQL.execute_ conn "PRAGMA foreign_keys = ON"
    ensureTablesAreCreated conn
    return $ Connection conn

ensureTablesAreCreated :: SQL.Connection -> IO ()
ensureTablesAreCreated conn = do
    SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS  " <> db_PACKAGE_NAMES <> " "
                        <> "( PACKAGE_NAME TEXT PRIMARY KEY"
                        <> ")"

    SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS  " <> db_PACKAGE_VERSIONS <> " "
                        <> "( PACKAGE_NAME TEXT NOT NULL"
                        <> ", VERSION_NAME TEXT NOT NULL"
                        <> ", REVISION_HASH TEXT NOT NULL"
                        <> ", DESCRIPTION TEXT"
                        <> ", NIXPATH TEXT"
                        <> ", DAY INTEGER NOT NULL"
                        <> ", PRIMARY KEY (PACKAGE_NAME, VERSION_NAME)"
                        <> ", FOREIGN KEY (PACKAGE_NAME) REFERENCES " <> db_PACKAGE_NAMES <> "(PACKAGE_NAME)"
                        <> ")"

-- | Retrieve all versions available for a package
versions :: Connection -> Name -> IO [(Version, VersionInfo)]
versions (Connection conn) (Name name) = do
    results <- SQL.query
        conn
        ("SELECT * FROM " <> db_PACKAGE_VERSIONS <> " WHERE PACKAGE_NAME = ?")
        [unpack name]
    return $ toVersionAndInfo <$> results
        where
            toVersionAndInfo (SQLPackageVersion (_, version, info)) = (version, info)

-- | Save the entire database
persist :: Connection -> PackageDB -> IO ()
persist conn (PackageDB packages) = do
    mapConcurrently_ (uncurry persistPackage) (HashMap.toList packages)
    return ()
    where
        persistPackage name versions =
            mapConcurrently_
                (uncurry $ persistVersion conn name)
                (HashMap.toList versions)

-- | Save the version info of a package in the database
persistVersion :: Connection -> Name -> Version -> VersionInfo -> IO ()
persistVersion (Connection conn) packageName version versionInfo =
    catchJust noPackageWithThatName
        insertVersion
        (\_ -> insertPackageName >> insertVersion)
    where
        insertVersion = SQL.execute conn
            ("INSERT OR REPLACE INTO " <> db_PACKAGE_VERSIONS <> " VALUES (?,?,?,?,?,?)")
            (SQLPackageVersion (packageName, version, versionInfo))

        insertPackageName = SQL.execute conn
            ("INSERT OR REPLACE INTO " <> db_PACKAGE_NAMES <> " VALUES (?)")
            (SQLPackageName packageName)

        noPackageWithThatName = isConstraintError

        isConstraintError :: SQL.SQLError -> Maybe ()
        isConstraintError err =
            if SQL.sqlError err == SQL.ErrorConstraint
               then Just ()
               else Nothing

newtype SQLPackageName = SQLPackageName Name

instance ToRow SQLPackageName where
    toRow (SQLPackageName (Name name)) = [SQLText name]

instance FromRow SQLPackageName where
    fromRow = (SQLPackageName . Name) <$> SQL.field

newtype SQLPackageVersion = SQLPackageVersion (Name, Version, VersionInfo)

instance ToRow SQLPackageVersion where
    toRow (SQLPackageVersion (name, version, VersionInfo { revision, description , nixpath, date })) =
        [ SQLText $ fromName name
        , SQLText $ fromVersion version
        , SQLText $ fromHash revision
        , nullable $ SQLText <$> description
        , nullable $ SQLText . pack <$> nixpath
        , SQLInteger $ fromInteger $ toModifiedJulianDay date
        ]

nullable :: Maybe SQLData -> SQLData
nullable = fromMaybe SQLNull

instance FromRow SQLPackageVersion where
    fromRow = create
            <$> SQL.field
            <*> SQL.field
            <*> SQL.field
            <*> SQL.field
            <*> SQL.field
            <*> SQL.field
        where
            create :: Text -> Text -> Text -> Maybe Text -> Maybe Text -> Integer -> SQLPackageVersion
            create name version revision description nixpath date =
                SQLPackageVersion
                    ( Name name
                    , Version version
                    , VersionInfo
                        { revision = Hash revision
                        , description = description
                        , nixpath = unpack <$> nixpath
                        , date = ModifiedJulianDay $ fromInteger date
                        }
                    )

