{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

{-|
Save and retrieving Database types from persistent storage
-}

module Nix.Versions.Database
    ( connect
    , disconnect
    , versions
    , save
    , Connection
    ) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (catchJust)
import Data.Int (Int64)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day(..), toModifiedJulianDay)
import Database.SQLite.Simple (ToRow(toRow), FromRow(fromRow), SQLData(..))
import Nix.Revision (Revision(..), Package(..))
import Nix.Versions.Types (CachePath(..), DBFile(..), Hash(..), Version(..), Name(..), Commit(..))

import qualified Data.HashMap.Strict as HashMap
import qualified Database.SQLite.Simple as SQL

newtype Connection = Connection SQL.Connection

-- Constants

db_PACKAGE_NAMES    = "PACKAGE_NAMES"
db_PACKAGE_VERSIONS = "PACKAGE_VERSIONS"
db_REVISIONS        = "REVISIONS"

-- | Get a connection and prepare database for usage
connect :: CachePath -> DBFile -> IO Connection
connect (CachePath dir) (DBFile fname) = do
    conn <- SQL.open $ dir <> "/" <> fname
    -- Enable foreign key constraints.
    -- It's really weird that they would otherwise just not work.
    SQL.execute_ conn "PRAGMA foreign_keys = ON"
    ensureTablesAreCreated conn
    return $ Connection conn

ensureTablesAreCreated :: SQL.Connection -> IO ()
ensureTablesAreCreated conn = do
    SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS  " <> db_REVISIONS <> " "
                        -- | Details about the Revision's commit
                        <> "( COMMIT_HASH       TEXT        NOT NULL PRIMARY KEY"
                        <> ", COMMIT_DAY        INTEGER     NOT NULL"
                        -- | Even though the commit might have been done in a certain date,
                        -- we added it to the database to represent the state of nixpkgs
                        -- on a specific date, which may be different from the exact commit date.
                        <> ", REPRESENTS_DAY    INTEGER     NOT NULL"
                        -- | Whether we were able to successfully add all Revision packages to the table
                        <> ", SUCCESS           BOOLEAN     NOT NULL CHECK (SUCCESS IN (0,1))"
                        <> ")"

    SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS  " <> db_PACKAGE_NAMES <> " "
                        <> "( PACKAGE_NAME TEXT PRIMARY KEY"
                        <> ")"

    SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS  " <> db_PACKAGE_VERSIONS <> " "
                        <> "( PACKAGE_NAME  TEXT NOT NULL"
                        <> ", VERSION_NAME  TEXT NOT NULL"
                        <> ", REVISION_HASH TEXT NOT NULL"
                        <> ", DESCRIPTION   TEXT"
                        <> ", NIXPATH       TEXT"
                        <> ", PRIMARY KEY (PACKAGE_NAME, VERSION_NAME)"
                        <> ", FOREIGN KEY (PACKAGE_NAME) REFERENCES " <> db_PACKAGE_NAMES <> "(PACKAGE_NAME)"
                        <> ", FOREIGN KEY (REVISION_HASH) REFERENCES " <> db_REVISIONS <> "(HASH)"
                        <> ")"

disconnect :: Connection -> IO ()
disconnect (Connection conn) = SQL.close conn

-- | Retrieve all versions available for a package
versions :: Connection -> Name -> IO [(Commit, Package)]
versions (Connection conn) (Name name) = do
    results <- SQL.query
        conn
        ("SELECT * FROM " <> db_PACKAGE_VERSIONS <> " WHERE PACKAGE_NAME = ?")
        [unpack name]
    return $ toInfo <$> results
        where
            toInfo (SQLPackageVersion (_, pkg, commit)) = (commit, pkg)

-- | Save the entire database
save :: Connection -> Revision -> IO ()
save conn (Revision commit packages) = do
    mapConcurrently_ persistPackage (HashMap.toList packages)
    where
        persistPackage (name, info) =
            persistVersion conn commit name info


-- | Save the version info of a package in the database
persistVersion :: Connection -> Commit -> Name -> Package -> IO ()
persistVersion (Connection conn) commit name info =
    catchJust noPackageWithThatName
        insertVersion
        (\_ -> insertPackageName >> insertVersion)
    where
        insertVersion = SQL.execute conn
            ("INSERT OR REPLACE INTO " <> db_PACKAGE_VERSIONS <> " VALUES (?,?,?,?,?,?)")
            (SQLPackageVersion (name, info, commit))

        insertPackageName = SQL.execute conn
            ("INSERT OR REPLACE INTO " <> db_PACKAGE_NAMES <> " VALUES (?)")
            (SQLPackageName name)

        noPackageWithThatName = isConstraintError

        isConstraintError :: SQL.SQLError -> Maybe ()
        isConstraintError err =
            if SQL.sqlError err == SQL.ErrorConstraint
               then Just ()
               else Nothing


-- | Whether all revision entries were added to the table.
newtype Success = Success Bool
    deriving (Show, Eq)
    deriving newtype (Enum)

data SQLRevision = SQLRevision Day Commit Success

instance FromRow SQLRevision where
    fromRow = (\hash date represents success -> SQLRevision represents (Commit hash date) success)
        <$> (SQL.field <&> Hash)
        <*> (SQL.field <&> ModifiedJulianDay . fromInteger)
        <*> (SQL.field <&> ModifiedJulianDay . fromInteger)
        <*> (SQL.field <&> Success . toBool)
        where
            toBool :: Int64 -> Bool
            toBool = \case
                0 -> False
                _ -> True

instance ToRow SQLRevision where
    toRow (SQLRevision represents (Commit hash date) success) =
        [ SQLText    $ fromHash hash                   -- ^ COMMIT_HASH
        , SQLInteger $ dayToInt date                   -- ^ COMMIT_DAY
        , SQLInteger $ dayToInt represents             -- ^ REPRESENTS_DAY
        , SQLInteger $ fromIntegral $ fromEnum success -- ^ SUCCESS
        ]


newtype SQLPackageName = SQLPackageName Name

instance ToRow SQLPackageName where
    toRow (SQLPackageName (Name name)) = [SQLText name]

instance FromRow SQLPackageName where
    fromRow = (SQLPackageName . Name) <$> SQL.field

newtype SQLPackageVersion = SQLPackageVersion (Name, Package, Commit)

instance ToRow SQLPackageVersion where
    toRow (SQLPackageVersion (name, Package { description, nixpkgsPath, version }, Commit hash date)) =
        [ SQLText $ fromName name
        , SQLText $ fromVersion version
        , SQLText $ fromHash hash
        , nullable $ SQLText <$> description
        , nullable $ SQLText . pack <$> nixpkgsPath
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
                    , Package
                        { version = Version version
                        , description = description
                        , nixpkgsPath = unpack <$> nixpath
                        }
                    , Commit (Hash revision) (ModifiedJulianDay $ fromInteger date)
                    )

dayToInt :: Day -> Int64
dayToInt = fromInteger . toModifiedJulianDay
