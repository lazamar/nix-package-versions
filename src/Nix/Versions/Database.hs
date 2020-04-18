{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

{-|
Save and retrieving Database types from persistent storage
-}

module Nix.Versions.Database
    ( Connection
    , RevisionState(..)
    , connect
    , disconnect

    -- Write
    , save
    , registerInvalidRevision

    -- Read
    , versions
    , revisions
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
                        <> ", STATE             TEXT        NOT NULL"
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
                        <> ", FOREIGN KEY (REVISION_HASH) REFERENCES " <> db_REVISIONS <> "(COMMIT_HASH)"
                        <> ")"

disconnect :: Connection -> IO ()
disconnect (Connection conn) = SQL.close conn

-------------------------------------------------------------------------------
-- Read

-- | Retrieve all versions available for a package
-- This will be on the order of the tens, or maximum the
-- hundreds, so it is fine to just return all of them
versions :: Connection -> Name -> IO [(Hash, Package)]
versions (Connection conn) (Name name) = do
    results <- SQL.query
        conn
        ("SELECT * FROM " <> db_PACKAGE_VERSIONS <> " WHERE PACKAGE_NAME = ?")
        [unpack name]
    return $ toInfo <$> results
        where
            toInfo (SQLPackageVersion (_, pkg, hash)) = (hash, pkg)

-- | Retrieve all revisions available in the database
-- This will be between one hundred and one thousand.
revisions :: Connection -> IO [(Day, Commit, RevisionState)]
revisions (Connection conn) = do
    results <- SQL.query_ conn ("SELECT * FROM " <> db_REVISIONS)
    return $ toInfo <$> results
        where
            toInfo (SQLRevision day commit state) = (day, commit, state)

-------------------------------------------------------------------------------
-- Write

-- | When there is a problem building the revision this function allows us
-- to record that in the database so that later we don't try to build it again
registerInvalidRevision :: Connection -> Day -> Commit -> IO ()
registerInvalidRevision conn represents commit =
    persistRevision conn represents (Revision commit mempty) InvalidRevision

-- | Save the entire database
save :: Connection -> Day -> Revision -> IO ()
save conn represents revision = do
    persistRevisionWithState Incomplete
    mapConcurrently_ persistPackage (HashMap.toList packages)
    persistRevisionWithState Success
    where
        Revision (Commit hash _) packages = revision

        persistPackage (name, info) =
            persistVersion conn hash name info

        persistRevisionWithState =
            persistRevision conn represents revision


persistRevision :: Connection -> Day -> Revision -> RevisionState -> IO ()
persistRevision (Connection conn) represents (Revision commit _) state =
    SQL.execute conn
            ("INSERT OR REPLACE INTO " <> db_REVISIONS <> " VALUES (?,?,?,?)")
            (SQLRevision represents commit state)

-- | Save the version info of a package in the database
persistVersion :: Connection -> Hash -> Name -> Package -> IO ()
persistVersion (Connection conn) hash name info =
    catchJust noPackageWithThatName
        insertVersion
        (\_ -> insertPackageName >> insertVersion)
    where
        insertVersion = SQL.execute conn
            ("INSERT OR REPLACE INTO " <> db_PACKAGE_VERSIONS <> " VALUES (?,?,?,?,?)")
            (SQLPackageVersion (name, info, hash))

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
data RevisionState
    = Success            -- ^ All revision packages were successfully added to the DB
    | Incomplete         -- ^ The process of adding packages to the DB was started but not finished
    | InvalidRevision    -- ^ This revision cannot be built. It is not worth trying again.
    deriving (Show, Eq, Enum, Read)

data SQLRevision = SQLRevision Day Commit RevisionState

instance FromRow SQLRevision where
    fromRow = (\hash date represents state -> SQLRevision represents (Commit hash date) state)
        <$> (SQL.field <&> Hash)
        <*> (SQL.field <&> ModifiedJulianDay . fromInteger)
        <*> (SQL.field <&> ModifiedJulianDay . fromInteger)
        <*> (SQL.field <&> read)
        where
            toBool :: Int64 -> Bool
            toBool = \case
                0 -> False
                _ -> True

instance ToRow SQLRevision where
    toRow (SQLRevision represents (Commit hash date) state) =
        [ SQLText    $ fromHash hash                   -- ^ COMMIT_HASH
        , SQLInteger $ dayToInt date                   -- ^ COMMIT_DAY
        , SQLInteger $ dayToInt represents             -- ^ REPRESENTS_DAY
        , SQLText    $ pack $ show state               -- ^ STATE
        ]


newtype SQLPackageName = SQLPackageName Name

instance ToRow SQLPackageName where
    toRow (SQLPackageName (Name name)) = [SQLText name]

instance FromRow SQLPackageName where
    fromRow = (SQLPackageName . Name) <$> SQL.field

newtype SQLPackageVersion = SQLPackageVersion (Name, Package, Hash)

instance ToRow SQLPackageVersion where
    toRow (SQLPackageVersion (name, Package { description, nixpkgsPath, version }, hash)) =
        [ SQLText $ fromName name                   -- ^ PACKAGE_NAME
        , SQLText $ fromVersion version             -- ^ VERSION_NAME
        , SQLText $ fromHash hash                   -- ^ REVISION_HASH
        , nullable $ SQLText <$> description        -- ^ DESCRIPTION
        , nullable $ SQLText . pack <$> nixpkgsPath -- ^ NIXPATH
        ]

nullable :: Maybe SQLData -> SQLData
nullable = fromMaybe SQLNull

instance FromRow SQLPackageVersion where
    fromRow = create
            <$> (SQL.field <&> Name)
            <*> (SQL.field <&> Version)
            <*> (SQL.field <&> Hash)
            <*> (SQL.field)
            <*> (SQL.field)
        where
            create :: Name -> Version -> Hash -> Maybe Text -> Maybe Text -> SQLPackageVersion
            create name version revision description nixpath =
                SQLPackageVersion
                    ( name
                    , Package
                        { version = version
                        , description = description
                        , nixpkgsPath = unpack <$> nixpath
                        }
                    , revision
                    )

dayToInt :: Day -> Int64
dayToInt = fromInteger . toModifiedJulianDay
