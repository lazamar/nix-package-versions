{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Save and retrieving Database types from persistent storage
-}

module Nix.Versions.Database
    ( Connection
    , RevisionState(..)
    , connect
    , disconnect
    , withConnection

    -- Write
    , saveRevisionWithPackages
    , saveRevision

    -- Read
    , versions
    , revisions
    , revisionState
    ) where

import Control.Monad.Conc.Class (MonadConc)
import Control.Concurrent.Classy.Async (mapConcurrently_)
import Control.Exception (assert)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless)
import Data.Maybe (listToMaybe)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.String (fromString, IsString)
import Data.Text (Text, pack, unpack)
import Data.Time.Calendar (Day(..))
import Database.SQLite.Simple (ToRow(toRow), FromRow(fromRow), SQLData(..), NamedParam((:=)))
import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))
import Nix.Revision (Channel, Revision(..), RevisionPackages, Package(..))
import Nix.Versions.Types (CachePath(..), DBFile(..), Hash(..), Version(..), Name(..), Commit(..))

import qualified Data.HashMap.Strict as HashMap
import qualified Database.SQLite.Simple as SQL

newtype Connection = Connection SQL.Connection

-- Constants

db_REVISION_NEW , db_PACKAGE_NEW :: IsString a => a
db_REVISION_NEW = "revision"
db_PACKAGE_NEW = "package"

-- | Get a connection and prepare database for usage
connect :: MonadIO m => CachePath -> DBFile -> m Connection
connect (CachePath dir) (DBFile fname) = liftIO $ do
    conn <- SQL.open $ dir <> "/" <> fname
    -- Enable foreign key constraints.
    -- It's really weird that they would otherwise just not work.
    SQL.execute_ conn "PRAGMA foreign_keys = ON"
    ensureTablesAreCreated conn
    return $ Connection conn

ensureTablesAreCreated :: SQL.Connection -> IO ()
ensureTablesAreCreated conn = do
    SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS " <> db_REVISION_NEW <> " "
                        <> "( COMMIT_HASH       TEXT NOT NULL"
                        <> ", COMMIT_DATE       TEXT NOT NULL"
                        <> ", CHANNEL           TEXT NOT NULL"
                        <> ", REPRESENTS_DATE   TEXT NOT NULL"
                        <> ", STATE             TEXT NOT NULL"
                        <> ", PRIMARY KEY (CHANNEL, COMMIT_HASH, REPRESENTS_DATE)"
                        <> ")"

    SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS " <> db_PACKAGE_NEW <> " "
                        <> "( NAME              TEXT NOT NULL"
                        <> ", VERSION           TEXT NOT NULL"
                        <> ", CHANNEL           TEXT NOT NULL"
                        <> ", COMMIT_HASH       TEXT NOT NULL"
                        <> ", DESCRIPTION       TEXT"
                        <> ", NIXPATH           TEXT"
                        <> ", REPRESENTS_DATE   TEXT NOT NULL"
                        <> ", PRIMARY KEY (NAME, VERSION, CHANNEL)"
                        <> ", FOREIGN KEY (CHANNEL, COMMIT_HASH, REPRESENTS_DATE) REFERENCES " <> db_REVISION_NEW <> " (CHANNEL, COMMIT_HASH, REPRESENTS_DATE)"
                        <> ")"

disconnect :: MonadIO m => Connection -> m ()
disconnect (Connection conn) = liftIO $ SQL.close conn

withConnection :: (MonadMask m, MonadIO m) => CachePath -> DBFile -> (Connection -> m a) -> m a
withConnection cache file = bracket (connect cache file) disconnect

-------------------------------------------------------------------------------
-- Read

-- | Retrieve all versions available for a package
-- This will be on the order of the tens, or maximum the
-- hundreds, so it is fine to just return all of them
versions :: MonadIO m => Connection -> Channel -> Name -> m [(Package, Hash, Day)]
versions (Connection conn) channel name = liftIO $ do
    results <- SQL.queryNamed
        conn
        (fromString $ unwords
            [ "SELECT *"
            , "FROM"
            , db_PACKAGE_NEW
            , "WHERE CHANNEL = :channel AND NAME = :name"
            ]
        )
        [ ":name"    := name
        , ":channel" := channel
        ]
    return $ toVersionInfo <$> results
        where
            toVersionInfo (SQLPackage _ pkg _ hash represents) = (pkg, hash, represents)

-- | Retrieve all revisions available in the database
-- This will be between one hundred and one thousand.
revisions :: MonadIO m => Connection -> Channel -> m [(Day, Revision, RevisionState)]
revisions (Connection conn) channel = liftIO $ do
    results <- SQL.query
        conn
        ("SELECT * FROM " <> db_REVISION_NEW <> " WHERE CHANNEL = ?")
        [channel]
    return $ fromSQLRevision <$> results

-- | Answers the question "did we insert all packages contained in this commit?"
-- It requires a channel because a commit may be used in multiple channels and the
-- outcome in packages may be different
revisionState :: MonadIO m => Connection -> Revision -> m (Maybe RevisionState)
revisionState (Connection conn) (Revision channel (Commit (Hash hash) _)) = do
    commits <- liftIO $ SQL.queryNamed
        conn
        ("SELECT * FROM " <> db_REVISION_NEW <> " WHERE HASH = :hash AND CHANNEl = :channel")
        [ ":hash"    := hash
        , ":channel" := channel
        ]
    return $ toState <$> listToMaybe commits
    where
        toState (SQLRevision _ _ state) = state


fromSQLRevision :: SQLRevision -> (Day, Revision, RevisionState)
fromSQLRevision (SQLRevision day revision state) = (day, revision, state)

-------------------------------------------------------------------------------
-- Write

-- | Save the entire database
saveRevisionWithPackages :: (MonadConc m, MonadIO m) => Connection -> Day -> Revision -> RevisionPackages -> m ()
saveRevisionWithPackages conn represents revision packages = do
    saveRevision conn represents revision Incomplete
    mapConcurrently_ persistPackage (HashMap.toList packages)
    saveRevision conn represents revision Success
    where
        Revision _ (Commit hash _) = revision

        persistPackage (name, package) =
            saveVersion conn name package revision represents

-- | When there is a problem building the revision this function allows us
-- to record that in the database so that later we don't try to build it again
saveRevision :: MonadIO m => Connection -> Day -> Revision -> RevisionState -> m ()
saveRevision (Connection conn) represents revision state =
    liftIO $ SQL.execute conn
            ("INSERT OR REPLACE INTO " <> db_REVISION_NEW <> " VALUES (?,?,?,?,?)")
            (SQLRevision represents revision state)

-- | Save the version info of a package in the database
saveVersion :: MonadIO m => Connection -> Name -> Package -> Revision -> Day -> m ()
saveVersion (Connection conn) name pkg@Package{version} (Revision channel (Commit hash _)) represents =
    liftIO $ SQL.withTransaction conn $ do
        pkgs <- SQL.queryNamed
            conn
            ("SELECT * FROM " <> db_PACKAGE_NEW <> " WHERE NAME = :name AND VERSION = :version AND CHANNEL = :channel")
            [ ":name"    := name
            , ":channel" := channel
            , ":version" := version
            ]

        let newerRevisionAlreadyInDatabase =
                case listToMaybe pkgs of
                    Nothing -> False
                    Just (SQLPackage _ _ _ _ repr) -> repr >= represents

        unless newerRevisionAlreadyInDatabase $
            SQL.execute conn
                ("INSERT OR REPLACE INTO " <> db_PACKAGE_NEW <> " VALUES (?,?,?,?,?,?,?)")
                (SQLPackage name pkg channel hash represents)

-- | Whether all revision entries were added to the table.
data RevisionState
    = Success            -- ^ All revision packages were successfully added to the DB
    | Incomplete         -- ^ The process of adding packages to the DB was started but not finished
    | PreDownload        -- ^ We are still to start the download and handling of this state
    | InvalidRevision    -- ^ This revision cannot be built. It is not worth trying again.
    deriving (Show, Eq, Enum, Read)


-- | One row from db_REVISION_NEW
data SQLRevision = SQLRevision Day Revision RevisionState

instance FromRow SQLRevision where
    fromRow = construct
        <$> SQL.field
        <*> SQL.field
        <*> SQL.field
        <*> SQL.field
        <*> SQL.field
        where
            construct :: Hash -> Day -> Channel -> Day -> RevisionState -> SQLRevision
            construct hash date channel represents state =
                SQLRevision represents (Revision channel (Commit hash date)) state

instance ToRow SQLRevision where
    toRow (SQLRevision represents (Revision channel (Commit hash date)) state) =
        [ toField hash         -- ^ COMMIT_HASH
        , toField date         -- ^ COMMIT_DATE
        , toField channel      -- ^ CHANNEL
        , toField represents   -- ^ REPRESENTS_DATE
        , toField state        -- ^ STATE
        ]

-- | One row from db_PACKAGE_NEW
data SQLPackage = SQLPackage Name Package Channel Hash Day
    deriving (Show, Eq)

instance ToRow SQLPackage   where
    toRow (SQLPackage name (Package description version nixpkgsPath) channel hash represents) =
        [ toField name          -- NAME
        , toField version       -- VERSION
        , toField channel       -- CHANNEL
        , toField hash          -- COMMIT_HASH
        , nullable description  -- DESCRIPTION
        , nullable nixpkgsPath  -- NIXPATH
        , toField represents    -- REPRESENTS_DATE
        ]

instance FromRow SQLPackage where
    fromRow = create
        <$> SQL.field
        <*> SQL.field
        <*> SQL.field
        <*> SQL.field
        <*> SQL.field
        <*> SQL.field
        <*> SQL.field
        where
            create name version channel hash description nixpkgsPath represents =
                SQLPackage
                    name
                    (Package description version nixpkgsPath)
                    channel
                    hash
                    represents

instance ToField RevisionState where
    toField =  SQLText . pack . show

instance FromField RevisionState where
    fromField = fmap read  . fromField

instance ToField Name where
    toField = SQLText . fromName

instance FromField Name where
    fromField = fmap Name . fromField

instance ToField Hash where
    toField = SQLText . fromHash

instance FromField Hash where
    fromField = fmap Hash . fromField

instance ToField Channel where
    toField = SQLText . pack . show

instance FromField Channel where
    fromField = fmap read . fromField

instance ToField Version where
    toField = SQLText . fromVersion

instance FromField Version where
    fromField = fmap Version . fromField

nullable :: ToField a => Maybe a -> SQLData
nullable = fromMaybe SQLNull . fmap toField

