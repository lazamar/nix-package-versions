{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-| SQLite implementation of the Storage class
-}

module App.Storage.SQLite (withOpenDatabase) where

import Control.Monad.Conc.Class (MonadConc)
import Control.Concurrent.Classy.Async (mapConcurrently)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.Maybe (fromMaybe)
import Data.String (fromString, IsString)
import Data.Text (pack)
import Data.Time.Calendar (Day(..))
import Database.SQLite.Simple (ToRow(toRow), FromRow(fromRow), SQLData(..), NamedParam((:=)))
import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))

import Nix.Revision (Channel, Revision(..), RevisionPackages, Package(..))
import Nix.Versions.Types (CachePath(..), DBFile(..), Hash(..), Version(..), FullName(..), KeyName(..), Name(..), Commit(..))

import App.Storage (Storage, Database(..), RevisionState(..))
import qualified App.Storage as Storage

import qualified Database.SQLite.Simple as SQL

import Control.Monad.SQL

newtype SQLiteDatabase = SQLiteDatabase Connection

withOpenDatabase :: CachePath -> DBFile -> (Database -> IO a) -> IO a
withOpenDatabase (CachePath dir) (DBFile fname) act =
  connect path $ \conn -> do
    runSQL conn initialise
    act $ Database $ SQLiteDatabase conn
  where
    path = dir <> "/" <> fname

instance Storage SQLiteDatabase where
  versions (SQLiteDatabase conn) channel name =
    runSQL conn $ versions channel name
  revisions (SQLiteDatabase conn) channel =
    runSQL conn $ revisions channel
  writePackages (SQLiteDatabase conn) represents revision packages =
    runSQL conn $ saveRevisionWithPackages represents revision packages
  writeRevisionState (SQLiteDatabase conn) date revision state =
    runSQL conn $ saveRevision date revision state

-- Constants

db_REVISION_NEW , db_PACKAGE_NEW :: IsString a => a
db_REVISION_NEW = "revision"
db_PACKAGE_NEW = "package"

initialise :: (MonadConc m, MonadMask m, MonadIO m) => MonadSQLT m ()
initialise = do
  -- Prepare database for usage
  -- Enable foreign key constraints. It's really weird that they would otherwise just not work.
  execute_ "PRAGMA foreign_keys = ON"
  -- Make sure the correct transaction tracking option is in place
  execute_ "PRAGMA journal_mode = WAL"
  execute_ "PRAGMA synchronous = NORMAL"
  ensureTablesAreCreated

ensureTablesAreCreated :: MonadSQL m => m ()
ensureTablesAreCreated = do
    execute_ $ "CREATE TABLE IF NOT EXISTS " <> db_REVISION_NEW <> " "
                        <> "( COMMIT_HASH       TEXT NOT NULL"
                        <> ", COMMIT_DATE       TEXT NOT NULL"
                        <> ", CHANNEL           TEXT NOT NULL"
                        <> ", REPRESENTS_DATE   TEXT NOT NULL"
                        <> ", STATE             TEXT NOT NULL"
                        <> ", PRIMARY KEY (CHANNEL, COMMIT_HASH, REPRESENTS_DATE)"
                        <> ")"

    execute_ $ "CREATE TABLE IF NOT EXISTS " <> db_PACKAGE_NEW <> " "
                        <> "( NAME              TEXT NOT NULL"
                        <> ", VERSION           TEXT NOT NULL"
                        <> ", KEY_NAME          TEXT NOT NULL"
                        <> ", FULL_NAME         TEXT NOT NULL"
                        <> ", CHANNEL           TEXT NOT NULL"
                        <> ", COMMIT_HASH       TEXT NOT NULL"
                        <> ", DESCRIPTION       TEXT"
                        <> ", REPRESENTS_DATE   TEXT NOT NULL"
                        <> ", PRIMARY KEY (NAME, VERSION, CHANNEL)"
                        <> ", FOREIGN KEY (CHANNEL, COMMIT_HASH, REPRESENTS_DATE) REFERENCES " <> db_REVISION_NEW <> " (CHANNEL, COMMIT_HASH, REPRESENTS_DATE)"
                        <> ")"

    -- This one line caused a 100x speedup in package search
    execute_ $ "CREATE INDEX IF NOT EXISTS NAME_NOCASE_INDEX on " <> db_PACKAGE_NEW <> " (NAME COLLATE NOCASE)"


-------------------------------------------------------------------------------
-- Read

-- | Retrieve all versions available for a package
-- This will be on the order of the tens, or maximum the
-- hundreds, so it is fine to just return all of them
versions :: MonadSQL m => Channel -> Name -> m [(Package, Hash, Day)]
versions channel name = do
    results <- queryNamed
        (fromString $ unwords
            [ "SELECT *"
            , "FROM"
            , db_PACKAGE_NEW
            , "WHERE CHANNEL = :channel AND NAME = :name COLLATE NOCASE"
            , "ORDER BY REPRESENTS_DATE"
            ]
        )
        [ ":name"    := name
        , ":channel" := channel
        ]
    return $ toVersionInfo <$> results
        where
            toVersionInfo (SQLPackage pkg _ hash represents) = (pkg, hash, represents)

-- | Retrieve all revisions available in the database
-- This will be between one hundred and one thousand.
revisions :: MonadSQL m => Channel -> m [(Day, Revision, RevisionState)]
revisions channel = do
    results <- query ("SELECT * FROM " <> db_REVISION_NEW <> " WHERE CHANNEL = ?") [channel]
    return $ fromSQLRevision <$> results

fromSQLRevision :: SQLRevision -> (Day, Revision, RevisionState)
fromSQLRevision (SQLRevision day revision state) = (day, revision, state)

-------------------------------------------------------------------------------
-- Write


-- | Save the entire database
saveRevisionWithPackages
    :: (MonadIO m, MonadConc m, MonadSQL m)
    => Day -> Revision -> RevisionPackages -> m ()
saveRevisionWithPackages represents revision packages =
    withTransaction $ do
        saveRevision represents revision Incomplete
        saveVersions revision represents packages
        saveRevision represents revision Success

-- | When there is a problem building the revision this function allows us
-- to record that in the database so that later we don't try to build it again
saveRevision :: MonadSQL m => Day -> Revision -> RevisionState -> m ()
saveRevision represents revision state =
    execute
        ("INSERT OR REPLACE INTO " <> db_REVISION_NEW <> " VALUES (?,?,?,?,?)")
        (SQLRevision represents revision state)

-- | Save the version info of a package in the database
saveVersions :: (MonadConc m, MonadSQL m) => Revision -> Day -> [Package] -> m ()
saveVersions (Revision channel (Commit hash _)) represents packages  = do
    -- This should ideally happen inside of a transaction, but that causes more trouble
    -- than it is woth. It causes errors with multi-threaded inserts, as it says that
    -- we are trying to start a transaction inside another. It is unlikely that the same
    -- package version for the same channel would be added in parallel, as we currently parallelise
    -- version fetching by channel and even if it did happen the damage is minimal.
    toInsert <- (map fst . filter snd) <$> mapConcurrently (\p -> (p,) <$> needsInserting p) packages
    void $ traverse insert toInsert
    where
        needsInserting (Package{name, version}) = do
            pkgs <- queryNamed
                ("SELECT * FROM " <> db_PACKAGE_NEW <> " WHERE NAME = :name AND VERSION = :version AND CHANNEL = :channel")
                [ ":name"    := name
                , ":channel" := channel
                , ":version" := version
                ]
            let newerRevisionAlreadyInDatabase =
                    case listToMaybe pkgs of
                        Nothing -> False
                        Just (SQLPackage _ _ _ repr) -> repr >= represents
            return $ not newerRevisionAlreadyInDatabase

        insert pkg = execute
            ("INSERT OR REPLACE INTO " <> db_PACKAGE_NEW <> " VALUES (?,?,?,?,?,?,?,?)")
            (SQLPackage pkg channel hash represents)

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
data SQLPackage = SQLPackage Package Channel Hash Day
    deriving (Show, Eq)

instance ToRow SQLPackage where
    toRow (SQLPackage (Package name version keyName fullName description) channel hash represents) =
        [ toField name          -- NAME
        , toField version       -- VERSION
        , toField keyName       -- KEY_NAME
        , toField fullName      -- FULL_NAME
        , toField channel       -- CHANNEL
        , toField hash          -- COMMIT_HASH
        , nullable description  -- DESCRIPTION
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
        <*> SQL.field
        where
            create name version keyName fullName channel hash description represents =
                SQLPackage
                    (Package name version keyName fullName description)
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

instance ToField KeyName where
    toField = SQLText . fromKeyName

instance FromField KeyName where
    fromField = fmap KeyName . fromField

instance ToField FullName where
    toField = SQLText . fromFullName

instance FromField FullName where
    fromField = fmap FullName . fromField

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

