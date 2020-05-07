{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
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
    , removeRevisionsAndPackagesFrom

    -- Read
    , versions
    , revisions
    , revisionState
    ) where

import Control.Monad.Conc.Class (MonadConc)
import Control.Concurrent.Classy.Async (mapConcurrently_, mapConcurrently)
import Control.Monad.Catch (MonadMask, bracket, mask, onException)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import Nix.Versions.Types (CachePath(..), DBFile(..), Hash(..), Version(..), Name(..), Commit(..))

import qualified Database.SQLite.Simple as SQL
import qualified Data.HashMap.Strict as HashMap

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
    -- Make sure the correct transaction tracking option is in place
    SQL.execute_ conn "PRAGMA journal_mode = WAL"
    SQL.execute_ conn "PRAGMA synchronous = NORMAL"
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

    -- This one line caused a 100x speedup in package search
    SQL.execute_ conn $ "CREATE INDEX IF NOT EXISTS NAME_NOCASE_INDEX on " <> db_PACKAGE_NEW <> " (NAME COLLATE NOCASE)"

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
            , "WHERE CHANNEL = :channel AND NAME = :name COLLATE NOCASE"
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
        ("SELECT * FROM " <> db_REVISION_NEW <> " WHERE COMMIT_HASH = :hash AND CHANNEl = :channel")
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

withTransaction :: (MonadMask m, MonadIO m) => Connection -> m a -> m a
withTransaction (Connection conn) action = do
    mask $ \restore -> do
        begin
        r <- restore action `onException` rollback
        commit
        return r
    where
        begin    = liftIO $ SQL.execute_ conn "BEGIN TRANSACTION"
        commit   = liftIO $ SQL.execute_ conn "COMMIT TRANSACTION"
        rollback = liftIO $ SQL.execute_ conn "ROLLBACK TRANSACTION"


-- | Save the entire database
saveRevisionWithPackages :: (MonadConc m, MonadIO m) => Connection -> Day -> Revision -> RevisionPackages -> m ()
saveRevisionWithPackages conn represents revision packages = do
    liftIO $ SQL.execute_ connection "BEGIN TRANSACTION"
    saveRevision conn represents revision Incomplete
    saveVersions conn revision represents $ HashMap.elems packages
    saveRevision conn represents revision Success
    liftIO $ SQL.execute_ connection "COMMIT TRANSACTION"
    where
        Connection connection = conn

-- | When there is a problem building the revision this function allows us
-- to record that in the database so that later we don't try to build it again
saveRevision :: MonadIO m => Connection -> Day -> Revision -> RevisionState -> m ()
saveRevision (Connection conn) represents revision state =
    liftIO $ SQL.execute conn
            ("INSERT OR REPLACE INTO " <> db_REVISION_NEW <> " VALUES (?,?,?,?,?)")
            (SQLRevision represents revision state)

-- | Save the version info of a package in the database
saveVersions :: (MonadConc m, MonadIO m) => Connection -> Revision -> Day -> [Package] -> m ()
saveVersions (Connection conn) (Revision channel (Commit hash _)) represents packages  = do
    -- This should ideally happen inside of a transaction, but that causes more trouble
    -- than it is woth. It causes errors with multi-threaded inserts, as it says that
    -- we are trying to start a transaction inside another. It is unlikely that the same
    -- package version for the same channel would be added in parallel, as we currently parallelise
    -- version fetching by channel and even if it did happen the damage is minimal.
    toInsert <-
        liftIO
        $ fmap (map fst . filter snd)
        $ mapConcurrently (\p -> (p,) <$> needsInserting p) packages

    liftIO $ void $ traverse insert toInsert
    where
        needsInserting (Package{name, version}) = do
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
                        Just (SQLPackage _ _ _ repr) -> repr >= represents

            return $ not newerRevisionAlreadyInDatabase

        insert pkg =
            SQL.execute conn
                ("INSERT OR REPLACE INTO " <> db_PACKAGE_NEW <> " VALUES (?,?,?,?,?,?,?)")
                (SQLPackage pkg channel hash represents)

removeRevisionsAndPackagesFrom :: MonadIO m => Connection -> Commit -> m ()
removeRevisionsAndPackagesFrom  (Connection conn) (Commit hash _) = liftIO $ do
    SQL.execute conn ("DELETE FROM " <> db_PACKAGE_NEW <> " WHERE COMMIT_HASH = ?") [hash]
    SQL.execute conn ("DELETE FROM " <> db_REVISION_NEW <> " WHERE COMMIT_HASH = ?") [hash]

-- | Whether all revision entries were added to the table.
-- Order is important. Success is the max value
data RevisionState
    = PreDownload        -- ^ We are still to start the download and handling of this state
    | Incomplete         -- ^ The process of adding packages to the DB was started but not finished
    | InvalidRevision    -- ^ This revision cannot be built. It is not worth trying again.
    | Success            -- ^ All revision packages were successfully added to the DB
    deriving (Show, Eq, Enum, Read, Ord)


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
    toRow (SQLPackage (Package name description version nixpkgsPath) channel hash represents) =
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
                    (Package name description version nixpkgsPath)
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

