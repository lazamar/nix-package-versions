{-# OPTIONS_GHC -Wno-orphans #-}

{-| SQLite implementation of the Storage class
-}

module App.Storage.SQLite (withDatabase) where

import Control.Monad.Conc.Class (MonadConc)
import Control.Concurrent.Classy.Async (mapConcurrently)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (void)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Maybe (fromMaybe)
import Data.String (fromString, IsString)
import Data.Text (pack)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock.POSIX (POSIXTime)
import Database.SQLite.Simple (ToRow(toRow), FromRow(fromRow), SQLData(..), NamedParam((:=)))
import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))

import Nix
  ( Version(..)
  , PackageWithVersion(..)
  , KeyName(..)
  , Package(..)
  , Channel
  , Revision(..)
  , RevisionPackages
  , PackageDetails(..))

import Data.Git (Hash(..), Commit(..))
import Data.Time.Period (Period(..), toDay, fromDay)
import App.Storage (Storage, Database(..), CommitState(..))
import qualified App.Storage as Storage

import qualified Database.SQLite.Simple as SQL

import Control.Monad.SQL (Connection, MonadSQL(..), runSQL, connect)

newtype SQLiteDatabase = SQLiteDatabase Connection

withDatabase :: FilePath -> (Database -> IO a) -> IO a
withDatabase path act =
  connect path $ \conn -> do
    runSQL conn $ do
      -- Prepare database for usage
      -- Enable foreign key constraints. It's really weird that they would otherwise just not work.
      execute_ "PRAGMA foreign_keys = ON"
      -- Make sure the correct transaction tracking option is in place
      execute_ "PRAGMA journal_mode = WAL"
      execute_ "PRAGMA synchronous = NORMAL"
      ensureTablesAreCreated
    act $ Database $ SQLiteDatabase conn

instance Storage SQLiteDatabase where
  versions (SQLiteDatabase conn) channel name =
    runSQL conn $ versions channel name
  revisions (SQLiteDatabase conn) channel =
    runSQL conn $ revisions channel
  writePackages (SQLiteDatabase conn) represents revision packages =
    runSQL conn $ saveRevisionWithPackages represents revision packages
  writeRevisionState (SQLiteDatabase conn) date revision state =
    runSQL conn $ saveRevision date revision state

  versions' (SQLiteDatabase conn) channel package =
    runSQL conn $ versions' channel package

  coverage (SQLiteDatabase conn) channel =
    runSQL conn $ coverage channel

  writeCoverage (SQLiteDatabase conn) period channel commit =
    runSQL conn $ execute
    ("INSERT OR REPLACE INTO " <> db_COVERAGE <> " VALUES (?,?,?,?,?)")
    (SQLCoverage commit channel period)

  writePackage (SQLiteDatabase conn) (Commit hash _) details =
    runSQL conn $ execute
    ("INSERT OR REPLACE INTO "
      <> db_PACKAGE_DETAILS
      <> " VALUES (?,?,?,?,?,?)")
    (SQLPackageDetails details hash)

  writeCommitState (SQLiteDatabase conn) commit state =
    runSQL conn $ execute
    ("INSERT OR REPLACE INTO " <> db_COMMIT_STATES <> " VALUES (?,?,?)")
    (SQLCommitState commit state)

-- Constants

db_REVISION_NEW , db_PACKAGE_NEW :: IsString a => a
db_REVISION_NEW = "revision"
db_PACKAGE_NEW = "package"

db_PACKAGE_DETAILS, db_COVERAGE, db_COMMIT_STATES  :: IsString a => a
db_PACKAGE_DETAILS = "package_details"
db_COVERAGE = "coverage"
db_COMMIT_STATES = "commit_states"

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
    execute_ $
      "CREATE INDEX IF NOT EXISTS NAME_NOCASE_INDEX_2 on "
      <> db_PACKAGE_NEW
      <> " (NAME COLLATE NOCASE)"

    execute_ $ "CREATE TABLE IF NOT EXISTS " <> db_COMMIT_STATES <> " "
      <> "( COMMIT_HASH       TEXT NOT NULL"
      <> ", COMMIT_DATE       INTEGER NOT NULL"
      <> ", INDEXING_STATE    TEXT NOT NULL"
      <> ", PRIMARY KEY COMMIT_HASH"
      <> ")"

    execute_ $ "CREATE TABLE IF NOT EXISTS " <> db_COVERAGE <> " "
      <> "( COMMIT_HASH       TEXT NOT NULL"
      <> ", COMMIT_DATE       INTEGER NOT NULL" -- we keep the date here for convenience
      <> ", CHANNEL           TEXT NOT NULL"
      <> ", PERIOD_START      INTEGER NOT NULL"
      <> ", PERIOD_END        INTEGER NOT NULL"
      <> ", PRIMARY KEY (CHANNEL, PERIOD_START, PERIOD_END)"
      <> ", FOREIGN KEY COMMIT_HASH REFERENCES " <> db_COMMIT_STATES <> " COMMIT_HASH"
      <> ")"

    execute_ $ "CREATE TABLE IF NOT EXISTS " <> db_PACKAGE_DETAILS <> " "
      <> "( NAME              TEXT NOT NULL"
      <> ", VERSION           TEXT NOT NULL"
      <> ", KEY_NAME          TEXT NOT NULL"
      <> ", FULL_NAME         TEXT NOT NULL"
      <> ", DESCRIPTION       TEXT"
      <> ", COMMIT_HASH       TEXT NOT NULL"
      <> ", PRIMARY KEY (COMMIT_HASH, KEY_NAME)"
      <> ", FOREIGN KEY COMMIT_HASH REFERENCES " <> db_COMMIT_STATES <> " COMMIT_HASH"
      <> ")"

    -- This one line caused a 100x speedup in package search
    execute_ $
      "CREATE INDEX IF NOT EXISTS NAME_NOCASE_INDEX on "
      <> db_PACKAGE_DETAILS
      <> " (NAME COLLATE NOCASE)"

-- | One row from db_COMMIT_STATES
data SQLCommitState = SQLCommitState Commit CommitState
  deriving Show

instance FromRow SQLCommitState where
  fromRow = construct
    <$> SQL.field  -- COMMIT_HASH
    <*> SQL.field  -- COMMIT_DATE
    <*> SQL.field  -- INDEXING_STATE
    where
    construct :: Hash -> SQLPOSIX -> CommitState -> SQLCommitState
    construct hash (SQLPOSIX time) state =
      SQLCommitState (Commit hash time) state

instance ToRow SQLCommitState where
  toRow (SQLCommitState (Commit hash time) state) =
    [ toField hash            -- ^ COMMIT_HASH
    , toField (SQLPOSIX time) -- ^ COMMIT_DATE
    , toField state           -- ^ INDEXING_STATE
    ]

-- | One row from db_COVERAGE
data SQLCoverage = SQLCoverage Commit Channel Period
  deriving Show

instance FromRow SQLCoverage where
  fromRow = pure construct
    <*> SQL.field -- COMMIT_HASH
    <*> SQL.field -- COMMIT_DATE
    <*> SQL.field -- CHANNEL
    <*> SQL.field -- PERIOD_START
    <*> SQL.field -- PERIOD_END
    where
    construct
      :: Hash
      -> SQLPOSIX
      -> Channel
      -> SQLPOSIX
      -> SQLPOSIX
      -> SQLCoverage
    construct hash (SQLPOSIX time) channel (SQLPOSIX from) (SQLPOSIX to) =
      SQLCoverage (Commit hash time) channel (Period from to)

instance ToRow SQLCoverage where
  toRow (SQLCoverage (Commit hash time) channel (Period from to)) =
    [ toField hash            -- ^  COMMIT_HASH
    , toField (SQLPOSIX time) -- ^  COMMIT_DATE
    , toField channel         -- ^  CHANNEL
    , toField (SQLPOSIX from) -- ^  PERIOD_START
    , toField (SQLPOSIX to)   -- ^  PERIOD_END
    ]

-- | One row from db_COVERAGE
data SQLPackageDetails = SQLPackageDetails PackageDetails Hash
  deriving Show

instance ToRow SQLPackageDetails where
  toRow (SQLPackageDetails details hash) =
    let (PackageDetails name version keyName fullName description) = details
    in
    [ toField name          -- NAME
    , toField version       -- VERSION
    , toField keyName       -- KEY_NAME
    , toField fullName      -- FULL_NAME
    , nullable description  -- DESCRIPTION
    , toField hash          -- COMMIT_HASH
    ]

instance FromRow SQLPackageDetails where
  fromRow = pure construct
    <*> SQL.field  -- NAME
    <*> SQL.field  -- VERSION
    <*> SQL.field  -- KEY_NAME
    <*> SQL.field  -- FULL_NAME
    <*> SQL.field  -- DESCRIPTION
    <*> SQL.field  -- COMMIT_HASH
    where
    construct name version keyName fullName description hash =
      SQLPackageDetails details hash
      where
        details = PackageDetails name version keyName fullName description

-------------------------------------------------------------------------------
-- Read

versions' :: MonadSQL m => Channel -> Package -> m [(PackageDetails, Commit)]
versions' channel package = do
  results <- queryNamed
    (fromString $ unwords
        [ "SELECT " <> intercalate ","
            [ db_PACKAGE_DETAILS <> ".NAME"
            , db_PACKAGE_DETAILS <> ".VERSION"
            , db_PACKAGE_DETAILS <> ".KEY_NAME"
            , db_PACKAGE_DETAILS <> ".FULL_NAME"
            , db_PACKAGE_DETAILS <> ".DESCRIPTION"
            , db_PACKAGE_DETAILS <> ".COMMIT_HASH"
            , db_COVERAGE        <> ".COMMIT_DATE"
            ]
        , "FROM " <> db_PACKAGE_DETAILS
        , "INNER JOIN " <> db_COVERAGE
          <> " ON "
          <> db_PACKAGE_DETAILS <> ".COMMIT_HASH"
          <> " = "
          <> db_COVERAGE <> ".COMMIT_HASH"
        , "WHERE"
        , "NAME = :name COLLATE NOCASE AND CHANNEL = :channel"
        , "ORDER BY COMMIT_DATE"
        ]
    )
    [ ":name" := package
    , ":channel" := channel
    ]
  return $ parse <$> results
  where
  parse (name, version, keyName, fullName, description, hash, (SQLPOSIX time)) =
    ( PackageDetails name version keyName fullName description
    , Commit hash time
    )

coverage :: MonadSQL m => Channel -> m [(Period, Commit, CommitState)]
coverage channel = do
  results <- queryNamed
    (fromString $ unwords
        [ "SELECT " <> intercalate ","
            [ db_COVERAGE <> ".COMMIT_HASH"
            , db_COVERAGE <> ".COMMIT_DATE"
            , db_COVERAGE <> ".PERIOD_START"
            , db_COVERAGE <> ".PERIOD_END"
            , db_COMMIT_STATES <> ".INDEXING_STATE"
            ]
        , "FROM " <> db_COVERAGE
        , "INNER JOIN " <> db_COMMIT_STATES
          <> " ON "
          <> db_COVERAGE <> ".COMMIT_HASH"
          <> " = "
          <> db_COMMIT_STATES <> ".COMMIT_HASH"
        , "WHERE"
        , "CHANNEL = :channel"
        , "ORDER BY COMMIT_DATE"
        ]
    )
    [ ":channel" := channel ]
  return $ parse <$> results
  where
    parse (hash, SQLPOSIX time, SQLPOSIX start, SQLPOSIX end, state) =
      (Period start end, Commit hash time, state)

-- | Retrieve all versions available for a package
-- This will be on the order of the tens, or maximum the
-- hundreds, so it is fine to just return all of them
versions :: MonadSQL m => Channel -> Package -> m [(PackageDetails, Hash, Day)]
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
revisions :: MonadSQL m => Channel -> m [(Day, Revision, CommitState)]
revisions channel = do
    results <- query ("SELECT * FROM " <> db_REVISION_NEW <> " WHERE CHANNEL = ?") [channel]
    return $ fromSQLRevision <$> results

fromSQLRevision :: SQLRevision -> (Day, Revision, CommitState)
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
saveRevision :: MonadSQL m => Day -> Revision -> CommitState -> m ()
saveRevision represents revision state =
    execute
        ("INSERT OR REPLACE INTO " <> db_REVISION_NEW <> " VALUES (?,?,?,?,?)")
        (SQLRevision represents revision state)

-- | Save the version info of a package in the database
saveVersions :: (MonadConc m, MonadSQL m) => Revision -> Day -> [PackageDetails] -> m ()
saveVersions (Revision channel (Commit hash _)) represents packages  = do
    -- This should ideally happen inside of a transaction, but that causes more trouble
    -- than it is woth. It causes errors with multi-threaded inserts, as it says that
    -- we are trying to start a transaction inside another. It is unlikely that the same
    -- package version for the same channel would be added in parallel, as we currently parallelise
    -- version fetching by channel and even if it did happen the damage is minimal.
    toInsert <- (map fst . filter snd) <$> mapConcurrently (\p -> (p,) <$> needsInserting p) packages
    void $ traverse insert toInsert
    where
        needsInserting (PackageDetails{name, version}) = do
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
data SQLRevision = SQLRevision Day Revision CommitState

instance FromRow SQLRevision where
    fromRow = construct
        <$> SQL.field
        <*> SQL.field
        <*> SQL.field
        <*> SQL.field
        <*> SQL.field
        where
            construct :: Hash -> Day -> Channel -> Day -> CommitState -> SQLRevision
            construct hash date channel represents state =
                SQLRevision represents (Revision channel (Commit hash (fromDay date))) state

instance ToRow SQLRevision where
    toRow (SQLRevision represents (Revision channel (Commit hash date)) state) =
        [ toField hash         -- ^ COMMIT_HASH
        , toField (toDay date) -- ^ COMMIT_DATE
        , toField channel      -- ^ CHANNEL
        , toField represents   -- ^ REPRESENTS_DATE
        , toField state        -- ^ STATE
        ]

-- | One row from db_PACKAGE_NEW
data SQLPackage = SQLPackage PackageDetails Channel Hash Day
    deriving (Show, Eq)

instance ToRow SQLPackage where
    toRow (SQLPackage (PackageDetails name version keyName fullName description) channel hash represents) =
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
                    (PackageDetails name version keyName fullName description)
                    channel
                    hash
                    represents

instance ToField CommitState where
    toField =  SQLText . pack . show

instance FromField CommitState where
    fromField = fmap read  . fromField

instance ToField Package where
    toField = SQLText . unPackage

instance FromField Package where
    fromField = fmap Package . fromField

instance ToField KeyName where
    toField = SQLText . fromKeyName

instance FromField KeyName where
    fromField = fmap KeyName . fromField

instance ToField PackageWithVersion where
    toField = SQLText . unPackageWithVersion

instance FromField PackageWithVersion where
    fromField = fmap PackageWithVersion . fromField

instance ToField Hash where
    toField = SQLText . fromHash

instance FromField Hash where
    fromField = fmap Hash . fromField

instance ToField Channel where
    toField = SQLText . pack . show

instance FromField Channel where
    fromField = fmap read . fromField

instance ToField Version where
    toField = SQLText . unVersion

instance FromField Version where
    fromField = fmap Version . fromField

-- Stores POSIXTime ignoring milliseconds.
newtype SQLPOSIX = SQLPOSIX POSIXTime
  deriving Show

instance FromField SQLPOSIX where
    fromField = fmap construct . fromField
      where
      construct :: Int -> SQLPOSIX
      construct = SQLPOSIX . realToFrac

instance ToField SQLPOSIX where
    toField (SQLPOSIX n) = SQLInteger $ ceiling n
nullable :: ToField a => Maybe a -> SQLData
nullable = fromMaybe SQLNull . fmap toField

