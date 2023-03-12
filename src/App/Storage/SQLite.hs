{-# OPTIONS_GHC -Wno-orphans #-}

{-| SQLite implementation of the Storage class
-}

module App.Storage.SQLite (withDatabase) where

import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.String (fromString, IsString)
import Data.Text (pack)
import Data.Time.Clock.POSIX (POSIXTime)
import Database.SQLite.Simple (ToRow(toRow), SQLData(..), NamedParam((:=)))
import Database.SQLite.Simple.FromField (FromField(..))
import Database.SQLite.Simple.ToField (ToField(..))

import Nix
  ( Version(..)
  , PackageWithVersion(..)
  , KeyName(..)
  , Package(..)
  , Channel
  , PackageDetails(..))

import Data.Git (Hash(..), Commit(..))
import Data.Time.Period (Period(..))
import App.Storage (Storage, Database(..), CommitState(..))
import qualified App.Storage as Storage

import Control.Monad.SQL (Connection, MonadSQL(..), runSQL, connect)

newtype SQLiteDatabase = SQLiteDatabase Connection

instance Storage SQLiteDatabase where
  versions (SQLiteDatabase conn) channel package =
    runSQL conn $ versions channel package

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

withDatabase :: FilePath -> (Database -> IO a) -> IO a
withDatabase path act =
  connect path $ \conn -> do
    runSQL conn $ do
      -- Prepare database for usage
      -- Enable foreign key constraints.
      -- It's really weird that they would otherwise just not work.
      execute_ "PRAGMA foreign_keys = ON"
      -- Make sure the correct transaction tracking option is in place
      execute_ "PRAGMA journal_mode = WAL"
      execute_ "PRAGMA synchronous = NORMAL"
      ensureTablesAreCreated
    act $ Database $ SQLiteDatabase conn

-- Constants

db_PACKAGE_DETAILS, db_COVERAGE, db_COMMIT_STATES  :: IsString a => a
db_PACKAGE_DETAILS = "package_details"
db_COVERAGE = "coverage"
db_COMMIT_STATES = "commit_states"

ensureTablesAreCreated :: MonadSQL m => m ()
ensureTablesAreCreated = do
  execute_ $ "CREATE TABLE IF NOT EXISTS " <> db_COMMIT_STATES <> " "
    <> "( COMMIT_HASH       TEXT NOT NULL"
    <> ", COMMIT_DATE       INTEGER NOT NULL"
    <> ", INDEXING_STATE    TEXT NOT NULL"
    <> ", PRIMARY KEY (COMMIT_HASH)"
    <> ")"

  execute_ $ "CREATE TABLE IF NOT EXISTS " <> db_COVERAGE <> " "
    <> "( COMMIT_HASH       TEXT NOT NULL"
    <> ", COMMIT_DATE       INTEGER NOT NULL" -- we keep the date here for convenience
    <> ", CHANNEL           TEXT NOT NULL"
    <> ", PERIOD_START      INTEGER NOT NULL"
    <> ", PERIOD_END        INTEGER NOT NULL"
    <> ", PRIMARY KEY (CHANNEL, COMMIT_HASH, PERIOD_START, PERIOD_END)"
    <> ", FOREIGN KEY (COMMIT_HASH) REFERENCES " <> db_COMMIT_STATES <> " (COMMIT_HASH)"
    <> ")"

  execute_ $ "CREATE TABLE IF NOT EXISTS " <> db_PACKAGE_DETAILS <> " "
    <> "( NAME              TEXT NOT NULL"
    <> ", VERSION           TEXT NOT NULL"
    <> ", KEY_NAME          TEXT NOT NULL"
    <> ", FULL_NAME         TEXT NOT NULL"
    <> ", DESCRIPTION       TEXT"
    <> ", COMMIT_HASH       TEXT NOT NULL"
    <> ", PRIMARY KEY (COMMIT_HASH, KEY_NAME)"
    <> ", FOREIGN KEY (COMMIT_HASH) REFERENCES " <> db_COMMIT_STATES <> " (COMMIT_HASH)"
    <> ")"


  -- This one line caused a 100x speedup in package search
  execute_ $
    "CREATE INDEX IF NOT EXISTS NAME_NOCASE_INDEX on "
    <> db_PACKAGE_DETAILS
    <> " (NAME COLLATE NOCASE)"

versions :: MonadSQL m => Channel -> Package -> m [(PackageDetails, Commit)]
versions channel package = do
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

-- | One row from db_COMMIT_STATES
data SQLCommitState = SQLCommitState Commit CommitState
  deriving Show

instance ToRow SQLCommitState where
  toRow (SQLCommitState (Commit hash time) state) =
    [ toField hash            -- ^ COMMIT_HASH
    , toField (SQLPOSIX time) -- ^ COMMIT_DATE
    , toField state           -- ^ INDEXING_STATE
    ]

-- | One row from db_COVERAGE
data SQLCoverage = SQLCoverage Commit Channel Period
  deriving Show

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

