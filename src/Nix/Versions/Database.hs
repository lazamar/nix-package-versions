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
    , save
    , saveRevision
    , saveRevisionWithState

    -- Read
    , versions
    , revisions
    , revisionCommit
    ) where

import Control.Monad.Conc.Class (MonadConc)
import Control.Concurrent.Classy.Async (mapConcurrently_)
import Control.Exception (assert)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
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

db_REVISION_COMMITS, db_REVISIONS, db_PACKAGE_VERSIONS :: IsString a => a
db_PACKAGE_VERSIONS = "PACKAGE_VERSIONS"
db_REVISIONS        = "REVISIONS"
db_REVISION_COMMITS = "REVISION_COMMITS"

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
    SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS  " <> db_REVISION_COMMITS <> " "
                        -- | Details about the Revision's commit
                        <> "( HASH              TEXT        NOT NULL PRIMARY KEY"
                        <> ", DAY               INTEGER     NOT NULL"
                        -- | Whether we were able to successfully add all packages versions
                        -- available in this commit
                        <> ", STATE             TEXT        NOT NULL"
                        <> ")"

    SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS  " <> db_REVISIONS <> " "
                        -- The same commit hash can be used for multiple channels and for
                        -- multiple dates of the same channel
                        <> "( COMMIT_HASH       TEXT        NOT NULL"
                        <> ", CHANNEL           TEXT        NOT NULL" -- TODO: Foreign key
                        -- | Even though the commit might have been done in a certain date,
                        -- we added it to the database to represent the state of nixpkgs
                        -- on a specific date, which may be different from the exact commit date.
                        <> ", REPRESENTS_DAY    INTEGER     NOT NULL"
                        <> ", PRIMARY KEY (CHANNEL, REPRESENTS_DAY)"
                        <> ", FOREIGN KEY (COMMIT_HASH) REFERENCES " <> db_REVISION_COMMITS <> "(HASH)"
                        <> ")"

    SQL.execute_ conn $ "CREATE TABLE IF NOT EXISTS  " <> db_PACKAGE_VERSIONS <> " "
                        <> "( PACKAGE_NAME  TEXT NOT NULL"
                        <> ", VERSION_NAME  TEXT NOT NULL"
                        <> ", REVISION_HASH TEXT NOT NULL"
                        <> ", DESCRIPTION   TEXT"
                        <> ", NIXPATH       TEXT"
                        <> ", PRIMARY KEY (PACKAGE_NAME, VERSION_NAME, REVISION_HASH)"
                        <> ", FOREIGN KEY (REVISION_HASH) REFERENCES " <> db_REVISION_COMMITS <> "(HASH)"
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
versions :: MonadIO m => Connection -> Channel -> Name -> m [(Hash, Package)]
versions (Connection conn) channel (Name name) = liftIO $ do
    results <- SQL.queryNamed
        conn
        (fromString $ unwords
            [ "SELECT"
            ,   "PACKAGE_NAME,"
            ,   "VERSION_NAME,"
            ,   "REVISION_HASH,"
            ,   "DESCRIPTION,"
            ,   "NIXPATH"
            , "FROM"
            , db_PACKAGE_VERSIONS, "INNER JOIN", db_REVISIONS
            , "WHERE"
            , "COMMIT_HASH = REVISION_HASH"
            , "AND CHANNEL =  :channel"
            , "AND PACKAGE_NAME = :name"
            ]
        )
        [ ":name"    := unpack name
        , ":channel" := show channel
        ]
    return $ toVersionInfo <$> results
        where
            toVersionInfo  (SQLPackageVersion (_, pkg, hash)) = (hash, pkg)

-- | Retrieve all revisions available in the database
-- This will be between one hundred and one thousand.
revisions :: MonadIO m => Connection -> Channel -> m [(Day, Revision, RevisionState)]
revisions (Connection conn) channel = liftIO $ do
    results <- SQL.query
        conn
        (sqlRevisionQueryWithWhere  <> " AND CHANNEL = ?")
        [show channel]
    return $ toInfo <$> results

revisionCommit :: MonadIO m => Connection -> Hash ->  m (Maybe (Commit, RevisionState))
revisionCommit  (Connection conn) (Hash hash) = liftIO $ do
    commits <- SQL.query
        conn
        ("SELECT * FROM " <> db_REVISION_COMMITS <> " AND HASH = ?")
        [show hash]
    return $ assert (length commits <= 1) $ toResult <$> listToMaybe commits
    where
        toResult (SQLRevisionCommit commit state) = (commit, state)


toInfo :: SQLRevision -> (Day, Revision, RevisionState)
toInfo (SQLRevision day revision state) = (day, revision, state)

-------------------------------------------------------------------------------
-- Write

-- | When there is a problem building the revision this function allows us
-- to record that in the database so that later we don't try to build it again
saveRevision :: MonadIO m => Connection -> Day -> Revision -> m ()
saveRevision conn represents revision =
    persistRevision conn represents revision

saveRevisionWithState :: MonadIO m => Connection -> Day -> Revision -> RevisionState -> m ()
saveRevisionWithState conn represents revision@(Revision _ commit) state = do
    persistCommit conn commit state
    persistRevision conn represents revision

-- | Save the entire database
save :: (MonadConc m, MonadIO m) => Connection -> Day -> Revision -> RevisionPackages -> m ()
save conn represents revision packages = do
    saveRevisionWithState conn represents revision Incomplete
    mapConcurrently_ persistPackage (HashMap.toList packages)
    saveRevisionWithState conn represents revision Success
    where
        Revision _ (Commit hash _) = revision

        persistPackage (name, info) =
            persistVersion conn hash name info


persistRevision :: MonadIO m => Connection -> Day -> Revision -> m ()
persistRevision (Connection conn) represents (Revision channel (Commit hash _)) =
    liftIO $ SQL.execute conn
            ("INSERT OR REPLACE INTO " <> db_REVISIONS <> " VALUES (?,?,?)")
            (SQLRevisionRow hash channel represents)

-- | Save the version info of a package in the database
persistVersion :: MonadIO m => Connection -> Hash -> Name -> Package -> m ()
persistVersion (Connection conn) hash name info =
    liftIO $ SQL.execute conn
            ("INSERT OR REPLACE INTO " <> db_PACKAGE_VERSIONS <> " VALUES (?,?,?,?,?)")
            (SQLPackageVersion (name, info, hash))

persistCommit :: MonadIO m => Connection -> Commit -> RevisionState -> m ()
persistCommit (Connection conn) commit state =
    liftIO $ SQL.execute conn
            ("INSERT OR REPLACE INTO " <> db_REVISION_COMMITS <> " VALUES (?,?,?)")
            (SQLRevisionCommit commit state)


-- | Whether all revision entries were added to the table.
data RevisionState
    = Success            -- ^ All revision packages were successfully added to the DB
    | Incomplete         -- ^ The process of adding packages to the DB was started but not finished
    | InvalidRevision    -- ^ This revision cannot be built. It is not worth trying again.
    deriving (Show, Eq, Enum, Read)


-- | A query that yields a value that can become an SQLRevision value
sqlRevisionQueryWithWhere :: IsString a => a
sqlRevisionQueryWithWhere = fromString $ unwords
    ["SELECT"
    ,  "COMMIT_HASH,"
    ,  "DAY,"
    ,  "CHANNEL,"
    ,  "REPRESENTS_DAY,"
    ,  "STATE"
    , "FROM"
    , db_REVISIONS, "INNER JOIN", db_REVISION_COMMITS
    , "WHERE"
    , "COMMIT_HASH = HASH"
    ]

-- | One row of a join between db_REVISIONS and db_REVISION_COMMITS
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

-- | One row of db_REVISIONS
data SQLRevisionRow = SQLRevisionRow Hash Channel Day
    deriving (Show, Eq)

instance ToRow SQLRevisionRow  where
    toRow (SQLRevisionRow hash channel day) =
        [ toField hash
        , toField channel
        , toField day
        ]

instance FromRow SQLRevisionRow  where
    fromRow = SQLRevisionRow
        <$> SQL.field
        <*> SQL.field
        <*> (SQL.field <&> ModifiedJulianDay . fromInteger)

-- | One row of db_PACKAGE_VERSIONS
newtype SQLPackageVersion = SQLPackageVersion (Name, Package, Hash)
    deriving (Show, Eq)

instance ToRow SQLPackageVersion where
    toRow (SQLPackageVersion (name, Package { description, nixpkgsPath, version }, hash)) =
        [ toField name         -- ^ PACKAGE_NAME
        , toField version      -- ^ VERSION_NAME
        , toField hash         -- ^ REVISION_HASH
        , nullable description -- ^ DESCRIPTION
        , nullable nixpkgsPath -- ^ NIXPATH
        ]

-- | One row of db_REVISION_COMMITS
data SQLRevisionCommit = SQLRevisionCommit Commit RevisionState
    deriving (Show, Eq)

instance ToRow SQLRevisionCommit  where
    toRow (SQLRevisionCommit (Commit hash date) state) =
        [ toField hash     -- ^ HASH
        , toField date     -- ^ DAY
        , toField state    -- ^ STATE
        ]

instance FromRow SQLRevisionCommit  where
    fromRow = create
        <$> SQL.field
        <*> SQL.field
        <*> SQL.field
        where
            create hash date state =
                SQLRevisionCommit (Commit hash date) state

-------------

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
