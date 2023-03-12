{-# LANGUAGE ExistentialQuantification #-}
module App.Storage where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Data.Git (Hash, Commit)
import Nix (Package, Channel, Revision, PackageDetails)

-- | Whether all revision entries were added to the table.
-- Order is important. Success is the max value
data CommitState
    = Incomplete  -- ^ The process of adding packages to the DB was started but not finished
    | Broken      -- ^ This revision cannot be built. It is not worth trying again
    | Success     -- ^ All revision packages were successfully added to the DB
    deriving (Show, Eq, Enum, Read, Ord, Generic)

instance ToJSON CommitState
instance FromJSON CommitState

data Period = Period
  { p_start :: POSIXTime
  , p_end :: POSIXTime
  }

class Storage s where
  -- read
  versions :: s -> Channel -> Package -> IO [(PackageDetails, Hash, Day)]
  revisions :: s -> Channel -> IO [(Day, Revision, CommitState)]

  -- write
  writePackages :: s -> Day -> Revision -> [PackageDetails] -> IO ()
  writeRevisionState :: s -> Day -> Revision -> CommitState -> IO ()



  versions' :: s -> Channel -> Package -> IO [(PackageDetails, Commit)]
  coverage :: s -> Channel -> IO [(Period, Commit, CommitState)]

  writeCoverage :: s -> Period -> Channel -> Commit -> IO ()
  writePackage :: s -> Commit -> PackageDetails -> IO ()
  writeCommitState :: s -> Commit -> CommitState -> IO ()

data Database = forall s. Storage s => Database s

instance Storage Database where
    versions (Database s) channel name = versions s channel name
    revisions (Database s) channel = revisions s channel
    writePackages (Database s) day revision packages =
      writePackages s day revision packages
    writeRevisionState (Database s) day revision state =
      writeRevisionState s day revision state


-- all package versions for a package from a channel
--  (SELECT (PKG_COMMIT, NAME, VERSION) from PACKAGES where
--    NAME = name)
--  JOIN SELECT * from COVERAGE where
--    COMMIT = PKG_COMMIT
--    CHANNEL = channel
--
-- all covered periods for a channel
--    SELECT * from COVERAGE where CHANNEL = channel

-- PACKAGES
-- (package name, package versions, key_name, full_name, commit)
-- package info
-- commit

-- COVERAGE
-- (channel, commit, period)
-- channel -> (period, commit)
-- commit -> [(channel, period)]

-- should only be used during indexing
-- COMMMIT STATES
-- (commit, state)
