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

data Coverage = Coverage Period Revision CommitState

data Period = Period
  { start :: POSIXTime
  , end :: POSIXTime
  }

class Storage s where
  -- read
  versions :: s -> Channel -> Package -> IO [(PackageDetails, Hash, Day)]
  revisions :: s -> Channel -> IO [(Day, Revision, CommitState)]

  -- write
  writePackages :: s -> Day -> Revision -> [PackageDetails] -> IO ()
  writeRevisionState :: s -> Day -> Revision -> CommitState -> IO ()

  writeCommitPackages :: s -> Commit -> [PackageDetails] -> IO ()
  writeCommitState :: s -> Commit -> CommitState -> IO ()


  coverage :: s -> Channel -> IO [Coverage]
  versions' :: s -> Channel -> Package -> IO [(PackageDetails, Commit)]

  writeCoverage :: s -> Coverage -> IO ()
  writePackage :: s -> Commit -> PackageDetails -> IO ()

data Database = forall s. Storage s => Database s

instance Storage Database where
    versions (Database s) channel name = versions s channel name
    revisions (Database s) channel = revisions s channel
    writePackages (Database s) day revision packages =
      writePackages s day revision packages
    writeRevisionState (Database s) day revision state =
      writeRevisionState s day revision state


