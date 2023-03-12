{-# LANGUAGE ExistentialQuantification #-}
module App.Storage where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Data.Git (Commit)
import Data.Time.Period (Period)
import Nix (Package, Channel, PackageDetails)

-- | Whether all revision entries were added to the table.
-- Order is important. Success is the max value
data CommitState
    = Incomplete  -- ^ The process of adding packages to the DB was started but not finished
    | Broken      -- ^ This revision cannot be built. It is not worth trying again
    | Success     -- ^ All revision packages were successfully added to the DB
    deriving (Show, Eq, Enum, Read, Ord, Generic)

instance ToJSON CommitState
instance FromJSON CommitState

class Storage s where
  -- | Retrieve all versions available for a package
  -- This will be on the order of the tens, or maximum the
  -- hundreds, so it is fine to just return all of them
  versions :: s -> Channel -> Package -> IO [(PackageDetails, Commit)]
  coverage :: s -> Channel -> IO [(Period, Commit, CommitState)]

  writeCoverage :: s -> Period -> Channel -> Commit -> IO ()
  writePackage :: s -> Commit -> PackageDetails -> IO ()

  -- | When there is a problem building the revision this function allows us
  -- to record that in the database so that later we don't try to build it again
  writeCommitState :: s -> Commit -> CommitState -> IO ()

data Database = forall s. Storage s => Database s

instance Storage Database where
  versions (Database s) channel package =
    versions s channel package
  coverage (Database s) channel =
    coverage s channel
  writeCoverage (Database s) period channel commit =
    writeCoverage s period channel commit
  writePackage (Database s) commit details =
    writePackage s commit details
  writeCommitState (Database s) commit state =
    writeCommitState s commit state


