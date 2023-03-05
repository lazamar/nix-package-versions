{-# LANGUAGE ExistentialQuantification #-}
module App.Storage where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Calendar (Day(..))
import GHC.Generics (Generic)

import Data.Git (Hash)
import Nix.Revision (Channel, Revision, Package)
import Nix (Name)

-- | Whether all revision entries were added to the table.
-- Order is important. Success is the max value
data RevisionState
    = PreDownload        -- ^ We are still to start the download and handling of this state
    | Incomplete         -- ^ The process of adding packages to the DB was started but not finished
    | InvalidRevision    -- ^ This revision cannot be built. It is not worth trying again.
    | Success            -- ^ All revision packages were successfully added to the DB
    deriving (Show, Eq, Enum, Read, Ord, Generic)

instance ToJSON RevisionState
instance FromJSON RevisionState

class Storage s where
  -- read
  versions :: s -> Channel -> Name -> IO [(Package, Hash, Day)]
  revisions :: s -> Channel -> IO [(Day, Revision, RevisionState)]

  -- write
  writePackages :: s -> Day -> Revision -> [Package] -> IO ()
  writeRevisionState :: s -> Day -> Revision -> RevisionState -> IO ()

data Database = forall s. Storage s => Database s

instance Storage Database where
    versions (Database s) channel name = versions s channel name
    revisions (Database s) channel = revisions s channel
    writePackages (Database s) day revision packages =
      writePackages s day revision packages
    writeRevisionState (Database s) day revision state =
      writeRevisionState s day revision state

