module Nix.Storage where

import Data.Time.Calendar (Day(..))

import Nix.Revision (Channel, Revision, Package)
import Nix.Versions.Types (Name, Hash)

-- | Whether all revision entries were added to the table.
-- Order is important. Success is the max value
data RevisionState
    = PreDownload        -- ^ We are still to start the download and handling of this state
    | Incomplete         -- ^ The process of adding packages to the DB was started but not finished
    | InvalidRevision    -- ^ This revision cannot be built. It is not worth trying again.
    | Success            -- ^ All revision packages were successfully added to the DB
    deriving (Show, Eq, Enum, Read, Ord)

class Storage s where
  -- read
  versions :: s -> Channel -> Name -> IO [(Package, Hash, Day)]
  revisions :: s -> Channel -> IO [(Day, Revision, RevisionState)]

  -- write
  writePackages :: s -> Day -> Revision -> [Package] -> IO ()
  writeRevisionState :: s -> Day -> Revision -> RevisionState -> IO ()
