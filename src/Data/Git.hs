module Data.Git where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Time.Clock.POSIX (POSIXTime)

newtype Hash = Hash { fromHash :: Text }
    deriving (Eq, Show, Generic, Ord)
    deriving newtype (Hashable, FromJSON, ToJSON)

data Commit = Commit Hash POSIXTime
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, ToJSON, FromJSONKey, FromJSON, ToJSONKey)

data Branch = Branch { unBranch :: Text }
    deriving (Show)
