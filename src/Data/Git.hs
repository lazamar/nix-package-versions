module Data.Git where

import Prettyprinter (Pretty(..), (<+>))
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics (Generic)
import Data.Time.Clock.POSIX (POSIXTime)

newtype Hash = Hash { fromHash :: Text }
    deriving (Eq, Show, Generic, Ord)
    deriving newtype (Hashable, FromJSON, ToJSON)

instance Pretty Hash where
    pretty (Hash txt) = pretty txt

data Commit = Commit Hash POSIXTime
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (Hashable, ToJSON, FromJSONKey, FromJSON, ToJSONKey)

instance Pretty Commit where
    pretty (Commit hash time) = pretty hash <+> p time
        where p = pretty .  iso8601Show . posixSecondsToUTCTime

data Branch = Branch { unBranch :: Text }
    deriving (Show)
