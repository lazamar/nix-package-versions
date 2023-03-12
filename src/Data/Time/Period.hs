module Data.Time.Period
    ( Period(..)
    , prettyPOSIX
    )
    where

import Prettyprinter
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import GHC.Generics (Generic)

data Period = Period
  { periodStart :: POSIXTime
  , periodEnd :: POSIXTime
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, FromJSONKey, ToJSONKey, Hashable)

instance Pretty Period where
    pretty (Period from to) =
        "[" <> prettyPOSIX from <> " - " <> prettyPOSIX to <> "]"

prettyPOSIX :: POSIXTime -> Doc ann
prettyPOSIX = pretty .  iso8601Show . posixSecondsToUTCTime

