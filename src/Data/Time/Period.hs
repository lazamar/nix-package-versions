module Data.Time.Period
    ( Period(..)
    , toDay
    , fromDay
    )
    where

import Prettyprinter
import Data.Time.Clock (UTCTime(..))
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Calendar (Day)
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
    pretty (Period from to) = "[" <> p from <> " - " <> p to <> "]"
        where p = pretty .  iso8601Show . posixSecondsToUTCTime

toDay :: POSIXTime -> Day
toDay posix = day
  where UTCTime day _ = posixSecondsToUTCTime posix

fromDay :: Day -> POSIXTime
fromDay day = utcTimeToPOSIXSeconds $ UTCTime day 0
