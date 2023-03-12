module Data.Time.Period
    ( Period(..)
    , PeriodLength
    , toDay
    , fromDay
    , week
    )
    where

import Prettyprinter
import Data.Time.Clock (UTCTime(..), NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, posixDayLength, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Period = Period
  { periodStart :: POSIXTime
  , periodEnd :: POSIXTime
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty Period where
    pretty (Period from to) = "[" <> p from <> " - " <> p to <> "]"
        where p = pretty .  iso8601Show . posixSecondsToUTCTime

week :: PeriodLength
week = PeriodLength (7 * posixDayLength)

newtype PeriodLength = PeriodLength NominalDiffTime
  deriving (Show, Eq, Ord)
  deriving newtype (Num, Real)

toDay :: POSIXTime -> Day
toDay posix = day
  where UTCTime day _ = posixSecondsToUTCTime posix

fromDay :: Day -> POSIXTime
fromDay day = utcTimeToPOSIXSeconds $ UTCTime day 0
