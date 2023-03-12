module Data.Time.Period
    ( Period(..)
    , PeriodLength
    , prettyPeriod
    , toDay
    , fromDay
    , week
    )
    where

import Data.Time.Clock (UTCTime(..), NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, posixDayLength, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Calendar (Day, showGregorian)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data Period = Period
  { periodStart :: POSIXTime
  , periodEnd :: POSIXTime
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

week :: PeriodLength
week = PeriodLength (7 * posixDayLength)

newtype PeriodLength = PeriodLength NominalDiffTime
  deriving (Show, Eq, Ord)
  deriving newtype (Num, Real)

prettyPeriod :: Period -> String
prettyPeriod (Period from to) = unwords
  [ "["
  , showGregorian $ toDay from
  , "-"
  , showGregorian $ toDay to
  , "]"
  ]

toDay :: POSIXTime -> Day
toDay posix = day
  where UTCTime day _ = posixSecondsToUTCTime posix

fromDay :: Day -> POSIXTime
fromDay day = utcTimeToPOSIXSeconds $ UTCTime day 0
