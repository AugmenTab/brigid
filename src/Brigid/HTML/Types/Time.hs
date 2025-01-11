module Brigid.HTML.Types.Time
  ( Date
  , mkDate
  , dateToBytes
  , dateToText
  , DatetimeLocal
  , mkDatetimeLocal
  , datetimeLocalToBytes
  , datetimeLocalToText
  , Month
  , mkMonth
  , monthFromDay
  , monthToBytes
  , monthToText
  , MonthOfYear
      ( January
      , February
      , March
      , April
      , May
      , June
      , July
      , August
      , September
      , October
      , November
      , December
      )
  , Time
  , mkTime
  , timeToBytes
  , timeToText
  , Week
  , mkWeek
  , weekToBytes
  , weekToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Data.Time qualified as Time

newtype Date = Date Time.Day
  deriving (Eq, Time.FormatTime)

instance Show Date where
  show = dateToString

mkDate :: Time.Day -> Date
mkDate = Date

dateToBytes :: Date -> LBS.ByteString
dateToBytes = LBS8.pack . dateToString

dateToString :: Date -> String
dateToString = format "%Y-%m-%d"

dateToText :: Date -> T.Text
dateToText = T.pack . dateToString

newtype DatetimeLocal = DatetimeLocal Time.LocalTime
  deriving (Eq, Time.FormatTime)

instance Show DatetimeLocal where
  show = datetimeLocalToString

mkDatetimeLocal :: Time.LocalTime -> DatetimeLocal
mkDatetimeLocal = DatetimeLocal

datetimeLocalToBytes :: DatetimeLocal -> LBS.ByteString
datetimeLocalToBytes = LBS8.pack . datetimeLocalToString

datetimeLocalToString :: DatetimeLocal -> String
datetimeLocalToString = format "%Y-%m-%dT%H:%M"


datetimeLocalToText :: DatetimeLocal -> T.Text
datetimeLocalToText = T.pack . datetimeLocalToString

newtype Month = Month Time.Day
  deriving (Eq, Time.FormatTime)

instance Show Month where
  show = monthToString

mkMonth :: Time.Year -> MonthOfYear -> Month
mkMonth year month =
  monthFromDay $ Time.fromGregorian year (monthOfYear month) 1

monthFromDay :: Time.Day -> Month
monthFromDay = Month

monthToBytes :: Month -> LBS.ByteString
monthToBytes = LBS8.pack . monthToString

monthToString :: Month -> String
monthToString = format "%Y-%m"

monthToText :: Month -> T.Text
monthToText = T.pack . monthToString

data MonthOfYear
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

monthOfYear :: MonthOfYear -> Time.MonthOfYear
monthOfYear month =
  case month of
    January   -> Time.January
    February  -> Time.February
    March     -> Time.March
    April     -> Time.April
    May       -> Time.May
    June      -> Time.June
    July      -> Time.July
    August    -> Time.August
    September -> Time.September
    October   -> Time.October
    November  -> Time.November
    December  -> Time.December

newtype Time = Time Time.TimeOfDay
  deriving (Eq, Time.FormatTime)

instance Show Time where
  show = timeToString

mkTime :: Time.TimeOfDay -> Time
mkTime = Time

timeToBytes :: Time -> LBS.ByteString
timeToBytes = LBS8.pack . timeToString

timeToString :: Time -> String
timeToString = format "%H:%M"

timeToText :: Time -> T.Text
timeToText = T.pack . timeToString

newtype Week = Week Time.Day
  deriving (Eq, Time.FormatTime)

instance Show Week where
  show = weekToString

mkWeek :: Time.Day -> Week
mkWeek = Week

weekToBytes :: Week -> LBS.ByteString
weekToBytes = LBS8.pack . weekToString

weekToString :: Week -> String
weekToString = format "%G-W%V"

weekToText :: Week -> T.Text
weekToText = T.pack . weekToString

-- Helpers
--
format :: Time.FormatTime t => String -> t -> String
format =
  Time.formatTime Time.defaultTimeLocale
