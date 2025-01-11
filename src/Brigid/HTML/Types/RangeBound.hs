{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.RangeBound
  ( RangeBound
  , RangeBoundTypes
  , mkRangeBound
  , rangeBoundToBytes
  , rangeBoundToText
  , RawRangeBound
  , mkRawRangeBound
  , rawRangeBoundToBytes
  , rawRangeBoundToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.Number (Number, numberToBytes, numberToText)
import Brigid.HTML.Types.Time qualified as BTime

newtype RangeBound = RangeBound (Shrubbery.Union RangeBoundTypes)

type RangeBoundTypes =
  [ BTime.Date -- 'InputDate
  , BTime.DatetimeLocal -- 'InputDatetimeLocal
  , BTime.Month -- 'InputMonth
  , Number -- 'InputNumber, 'InputRange, Meter, Progress
  , BTime.Time -- 'InputTime
  , BTime.Week -- 'InputWeek
  , RawRangeBound
  ]

mkRangeBound :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf rangeBound RangeBoundTypes
                )
             => rangeBound -> RangeBound
mkRangeBound =
  RangeBound . Shrubbery.unify

instance Show RangeBound where
  show (RangeBound rangeBound) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @BTime.Date             show
        . Shrubbery.branch @BTime.DatetimeLocal    show
        . Shrubbery.branch @BTime.Month            show
        . Shrubbery.branch @Number                 show
        . Shrubbery.branch @BTime.Time             show
        . Shrubbery.branch @BTime.Week             show
        . Shrubbery.branch @RawRangeBound          show
        $ Shrubbery.branchEnd
    ) rangeBound

rangeBoundToBytes :: RangeBound -> LBS.ByteString
rangeBoundToBytes (RangeBound rangeBound) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @BTime.Date             BTime.dateToBytes
      . Shrubbery.branch @BTime.DatetimeLocal    BTime.datetimeLocalToBytes
      . Shrubbery.branch @BTime.Month            BTime.monthToBytes
      . Shrubbery.branch @Number                 numberToBytes
      . Shrubbery.branch @BTime.Time             BTime.timeToBytes
      . Shrubbery.branch @BTime.Week             BTime.weekToBytes
      . Shrubbery.branch @RawRangeBound          rawRangeBoundToBytes
      $ Shrubbery.branchEnd
  ) rangeBound

rangeBoundToText :: RangeBound -> T.Text
rangeBoundToText (RangeBound rangeBound) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @BTime.Date             BTime.dateToText
      . Shrubbery.branch @BTime.DatetimeLocal    BTime.datetimeLocalToText
      . Shrubbery.branch @BTime.Month            BTime.monthToText
      . Shrubbery.branch @Number                 numberToText
      . Shrubbery.branch @BTime.Time             BTime.timeToText
      . Shrubbery.branch @BTime.Week             BTime.weekToText
      . Shrubbery.branch @RawRangeBound          rawRangeBoundToText
      $ Shrubbery.branchEnd
  ) rangeBound

newtype RawRangeBound =
  RawRangeBound
    { rawRangeBoundToText :: T.Text
    } deriving (Eq, Show)

mkRawRangeBound :: T.Text -> RawRangeBound
mkRawRangeBound = RawRangeBound

rawRangeBoundToBytes :: RawRangeBound -> LBS.ByteString
rawRangeBoundToBytes =
  LBS8.pack . T.unpack . rawRangeBoundToText
