{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Value
  ( Value
  , ValueTypes
  , mkValue
  , valueToBytes
  , valueToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Ratio (denominator, numerator)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (KnownNat)
import Numeric (showFFloat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.Email (Email, emailToBytes, emailToText)
import Brigid.HTML.Types.HexColor (HexColor, hexColorToBytes, hexColorToText)
import Brigid.HTML.Types.Method (Get, Post)
import Brigid.HTML.Types.Number (Number, numberToBytes, numberToText)
import Brigid.HTML.Types.Phone (PhoneNumber, phoneNumberToBytes, phoneNumberToText)
import Brigid.HTML.Types.Time qualified as BTime
import Brigid.HTML.Types.URL qualified as URL

newtype Value = Value (Shrubbery.Union ValueTypes)

type ValueTypes =
  [ HexColor
  , BTime.Date
  , Email
  , BTime.Month
  , Number
  , Rational
  , PhoneNumber
  , T.Text
  , BTime.Time
  , URL.AbsoluteURL
  , URL.RelativeURL Get
  , URL.RelativeURL Post
  , URL.RawURL
  , BTime.Week
  ]

mkValue :: ( KnownNat branchIndex
           , branchIndex ~ FirstIndexOf value ValueTypes
           )
        => value -> Value
mkValue =
  Value . Shrubbery.unify

instance Show Value where
  show (Value value) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @HexColor               show
        . Shrubbery.branch @BTime.Date             show
        . Shrubbery.branch @Email                  show
        . Shrubbery.branch @BTime.Month            show
        . Shrubbery.branch @Number                 show
        . Shrubbery.branch @Rational               showDecimal
        . Shrubbery.branch @PhoneNumber            show
        . Shrubbery.branch @T.Text                 T.unpack
        . Shrubbery.branch @BTime.Time             show
        . Shrubbery.branch @URL.AbsoluteURL        show
        . Shrubbery.branch @(URL.RelativeURL Get)  show
        . Shrubbery.branch @(URL.RelativeURL Post) show
        . Shrubbery.branch @URL.RawURL             show
        . Shrubbery.branch @BTime.Week             show
        $ Shrubbery.branchEnd
    ) value

valueToBytes :: Value -> LBS.ByteString
valueToBytes (Value value) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @HexColor               hexColorToBytes
      . Shrubbery.branch @BTime.Date             BTime.dateToBytes
      . Shrubbery.branch @Email                  emailToBytes
      . Shrubbery.branch @BTime.Month            BTime.monthToBytes
      . Shrubbery.branch @Number                 numberToBytes
      . Shrubbery.branch @Rational               (LBS8.pack . showDecimal)
      . Shrubbery.branch @PhoneNumber            phoneNumberToBytes
      . Shrubbery.branch @T.Text                 (LBS.fromStrict . TE.encodeUtf8)
      . Shrubbery.branch @BTime.Time             BTime.timeToBytes
      . Shrubbery.branch @URL.AbsoluteURL        URL.absoluteURLToBytes
      . Shrubbery.branch @(URL.RelativeURL Get)  URL.relativeURLToBytes
      . Shrubbery.branch @(URL.RelativeURL Post) URL.relativeURLToBytes
      . Shrubbery.branch @URL.RawURL             URL.rawURLToBytes
      . Shrubbery.branch @BTime.Week             BTime.weekToBytes
      $ Shrubbery.branchEnd
  ) value

valueToText :: Value -> T.Text
valueToText (Value value) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @HexColor               hexColorToText
      . Shrubbery.branch @BTime.Date             BTime.dateToText
      . Shrubbery.branch @Email                  emailToText
      . Shrubbery.branch @BTime.Month            BTime.monthToText
      . Shrubbery.branch @Number                 numberToText
      . Shrubbery.branch @Rational               (T.pack . showDecimal)
      . Shrubbery.branch @PhoneNumber            phoneNumberToText
      . Shrubbery.branch @T.Text                 id
      . Shrubbery.branch @BTime.Time             BTime.timeToText
      . Shrubbery.branch @URL.AbsoluteURL        URL.absoluteURLToText
      . Shrubbery.branch @(URL.RelativeURL Get)  URL.relativeURLToText
      . Shrubbery.branch @(URL.RelativeURL Post) URL.relativeURLToText
      . Shrubbery.branch @URL.RawURL             URL.rawURLToText
      . Shrubbery.branch @BTime.Week             BTime.weekToText
      $ Shrubbery.branchEnd
  ) value

-- Helpers
--
toDecimal :: Rational -> Double
toDecimal = fromRational

showDecimal :: Rational -> String
showDecimal n =
  if denominator n == 1
     then show $ numerator n
     else showFFloat Nothing (toDecimal n) ""
