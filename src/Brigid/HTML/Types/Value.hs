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
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.EmailAddress (EmailAddress, emailAddressToBytes, emailAddressToText)
import Brigid.HTML.Types.HexColor (HexColor, hexColorToBytes, hexColorToText)
import Brigid.HTML.Types.Method (Get, Post)
import Brigid.HTML.Types.Number (Number, numberToBytes, numberToText)
import Brigid.HTML.Types.Phone (PhoneNumber, phoneNumberToBytes, phoneNumberToText)
import Brigid.HTML.Types.Time qualified as BTime
import Brigid.HTML.Types.URL qualified as URL
import Brigid.Internal.Render qualified as Render

newtype Value = Value (Shrubbery.Union ValueTypes)

instance Show Value where
  show (Value value) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @HexColor               show
        . Shrubbery.branch @BTime.Date             show
        . Shrubbery.branch @BTime.DatetimeLocal    show
        . Shrubbery.branch @EmailAddress           show
        . Shrubbery.branch @BTime.Month            show
        . Shrubbery.branch @Number                 show
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

type ValueTypes =
  [ HexColor
  , BTime.Date
  , BTime.DatetimeLocal
  , EmailAddress
  , BTime.Month
  , Number
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

valueToBytes :: Value -> LBS.ByteString
valueToBytes (Value value) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @HexColor               hexColorToBytes
      . Shrubbery.branch @BTime.Date             BTime.dateToBytes
      . Shrubbery.branch @BTime.DatetimeLocal    BTime.datetimeLocalToBytes
      . Shrubbery.branch @EmailAddress           emailAddressToBytes
      . Shrubbery.branch @BTime.Month            BTime.monthToBytes
      . Shrubbery.branch @Number                 numberToBytes
      . Shrubbery.branch @PhoneNumber            phoneNumberToBytes
      . Shrubbery.branch @T.Text                 Render.textToBytes
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
      . Shrubbery.branch @BTime.DatetimeLocal    BTime.datetimeLocalToText
      . Shrubbery.branch @EmailAddress           emailAddressToText
      . Shrubbery.branch @BTime.Month            BTime.monthToText
      . Shrubbery.branch @Number                 numberToText
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
