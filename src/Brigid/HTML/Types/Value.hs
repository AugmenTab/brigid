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
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.Email (Email, emailToBytes, emailToText)
import Brigid.HTML.Types.HexColor (HexColor, hexColorToBytes, hexColorToText)
import Brigid.HTML.Types.Phone (PhoneNumber, phoneNumberToBytes, phoneNumberToText)
import Brigid.HTML.Types.Time qualified as BTime
import Brigid.HTML.Types.URL qualified as URL

newtype Value = Value (Shrubbery.Union ValueTypes)

type ValueTypes =
  [ HexColor
  , BTime.Date
  , Email
  , BTime.Month
  , PhoneNumber
  , T.Text
  , BTime.Time
  , URL.AbsoluteURL
  -- , URL.RelativeURL _ -- 'InputUrl; TODO: What methods are permitted?
  , URL.RawURL
  -- , 'InputNumber
  -- , 'InputRange
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
      . Shrubbery.branch @HexColor        hexColorToBytes
      . Shrubbery.branch @BTime.Date      BTime.dateToBytes
      . Shrubbery.branch @Email           emailToBytes
      . Shrubbery.branch @BTime.Month     BTime.monthToBytes
      . Shrubbery.branch @PhoneNumber     phoneNumberToBytes
      . Shrubbery.branch @T.Text          (LBS.fromStrict . TE.encodeUtf8)
      . Shrubbery.branch @BTime.Time      BTime.timeToBytes
      . Shrubbery.branch @URL.AbsoluteURL URL.absoluteURLToBytes
      . Shrubbery.branch @URL.RawURL      URL.rawURLToBytes
      . Shrubbery.branch @BTime.Week      BTime.weekToBytes
      $ Shrubbery.branchEnd
  ) value

valueToText :: Value -> T.Text
valueToText (Value value) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @HexColor        hexColorToText
      . Shrubbery.branch @BTime.Date      BTime.dateToText
      . Shrubbery.branch @Email           emailToText
      . Shrubbery.branch @BTime.Month     BTime.monthToText
      . Shrubbery.branch @PhoneNumber     phoneNumberToText
      . Shrubbery.branch @T.Text          id
      . Shrubbery.branch @BTime.Time      BTime.timeToText
      . Shrubbery.branch @URL.AbsoluteURL URL.absoluteURLToText
      . Shrubbery.branch @URL.RawURL      URL.rawURLToText
      . Shrubbery.branch @BTime.Week      BTime.weekToText
      $ Shrubbery.branchEnd
  ) value
