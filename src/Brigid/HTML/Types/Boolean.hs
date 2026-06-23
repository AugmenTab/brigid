module Brigid.HTML.Types.Boolean
  ( OnOff
      ( On
      , Off
      )
  , onOffToBytes
  , onOffToBytesBuilder
  , onOffToText
  , onOffToTextBuilder
  , onOffBool
  , isOn
  , isOff
  , OpenClosed
      ( Open
      , Closed
      )
  , openClosedToBytes
  , openClosedToBytesBuilder
  , openClosedToText
  , openClosedToTextBuilder
  , openClosedBool
  , isOpen
  , isClosed
  , YesNo
      ( Yes
      , No
      )
  , yesNoToBytes
  , yesNoToBytesBuilder
  , yesNoToText
  , yesNoToTextBuilder
  , yesNoBool
  , isYes
  , isNo
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data OnOff
  = On
  | Off
  deriving (Bounded, Enum, Eq, Show)

onOffToBytes :: OnOff -> LBS.ByteString
onOffToBytes onOff =
  case onOff of
    On  -> "on"
    Off -> "off"

onOffToBytesBuilder :: OnOff -> Builder
{-# INLINE onOffToBytesBuilder #-}
onOffToBytesBuilder onOff =
  case onOff of
    On  -> string8 "on"
    Off -> string8 "off"

onOffToText :: OnOff -> T.Text
onOffToText onOff =
  case onOff of
    On  -> "on"
    Off -> "off"

onOffToTextBuilder :: OnOff -> TBL.Builder
onOffToTextBuilder = TBL.fromText . onOffToText

onOffBool :: Bool -> OnOff
onOffBool b =
  if b
    then On
    else Off

isOn :: OnOff -> Bool
isOn = (==) On

isOff :: OnOff -> Bool
isOff = (==) Off

data OpenClosed
  = Open
  | Closed
  deriving (Bounded, Enum, Eq, Show)

openClosedToBytes :: OpenClosed -> LBS.ByteString
openClosedToBytes openClosed =
  case openClosed of
    Open   -> "open"
    Closed -> "closed"

openClosedToBytesBuilder :: OpenClosed -> Builder
{-# INLINE openClosedToBytesBuilder #-}
openClosedToBytesBuilder openClosed =
  case openClosed of
    Open   -> string8 "open"
    Closed -> string8 "closed"

openClosedToText :: OpenClosed -> T.Text
openClosedToText openClosed =
  case openClosed of
    Open   -> "open"
    Closed -> "closed"

openClosedToTextBuilder :: OpenClosed -> TBL.Builder
openClosedToTextBuilder = TBL.fromText . openClosedToText

openClosedBool :: Bool -> OpenClosed
openClosedBool b =
  if b
    then Open
    else Closed

isOpen :: OpenClosed -> Bool
isOpen = (==) Open

isClosed :: OpenClosed -> Bool
isClosed = (==) Closed

data YesNo
  = Yes
  | No
  deriving (Bounded, Enum, Eq, Show)

yesNoToBytes :: YesNo -> LBS.ByteString
yesNoToBytes yesNo =
  case yesNo of
    Yes -> "yes"
    No  -> "no"

yesNoToBytesBuilder :: YesNo -> Builder
{-# INLINE yesNoToBytesBuilder #-}
yesNoToBytesBuilder yesNo =
  case yesNo of
    Yes -> string8 "yes"
    No  -> string8 "no"

yesNoToText :: YesNo -> T.Text
yesNoToText yesNo =
  case yesNo of
    Yes -> "yes"
    No  -> "no"

yesNoToTextBuilder :: YesNo -> TBL.Builder
yesNoToTextBuilder = TBL.fromText . yesNoToText

yesNoBool :: Bool -> YesNo
yesNoBool b =
  if b
    then Yes
    else No

isYes :: YesNo -> Bool
isYes = (==) Yes

isNo :: YesNo -> Bool
isNo = (==) No
