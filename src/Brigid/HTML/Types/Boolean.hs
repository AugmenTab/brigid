module Brigid.HTML.Types.Boolean
  ( OnOff
      ( On
      , Off
      )
  , onOffToBytes
  , onOffToText
  , onOffBool
  , isOn
  , isOff
  , OpenClosed
      ( Open
      , Closed
      )
  , openClosedToBytes
  , openClosedToText
  , openClosedBool
  , isOpen
  , isClosed
  , YesNo
      ( Yes
      , No
      )
  , yesNoToBytes
  , yesNoToText
  , yesNoBool
  , isYes
  , isNo
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data OnOff
  = On
  | Off
  deriving (Bounded, Enum, Eq, Show)

onOffToBytes :: OnOff -> LBS.ByteString
onOffToBytes onOff =
  case onOff of
    On  -> "on"
    Off -> "off"

onOffToText :: OnOff -> T.Text
onOffToText onOff =
  case onOff of
    On  -> "on"
    Off -> "off"

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
    Open -> "on"
    Closed -> "off"

openClosedToText :: OpenClosed -> T.Text
openClosedToText openClosed =
  case openClosed of
    Open -> "on"
    Closed -> "off"

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

yesNoToText :: YesNo -> T.Text
yesNoToText yesNo =
  case yesNo of
    Yes -> "yes"
    No  -> "no"

yesNoBool :: Bool -> YesNo
yesNoBool b =
  if b
    then Yes
    else No

isYes :: YesNo -> Bool
isYes = (==) Yes

isNo :: YesNo -> Bool
isNo = (==) No
