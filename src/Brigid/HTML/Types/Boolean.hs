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
