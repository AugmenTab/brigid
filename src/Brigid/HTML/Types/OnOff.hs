module Brigid.HTML.Types.OnOff
  ( OnOff
      ( On
      , Off
      )
  , onOffToBytes
  , onOffToText
  , isOn
  , isOff
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

isOn :: OnOff -> Bool
isOn = (==) On

isOff :: OnOff -> Bool
isOff = (==) Off
