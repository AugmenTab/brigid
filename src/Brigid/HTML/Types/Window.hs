module Brigid.HTML.Types.Window
  ( Window (Window)
  , windowToBytes
  , windowToBytesBuilder
  , windowToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Window = Window
  deriving (Eq, Show)

windowToBytes :: Window -> LBS.ByteString
windowToBytes = const "window"

windowToBytesBuilder :: Window -> Builder
windowToBytesBuilder = const "window"

windowToText :: Window -> T.Text
windowToText = const "window"
