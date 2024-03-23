module HTML.Types.Window
  ( Window (Window)
  , windowToBytes
  , windowToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Window = Window

windowToBytes :: Window -> LBS.ByteString
windowToBytes = const "window"

windowToText :: Window -> T.Text
windowToText = const "window"
