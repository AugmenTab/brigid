module Brigid.HTML.Types.Window
  ( Window (Window)
  , windowToBytes
  , windowToBytesBuilder
  , windowToText
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Window = Window
  deriving (Eq, Show)

windowToBytes :: Window -> LBS.ByteString
windowToBytes = const "window"

windowToBytesBuilder :: Window -> Builder
{-# INLINE windowToBytesBuilder #-}
windowToBytesBuilder = lazyByteString . windowToBytes

windowToText :: Window -> T.Text
windowToText = const "window"
