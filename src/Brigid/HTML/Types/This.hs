module Brigid.HTML.Types.This
  (This (This)
  , thisToBytes
  , thisToBytesBuilder
  , thisToText
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data This = This
  deriving (Eq, Show)

thisToBytes :: This -> LBS.ByteString
thisToBytes = const "this"

thisToBytesBuilder :: This -> Builder
{-# INLINE thisToBytesBuilder #-}
thisToBytesBuilder = lazyByteString . thisToBytes

thisToText :: This -> T.Text
thisToText = const "this"
