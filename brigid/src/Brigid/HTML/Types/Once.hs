module Brigid.HTML.Types.Once
  ( Once (Once)
  , onceToBytes
  , onceToBytesBuilder
  , onceToText
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Once = Once
  deriving (Eq, Show)

onceToBytes :: Once -> LBS.ByteString
onceToBytes = const "once"

onceToBytesBuilder :: Once -> Builder
{-# INLINE onceToBytesBuilder #-}
onceToBytesBuilder = lazyByteString . onceToBytes

onceToText :: Once -> T.Text
onceToText = const "once"
