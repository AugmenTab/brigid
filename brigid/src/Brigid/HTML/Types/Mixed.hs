module Brigid.HTML.Types.Mixed
  ( Mixed (Mixed)
  , mixedToBytes
  , mixedToBytesBuilder
  , mixedToText
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Mixed = Mixed
  deriving (Eq, Show)

mixedToBytes :: Mixed -> LBS.ByteString
mixedToBytes Mixed = "mixed"

mixedToBytesBuilder :: Mixed -> Builder
{-# INLINE mixedToBytesBuilder #-}
mixedToBytesBuilder = lazyByteString . mixedToBytes

mixedToText :: Mixed -> T.Text
mixedToText Mixed = "mixed"
