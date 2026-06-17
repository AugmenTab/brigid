module Brigid.HTML.Types.Changed
  ( Changed (Changed)
  , changedToBytes
  , changedToBytesBuilder
  , changedToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Changed = Changed
  deriving (Eq, Show)

changedToBytes :: Changed -> LBS.ByteString
changedToBytes = const "changed"

changedToBytesBuilder :: Changed -> Builder
{-# INLINE changedToBytesBuilder #-}
changedToBytesBuilder = const "changed"

changedToText :: Changed -> T.Text
changedToText = const "changed"
