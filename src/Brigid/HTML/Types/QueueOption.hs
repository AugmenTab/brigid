module Brigid.HTML.Types.QueueOption
  ( QueueOption
      ( QueueFirst
      , QueueLast
      , QueueAll
      , QueueNone
      )
  , queueOptionToBytes
  , queueOptionToBytesBuilder
  , queueOptionToText
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data QueueOption
  = QueueFirst
  | QueueLast
  | QueueAll
  | QueueNone
  deriving (Bounded, Enum, Eq, Show)

queueOptionToBytes :: QueueOption -> LBS.ByteString
queueOptionToBytes queueOption =
  "queue:" <>
    case queueOption of
      QueueFirst -> "first"
      QueueLast  -> "last"
      QueueAll   -> "all"
      QueueNone  -> "none"

queueOptionToBytesBuilder :: QueueOption -> Builder
{-# INLINE queueOptionToBytesBuilder #-}
queueOptionToBytesBuilder = lazyByteString . queueOptionToBytes

queueOptionToText :: QueueOption -> T.Text
queueOptionToText queueOption =
  "queue:" <>
    case queueOption of
      QueueFirst -> "first"
      QueueLast  -> "last"
      QueueAll   -> "all"
      QueueNone  -> "none"
