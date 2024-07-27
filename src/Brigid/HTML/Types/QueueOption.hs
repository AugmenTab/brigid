module Brigid.HTML.Types.QueueOption
  ( QueueOption
      ( QueueFirst
      , QueueLast
      , QueueAll
      , QueueNone
      )
  , queueOptionToBytes
  , queueOptionToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data QueueOption
  = QueueFirst
  | QueueLast
  | QueueAll
  | QueueNone

queueOptionToBytes :: QueueOption -> LBS.ByteString
queueOptionToBytes queueOption =
  "queue:" <>
    case queueOption of
      QueueFirst -> "first"
      QueueLast  -> "last"
      QueueAll   -> "all"
      QueueNone  -> "none"

queueOptionToText :: QueueOption -> T.Text
queueOptionToText queueOption =
  "queue:" <>
    case queueOption of
      QueueFirst -> "first"
      QueueLast  -> "last"
      QueueAll   -> "all"
      QueueNone  -> "none"
