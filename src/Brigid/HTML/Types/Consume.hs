module Brigid.HTML.Types.Consume
  ( Consume (Consume)
  , consumeToBytes
  , consumeToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Consume = Consume

consumeToBytes :: Consume -> LBS.ByteString
consumeToBytes = const "consume"

consumeToText :: Consume -> T.Text
consumeToText = const "consume"
