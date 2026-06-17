module Brigid.HTML.Types.Consume
  ( Consume (Consume)
  , consumeToBytes
  , consumeToBytesBuilder
  , consumeToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Consume = Consume
  deriving (Eq, Show)

consumeToBytes :: Consume -> LBS.ByteString
consumeToBytes = const "consume"

consumeToBytesBuilder :: Consume -> Builder
{-# INLINE consumeToBytesBuilder #-}
consumeToBytesBuilder = const "consume"

consumeToText :: Consume -> T.Text
consumeToText = const "consume"
