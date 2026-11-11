module Brigid.HTML.Types.InlineJSON
  ( InlineJSON (InlineJSON)
  , inlineJSONToBytes
  , inlineJSONToBytesBuilder
  , inlineJSONToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

newtype InlineJSON =
  InlineJSON
    { inlineJSONToBytes :: LBS.ByteString
    } deriving (Eq, Show)

inlineJSONToBytesBuilder :: InlineJSON -> Builder
inlineJSONToBytesBuilder =
  BSB.lazyByteString . inlineJSONToBytes

inlineJSONToText :: InlineJSON -> T.Text
inlineJSONToText =
  TE.decodeUtf8 . LBS.toStrict . inlineJSONToBytes
