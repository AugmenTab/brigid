module Brigid.HTML.Types.InlineJSON
  ( InlineJSON (InlineJSON)
  , inlineJSONToBytes
  , inlineJSONToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

newtype InlineJSON =
  InlineJSON
    { inlineJSONToBytes :: LBS.ByteString
    } deriving (Eq, Show)

inlineJSONToText :: InlineJSON -> T.Text
inlineJSONToText =
  TE.decodeUtf8 . LBS.toStrict . inlineJSONToBytes
