module Brigid.HTML.Types.InlineJSON
  ( InlineJSON
  , mkInlineJSON
  , inlineJSONToBytes
  , inlineJSONToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Fleece.Aeson qualified as FA

newtype InlineJSON =
  InlineJSON
    { inlineJSONToBytes :: LBS.ByteString
    } deriving (Eq, Show)

mkInlineJSON :: FA.Encoder a -> a -> InlineJSON
mkInlineJSON encoder =
  InlineJSON . FA.encode encoder

inlineJSONToText :: InlineJSON -> T.Text
inlineJSONToText =
  TE.decodeUtf8 . LBS.toStrict . inlineJSONToBytes
