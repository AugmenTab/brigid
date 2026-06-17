module Brigid.Types.RawJavaScript
  ( RawJavaScript (RawJavaScript)
  , rawJavaScriptToBytes
  , rawJavaScriptToBytesBuilder
  , rawJavaScriptToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Brigid.Internal.Render qualified as Render

newtype RawJavaScript =
  RawJavaScript
    { unRawJavaScript :: T.Text
    } deriving (Eq, Show)

rawJavaScriptToBytes :: RawJavaScript -> LBS.ByteString
rawJavaScriptToBytes =
  ("js:" <>) . Render.textToLazyBytes . unRawJavaScript

rawJavaScriptToBytesBuilder :: RawJavaScript -> Builder
{-# INLINE rawJavaScriptToBytesBuilder #-}
rawJavaScriptToBytesBuilder js =
  string8 "js:" <> TE.encodeUtf8Builder (unRawJavaScript js)

rawJavaScriptToText :: RawJavaScript -> T.Text
rawJavaScriptToText =
  ("js:" <>) . unRawJavaScript
