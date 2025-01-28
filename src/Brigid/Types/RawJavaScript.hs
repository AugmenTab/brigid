module Brigid.Types.RawJavaScript
  ( RawJavaScript (RawJavaScript)
  , rawJavaScriptToBytes
  , rawJavaScriptToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype RawJavaScript =
  RawJavaScript
    { unRawJavaScript :: T.Text
    }

rawJavaScriptToBytes :: RawJavaScript -> LBS.ByteString
rawJavaScriptToBytes =
  ("js:" <>) . Render.textToBytes . unRawJavaScript

rawJavaScriptToText :: RawJavaScript -> T.Text
rawJavaScriptToText =
  ("js:" <>) . unRawJavaScript
