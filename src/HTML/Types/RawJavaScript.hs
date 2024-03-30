module HTML.Types.RawJavaScript
  ( RawJavaScript (RawJavaScript)
  , rawJavaScriptToBytes
  , rawJavaScriptToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

newtype RawJavaScript =
  RawJavaScript
    { unRawJavaScript :: T.Text
    }

rawJavaScriptToBytes :: RawJavaScript -> LBS.ByteString
rawJavaScriptToBytes =
  ("js:" <>) . LBS.fromStrict . TE.encodeUtf8 . unRawJavaScript

rawJavaScriptToText :: RawJavaScript -> T.Text
rawJavaScriptToText =
  ("js:" <>) . unRawJavaScript
