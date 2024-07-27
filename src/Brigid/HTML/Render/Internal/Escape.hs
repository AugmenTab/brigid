module Brigid.HTML.Render.Internal.Escape
  ( attribute
  , attributeChar
  , html
  , urlByteString
  , urlText
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types.URI (urlEncode)

attribute :: T.Text -> T.Text
attribute =
  T.concatMap attributeChar

attributeChar :: Char -> T.Text
attributeChar c =
  case c of
    '"'  -> "&quot;"
    '\'' -> "&#39;"
    _    -> T.singleton c

html :: T.Text -> T.Text
html =
  T.concatMap $
    \c ->
      case c of
        '&' -> "&amp;"
        '<' -> "&lt;"
        '>' -> "&gt;"
        _   -> T.singleton c

urlByteString :: T.Text -> LBS.ByteString
urlByteString =
  LBS.fromStrict . encodeBS

urlText :: T.Text -> T.Text
urlText =
  TE.decodeUtf8 . encodeBS

encodeBS :: T.Text -> BS.ByteString
encodeBS =
  urlEncode False . TE.encodeUtf8
