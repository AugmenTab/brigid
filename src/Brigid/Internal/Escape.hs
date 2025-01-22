module Brigid.Internal.Escape
  ( attributeBytes
  , attributeCharBytes
  , attributeText
  , attributeCharText
  , escape
  , urlByteString
  , urlText
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types.URI (urlEncode)

attributeBytes :: LBS.ByteString -> LBS.ByteString
attributeBytes =
  LBS8.concatMap attributeCharBytes

attributeCharBytes :: Char -> LBS.ByteString
attributeCharBytes c =
  case c of
    '"'  -> "&quot;"
    '\'' -> "&#39;"
    _    -> LBS8.singleton c

attributeText :: T.Text -> T.Text
attributeText =
  T.concatMap attributeCharText

attributeCharText :: Char -> T.Text
attributeCharText c =
  case c of
    '"'  -> "&quot;"
    '\'' -> "&#39;"
    _    -> T.singleton c

escape :: T.Text -> T.Text
escape =
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
