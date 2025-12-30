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

import Brigid.Internal.Entities qualified as Entity

attributeBytes :: LBS.ByteString -> LBS.ByteString
attributeBytes =
  LBS8.concatMap attributeCharBytes

attributeCharBytes :: Char -> LBS.ByteString
attributeCharBytes c =
  case c of
    '"'  -> LBS8.pack Entity.quotationMark
    '\'' -> LBS8.pack Entity.singleQuote
    _    -> LBS8.singleton c

attributeText :: T.Text -> T.Text
attributeText =
  T.concatMap attributeCharText

attributeCharText :: Char -> T.Text
attributeCharText c =
  case c of
    '"'  -> T.pack Entity.quotationMark
    '\'' -> T.pack Entity.singleQuote
    _    -> T.singleton c

escape :: T.Text -> T.Text
escape =
  T.concatMap $
    \c ->
      case c of
        '&' -> T.pack Entity.ampersand
        '<' -> T.pack Entity.lessThanSign
        '>' -> T.pack Entity.greaterThanSign
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
