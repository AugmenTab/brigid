module Brigid.Internal.Escape
  ( attributeBytes
  , attributeBytesBuilder
  , attributeCharBytes
  , attributeCharBytesBuilder
  , attributeText
  , attributeCharText
  , escape
  , escapeBytesBuilder
  , lazyBytesAttributeBytesBuilder
  , urlByteString
  , urlText
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, charUtf8, string8, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types.URI (urlEncode)

import Brigid.Internal.Entities qualified as Entity
import Brigid.Internal.Render qualified as Render

attributeBytes :: LBS.ByteString -> LBS.ByteString
attributeBytes =
  toLazyByteString . lazyBytesAttributeBytesBuilder

attributeBytesBuilder :: T.Text -> Builder
attributeBytesBuilder =
  T.foldl' (\acc c -> acc <> attributeCharBytesBuilder c) mempty

attributeCharBytes :: Char -> LBS.ByteString
attributeCharBytes = toLazyByteString . attributeCharBytesBuilder

attributeCharBytesBuilder :: Char -> Builder
attributeCharBytesBuilder c =
  case c of
    '"'  -> string8 Entity.quotationMark
    '\'' -> string8 Entity.singleQuote
    _    -> charUtf8 c

lazyBytesAttributeBytesBuilder :: LBS.ByteString -> Builder
lazyBytesAttributeBytesBuilder = attributeBytesBuilder . Render.lazyBytesToText

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

escapeBytesBuilder :: T.Text -> Builder
escapeBytesBuilder =
  T.foldl' (\acc c -> acc <> escapeCharBytesBuilder c) mempty

escapeCharBytesBuilder :: Char -> Builder
escapeCharBytesBuilder c =
  case c of
    '&' -> string8 Entity.ampersand
    '<' -> string8 Entity.lessThanSign
    '>' -> string8 Entity.greaterThanSign
    _   -> charUtf8 c

urlByteString :: T.Text -> LBS.ByteString
urlByteString =
  LBS.fromStrict . encodeBS

urlText :: T.Text -> T.Text
urlText =
  TE.decodeUtf8 . encodeBS

encodeBS :: T.Text -> BS.ByteString
encodeBS =
  urlEncode False . TE.encodeUtf8
