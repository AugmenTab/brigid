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
import Data.ByteString.Builder.Prim qualified as BPrim
import Data.ByteString.Builder.Prim ((>$<), (>*<))
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Network.HTTP.Types.URI (urlEncode)

import Brigid.Internal.Entities qualified as Entity
import Brigid.Internal.Render qualified as Render

attributeBytes :: LBS.ByteString -> LBS.ByteString
attributeBytes = toLazyByteString . lazyBytesAttributeBytesBuilder

attributeBytesBuilder :: T.Text -> Builder
attributeBytesBuilder = TE.encodeUtf8BuilderEscaped attrEscapePrim

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
attributeText = T.concatMap attributeCharText

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

fixed5 :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> BPrim.FixedPrim Word8
{-# INLINE fixed5 #-}
fixed5 b1 b2 b3 b4 b5 =
  const (b1, (b2, (b3, (b4, b5)))) >$<
    (BPrim.word8 >*< (BPrim.word8 >*< (BPrim.word8 >*< (BPrim.word8 >*< BPrim.word8))))

htmlEscapePrim :: BPrim.BoundedPrim Word8
{-# INLINE htmlEscapePrim #-}
htmlEscapePrim =
  BPrim.condB (== 0x26) (BPrim.liftFixedToBounded $ fixed5 0x26 0x23 0x33 0x38 0x3B) $ -- & → &#38;
  BPrim.condB (== 0x3C) (BPrim.liftFixedToBounded $ fixed5 0x26 0x23 0x36 0x30 0x3B) $ -- < → &#60;
  BPrim.condB (== 0x3E) (BPrim.liftFixedToBounded $ fixed5 0x26 0x23 0x36 0x32 0x3B) $ -- > → &#62;
  BPrim.liftFixedToBounded BPrim.word8

attrEscapePrim :: BPrim.BoundedPrim Word8
{-# INLINE attrEscapePrim #-}
attrEscapePrim =
  BPrim.condB (== 0x22) (BPrim.liftFixedToBounded $ fixed5 0x26 0x23 0x33 0x34 0x3B) $ -- " → &#34;
  BPrim.condB (== 0x27) (BPrim.liftFixedToBounded $ fixed5 0x26 0x23 0x33 0x39 0x3B) $ -- ' → &#39;
  BPrim.liftFixedToBounded BPrim.word8

escapeBytesBuilder :: T.Text -> Builder
escapeBytesBuilder = TE.encodeUtf8BuilderEscaped htmlEscapePrim

urlByteString :: T.Text -> LBS.ByteString
urlByteString = LBS.fromStrict . encodeBS

urlText :: T.Text -> T.Text
urlText = TE.decodeUtf8 . encodeBS

encodeBS :: T.Text -> BS.ByteString
encodeBS = urlEncode False . TE.encodeUtf8
