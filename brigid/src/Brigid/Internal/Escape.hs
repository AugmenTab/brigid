-- | Escaping helpers shared by the HTML and HXML renderers.
--
-- = Attribute value contract
--
-- Every quoted attribute value is escaped exactly once, at the serialization
-- boundary, by the renderer's @buildAttribute@ family of functions. Callers
-- of those functions always pass the /raw/, unescaped value. Nothing else in
-- the renderers is permitted to escape attribute values, which is what rules
-- out double escaping.
--
-- For HTML attribute values the following characters are replaced with
-- numeric character references:
--
-- > &  -> &#38;
-- > <  -> &#60;
-- > >  -> &#62;
-- > "  -> &#34;
-- > '  -> &#39;
--
-- For XML (HXML) attribute values the same five characters are escaped, plus
-- tab, line feed and carriage return (@&#9;@, @&#10;@, @&#13;@) so that XML
-- attribute-value normalization does not alter them during parsing.
--
-- All escaped characters are ASCII. UTF-8 never encodes non-ASCII code points
-- using ASCII bytes, so escaping UTF-8 output byte-by-byte is equivalent to
-- escaping the source text character-by-character.
module Brigid.Internal.Escape
  ( -- * Attribute values
    htmlAttributeBytesBuilder
  , htmlAttributeTextBytesBuilder
  , htmlAttributeLazyBytesBuilder
  , htmlAttributeTextBuilder
  , htmlAttributeText
  , xmlAttributeBytesBuilder
  , xmlAttributeTextBytesBuilder
  , xmlAttributeLazyBytesBuilder
  , xmlAttributeTextBuilder
  , xmlAttributeText
    -- * Attribute names
  , isSafeAttributeName
    -- * Text content
  , escape
  , escapeBytesBuilder
  , escapeTextBuilder
    -- * URLs
  , urlByteString
  , urlText
  ) where

import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra qualified as BSBE
import Data.ByteString.Builder.Prim qualified as BPrim
import Data.ByteString.Builder.Prim ((>$<), (>*<))
import Data.ByteString.Lazy qualified as LBS
import Data.Char (isControl)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Network.HTTP.Types.URI (urlEncode)

import Brigid.Internal.Entities qualified as Entity

-- HTML attribute values
--

-- | Escapes a raw attribute value produced as a 'Builder'. The builder is
-- materialized into a small buffer first, since a 'Builder' cannot be
-- post-processed in place.
htmlAttributeBytesBuilder :: Builder -> Builder
htmlAttributeBytesBuilder =
  htmlAttributeLazyBytesBuilder . materialize

htmlAttributeTextBytesBuilder :: T.Text -> Builder
htmlAttributeTextBytesBuilder =
  TE.encodeUtf8BuilderEscaped htmlAttrEscapePrim

htmlAttributeLazyBytesBuilder :: LBS.ByteString -> Builder
htmlAttributeLazyBytesBuilder =
  foldMap (BPrim.primMapByteStringBounded htmlAttrEscapePrim) . LBS.toChunks

htmlAttributeTextBuilder :: TBL.Builder -> TBL.Builder
htmlAttributeTextBuilder =
  escapeTextWith isHTMLAttrSpecial htmlAttrEntity . TBL.runBuilder

htmlAttributeText :: T.Text -> TBL.Builder
htmlAttributeText =
  escapeTextWith isHTMLAttrSpecial htmlAttrEntity

-- XML attribute values
--

xmlAttributeBytesBuilder :: Builder -> Builder
xmlAttributeBytesBuilder =
  xmlAttributeLazyBytesBuilder . materialize

xmlAttributeTextBytesBuilder :: T.Text -> Builder
xmlAttributeTextBytesBuilder =
  TE.encodeUtf8BuilderEscaped xmlAttrEscapePrim

xmlAttributeLazyBytesBuilder :: LBS.ByteString -> Builder
xmlAttributeLazyBytesBuilder =
  foldMap (BPrim.primMapByteStringBounded xmlAttrEscapePrim) . LBS.toChunks

xmlAttributeTextBuilder :: TBL.Builder -> TBL.Builder
xmlAttributeTextBuilder =
  escapeTextWith isXMLAttrSpecial xmlAttrEntity . TBL.runBuilder

xmlAttributeText :: T.Text -> TBL.Builder
xmlAttributeText =
  escapeTextWith isXMLAttrSpecial xmlAttrEntity

-- Attribute names
--

-- | Attribute names cannot be escaped, so names supplied as arbitrary text
-- (custom attributes, @data-*@ suffixes, raw ARIA names) are checked before
-- rendering. A name is accepted when it is non-empty and contains no
-- whitespace, control characters, or any of @\" ' < > / = &@. Those are the
-- characters that could terminate the name, start a value, or close the tag.
-- This is a structural safety check rather than full validation against the
-- HTML or XML name grammar.
isSafeAttributeName :: T.Text -> Bool
isSafeAttributeName name =
  not (T.null name) && T.all isSafeNameChar name

isSafeNameChar :: Char -> Bool
isSafeNameChar c =
  not (isControl c)
    && c /= ' '
    && c /= '"'
    && c /= '\''
    && c /= '<'
    && c /= '>'
    && c /= '/'
    && c /= '='
    && c /= '&'

-- Text content
--

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
escapeBytesBuilder = TE.encodeUtf8BuilderEscaped htmlEscapePrim

escapeTextBuilder :: T.Text -> TBL.Builder
escapeTextBuilder = TBL.fromText . escape

-- URLs
--

urlByteString :: T.Text -> LBS.ByteString
urlByteString = LBS.fromStrict . encodeBS

urlText :: T.Text -> T.Text
urlText = TE.decodeUtf8 . encodeBS

encodeBS :: T.Text -> BS.ByteString
encodeBS = urlEncode False . TE.encodeUtf8

-- Internals
--

-- | Runs a builder into a lazy ByteString. It starts with a small first chunk
-- because attribute values are usually short, which keeps per-attribute
-- allocation low.
materialize :: Builder -> LBS.ByteString
materialize =
  BSBE.toLazyByteStringWith
    (BSBE.untrimmedStrategy 64 BSBE.smallChunkSize)
    LBS.empty

-- | Copies runs of characters that need no escaping directly, and replaces
-- each special character with its entity. When the input has nothing to
-- escape, the result is the input itself.
escapeTextWith :: (Char -> Bool) -> (Char -> T.Text) -> T.Text -> TBL.Builder
escapeTextWith isSpecial entity =
  go
    where
      go txt =
        case T.break isSpecial txt of
          (prefix, rest) ->
            case T.uncons rest of
              Nothing ->
                TBL.fromText prefix

              Just (c, rest') ->
                TBL.fromText prefix <> TBL.fromText (entity c) <> go rest'

isHTMLAttrSpecial :: Char -> Bool
isHTMLAttrSpecial c =
  c == '&' || c == '<' || c == '>' || c == '"' || c == '\''

htmlAttrEntity :: Char -> T.Text
htmlAttrEntity c =
  case c of
    '&'  -> T.pack Entity.ampersand
    '<'  -> T.pack Entity.lessThanSign
    '>'  -> T.pack Entity.greaterThanSign
    '"'  -> T.pack Entity.quotationMark
    '\'' -> T.pack Entity.singleQuote
    _    -> T.singleton c

isXMLAttrSpecial :: Char -> Bool
isXMLAttrSpecial c =
  isHTMLAttrSpecial c || c == '\t' || c == '\n' || c == '\r'

xmlAttrEntity :: Char -> T.Text
xmlAttrEntity c =
  case c of
    '\t' -> "&#9;"
    '\n' -> "&#10;"
    '\r' -> "&#13;"
    _    -> htmlAttrEntity c

fixed4 :: Word8 -> Word8 -> Word8 -> Word8 -> BPrim.FixedPrim Word8
{-# INLINE fixed4 #-}
fixed4 b1 b2 b3 b4 =
  const (b1, (b2, (b3, b4))) >$<
    (BPrim.word8 >*< (BPrim.word8 >*< (BPrim.word8 >*< BPrim.word8)))

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

htmlAttrEscapePrim :: BPrim.BoundedPrim Word8
{-# INLINE htmlAttrEscapePrim #-}
htmlAttrEscapePrim =
  BPrim.condB (== 0x22) (BPrim.liftFixedToBounded $ fixed5 0x26 0x23 0x33 0x34 0x3B) $ -- " → &#34;
  BPrim.condB (== 0x27) (BPrim.liftFixedToBounded $ fixed5 0x26 0x23 0x33 0x39 0x3B) $ -- ' → &#39;
  htmlEscapePrim

xmlAttrEscapePrim :: BPrim.BoundedPrim Word8
{-# INLINE xmlAttrEscapePrim #-}
xmlAttrEscapePrim =
  BPrim.condB (== 0x09) (BPrim.liftFixedToBounded $ fixed4 0x26 0x23 0x39 0x3B) $            -- \t → &#9;
  BPrim.condB (== 0x0A) (BPrim.liftFixedToBounded $ fixed5 0x26 0x23 0x31 0x30 0x3B) $       -- \n → &#10;
  BPrim.condB (== 0x0D) (BPrim.liftFixedToBounded $ fixed5 0x26 0x23 0x31 0x33 0x3B) $       -- \r → &#13;
  htmlAttrEscapePrim
