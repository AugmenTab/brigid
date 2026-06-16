module Brigid.Internal.Render
  ( bytesToLazyBytes
  , lazyBytesToBytes
  , bytesToText
  , lazyBytesToText
  , enumBoolToBytes
  , enumBoolToBytesBuilder
  , enumBoolToText
  , foldToBytesWithSeparator
  , foldToBytesBuilderWithSeparator
  , foldToTextWithSeparator
  , showBytes
  , showBytesBuilder
  , showIntegerBytesBuilder
  , showText
  , textToBytes
  , textToLazyBytes
  , textToBytesBuilder
  ) where

import Data.Bool qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, stringUtf8, toLazyByteString)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable qualified as Foldable
import Data.List qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

bytesToLazyBytes :: BS.ByteString -> LBS.ByteString
bytesToLazyBytes = LBS.fromStrict

lazyBytesToBytes :: LBS.ByteString -> BS.ByteString
lazyBytesToBytes = LBS.toStrict

bytesToText :: BS.ByteString -> T.Text
bytesToText = TE.decodeUtf8

lazyBytesToText :: LBS.ByteString -> T.Text
lazyBytesToText = bytesToText . lazyBytesToBytes

enumBoolToBytes :: Bool -> LBS.ByteString
enumBoolToBytes = toLazyByteString . enumBoolToBytesBuilder

enumBoolToBytesBuilder :: Bool -> Builder
enumBoolToBytesBuilder = B.bool "false" "true"

enumBoolToText :: Bool -> T.Text
enumBoolToText = B.bool "false" "true"

foldToBytesWithSeparator :: Foldable f
                         => (a -> LBS.ByteString)
                         -> LBS.ByteString
                         -> f a
                         -> LBS.ByteString
foldToBytesWithSeparator f sep =
  toLazyByteString
    . foldToBytesBuilderWithSeparator (BSB.lazyByteString . f) (BSB.lazyByteString sep)

foldToBytesBuilderWithSeparator :: Foldable f
                                => (a -> Builder)
                                -> Builder
                                -> f a
                                -> Builder
foldToBytesBuilderWithSeparator f sep items =
  case Foldable.toList items of
    []     -> mempty
    (x:xs) -> L.foldl' (\acc y -> acc <> sep <> f y) (f x) xs

foldToTextWithSeparator :: Foldable f
                        => (a -> T.Text) -> T.Text -> f a -> T.Text
foldToTextWithSeparator toText separator items =
  case Foldable.toList items of
    [] ->
      T.empty

    (x:[]) ->
      toText x

    (x:xs) ->
      toText x <> separator <> foldToTextWithSeparator toText separator xs

showBytes :: Show s => s -> LBS.ByteString
showBytes = toLazyByteString . showBytesBuilder

showBytesBuilder :: Show s => s -> Builder
showBytesBuilder = stringUtf8 . show

showIntegerBytesBuilder :: Integral a => a -> Builder
showIntegerBytesBuilder = BSB.integerDec . toInteger

showText :: Show s => s -> T.Text
showText = T.pack . show

textToBytes :: T.Text -> BS.ByteString
textToBytes = TE.encodeUtf8

textToLazyBytes :: T.Text -> LBS.ByteString
textToLazyBytes = toLazyByteString . textToBytesBuilder

textToBytesBuilder :: T.Text -> Builder
textToBytesBuilder = TE.encodeUtf8Builder
