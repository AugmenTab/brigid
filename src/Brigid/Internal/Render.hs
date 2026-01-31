module Brigid.Internal.Render
  ( bytesToLazyBytes
  , lazyBytesToBytes
  , bytesToText
  , lazyBytesToText
  , enumBoolToBytes
  , enumBoolToText
  , foldToBytesWithSeparator
  , foldToTextWithSeparator
  , showBytes
  , showText
  , textToBytes
  , textToLazyBytes
  ) where

import Data.Bool qualified as B
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Foldable qualified as Foldable
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
enumBoolToBytes = B.bool "false" "true"

enumBoolToText :: Bool -> T.Text
enumBoolToText = B.bool "false" "true"

foldToBytesWithSeparator :: Foldable f
                         => (a -> LBS.ByteString)
                         -> LBS.ByteString
                         -> f a
                         -> LBS.ByteString
foldToBytesWithSeparator toBytes separator items =
  case Foldable.toList items of
    [] ->
      LBS.empty

    (x:[]) ->
      toBytes x

    (x:xs) ->
      toBytes x <> separator <> foldToBytesWithSeparator toBytes separator xs

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
showBytes = LBS8.pack . show

showText :: Show s => s -> T.Text
showText = T.pack . show

textToBytes :: T.Text -> BS.ByteString
textToBytes = TE.encodeUtf8

textToLazyBytes :: T.Text -> LBS.ByteString
textToLazyBytes = bytesToLazyBytes . textToBytes
