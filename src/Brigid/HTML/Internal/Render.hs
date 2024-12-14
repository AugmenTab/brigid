module Brigid.HTML.Internal.Render
  ( enumBoolToBytes
  , enumBoolToText
  , foldToBytesWithSeparator
  , foldToTextWithSeparator
  , showBytes
  , showText
  ) where

import Data.Bool qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T

enumBoolToBytes :: Bool -> LBS.ByteString
enumBoolToBytes = B.bool "false" "true"

enumBoolToText :: Bool -> T.Text
enumBoolToText = B.bool "false" "true"

foldToBytesWithSeparator :: (a -> LBS.ByteString)
                         -> LBS.ByteString
                         -> [a]
                         -> LBS.ByteString
foldToBytesWithSeparator toBytes separator items =
  case items of
    [] ->
      LBS.empty

    (x:[]) ->
      toBytes x

    (x:xs) ->
      toBytes x <> separator <> foldToBytesWithSeparator toBytes separator xs

foldToTextWithSeparator :: (a -> T.Text) -> T.Text -> [a] -> T.Text
foldToTextWithSeparator toText separator items =
  case items of
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
