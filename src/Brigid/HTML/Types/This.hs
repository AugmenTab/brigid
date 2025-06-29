module Brigid.HTML.Types.This
  (This (This)
  , thisToBytes
  , thisToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data This = This
  deriving (Eq, Show)

thisToBytes :: This -> LBS.ByteString
thisToBytes = const "this"

thisToText :: This -> T.Text
thisToText = const "this"
