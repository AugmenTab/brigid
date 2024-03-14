module HTML.Types.This
  (This (This)
  , thisToBytes
  , thisToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data This = This

thisToBytes :: This -> LBS.ByteString
thisToBytes = const "this"

thisToText :: This -> T.Text
thisToText = const "this"
