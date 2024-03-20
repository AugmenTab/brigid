module HTML.Types.None
  ( None (None)
  , noneToBytes
  , noneToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data None = None

noneToBytes :: None -> LBS.ByteString
noneToBytes = const "none"

noneToText :: None -> T.Text
noneToText = const "none"
