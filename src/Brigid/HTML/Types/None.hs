module Brigid.HTML.Types.None
  ( None (None)
  , noneToBytes
  , noneToBytesBuilder
  , noneToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data None = None
  deriving (Eq, Show)

noneToBytes :: None -> LBS.ByteString
noneToBytes = const "none"

noneToBytesBuilder :: None -> Builder
noneToBytesBuilder = const "none"

noneToText :: None -> T.Text
noneToText = const "none"
