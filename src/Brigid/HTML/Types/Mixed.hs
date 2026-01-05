module Brigid.HTML.Types.Mixed
  ( Mixed (Mixed)
  , mixedToBytes
  , mixedToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T

data Mixed = Mixed
  deriving (Eq, Show)

mixedToBytes :: Mixed -> LBS.ByteString
mixedToBytes Mixed = LBS8.pack "mixed"

mixedToText :: Mixed -> T.Text
mixedToText Mixed = T.pack "mixed"
