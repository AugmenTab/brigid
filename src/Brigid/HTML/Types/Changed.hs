module Brigid.HTML.Types.Changed
  ( Changed (Changed)
  , changedToBytes
  , changedToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Changed = Changed
  deriving (Eq, Show)

changedToBytes :: Changed -> LBS.ByteString
changedToBytes = const "changed"

changedToText :: Changed -> T.Text
changedToText = const "changed"
