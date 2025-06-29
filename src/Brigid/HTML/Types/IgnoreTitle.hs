module Brigid.HTML.Types.IgnoreTitle
  ( IgnoreTitle (IgnoreTitle)
  , ignoreTitleToBytes
  , ignoreTitleToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data IgnoreTitle = IgnoreTitle
  deriving (Eq, Show)

ignoreTitleToBytes :: IgnoreTitle -> LBS.ByteString
ignoreTitleToBytes = const "ignoreTitle:true"

ignoreTitleToText :: IgnoreTitle -> T.Text
ignoreTitleToText = const "ignoreTitle:true"
