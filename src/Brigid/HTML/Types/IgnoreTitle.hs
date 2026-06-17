module Brigid.HTML.Types.IgnoreTitle
  ( IgnoreTitle (IgnoreTitle)
  , ignoreTitleToBytes
  , ignoreTitleToBytesBuilder
  , ignoreTitleToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data IgnoreTitle = IgnoreTitle
  deriving (Eq, Show)

ignoreTitleToBytes :: IgnoreTitle -> LBS.ByteString
ignoreTitleToBytes = const "ignoreTitle:true"

ignoreTitleToBytesBuilder :: IgnoreTitle -> Builder
{-# INLINE ignoreTitleToBytesBuilder #-}
ignoreTitleToBytesBuilder = const "ignoreTitle:true"

ignoreTitleToText :: IgnoreTitle -> T.Text
ignoreTitleToText = const "ignoreTitle:true"
