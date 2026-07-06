module Brigid.HTML.Types.BlockOption
  ( BlockOption
      ( Render
      )
  , blockOptionToBytes
  , blockOptionToBytesBuilder
  , blockOptionToText
  , blockOptionToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data BlockOption
  = Render
  deriving (Bounded, Enum, Eq, Show)

blockOptionToBytes :: BlockOption -> LBS.ByteString
blockOptionToBytes bo =
  case bo of
    Render -> "render"

blockOptionToBytesBuilder :: BlockOption -> Builder
{-# INLINE blockOptionToBytesBuilder #-}
blockOptionToBytesBuilder = lazyByteString . blockOptionToBytes

blockOptionToText :: BlockOption -> T.Text
blockOptionToText bo =
  case bo of
    Render -> "render"

blockOptionToTextBuilder :: BlockOption -> TBL.Builder
blockOptionToTextBuilder = TBL.fromText . blockOptionToText
