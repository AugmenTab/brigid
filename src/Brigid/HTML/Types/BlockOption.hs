module Brigid.HTML.Types.BlockOption
  ( BlockOption
      ( Render
      )
  , blockOptionToBytes
  , blockOptionToBytesBuilder
  , blockOptionToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data BlockOption
  = Render
  deriving (Bounded, Enum, Eq, Show)

blockOptionToBytes :: BlockOption -> LBS.ByteString
blockOptionToBytes bo =
  case bo of
    Render -> "render"

blockOptionToBytesBuilder :: BlockOption -> Builder
blockOptionToBytesBuilder bo =
  case bo of
    Render -> string8 "render"

blockOptionToText :: BlockOption -> T.Text
blockOptionToText bo =
  case bo of
    Render -> "render"
