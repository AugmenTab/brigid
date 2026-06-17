module Brigid.HTML.Types.Preload
  ( Preload
      ( PreloadNone
      , PreloadMetadata
      , PreloadAuto
      )
  , preloadToBytes
  , preloadToBytesBuilder
  , preloadToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Preload
  = PreloadNone
  | PreloadMetadata
  | PreloadAuto
  deriving (Bounded, Enum, Eq, Show)

preloadToBytes :: Preload -> LBS.ByteString
preloadToBytes preload =
  case preload of
    PreloadNone     -> "none"
    PreloadMetadata -> "metadata"
    PreloadAuto     -> "auto"

preloadToBytesBuilder :: Preload -> Builder
{-# INLINE preloadToBytesBuilder #-}
preloadToBytesBuilder preload =
  case preload of
    PreloadNone     -> string8 "none"
    PreloadMetadata -> string8 "metadata"
    PreloadAuto     -> string8 "auto"

preloadToText :: Preload -> T.Text
preloadToText preload =
  case preload of
    PreloadNone     -> "none"
    PreloadMetadata -> "metadata"
    PreloadAuto     -> "auto"
