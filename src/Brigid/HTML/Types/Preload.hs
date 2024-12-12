module Brigid.HTML.Types.Preload
  ( Preload
      ( PreloadNone
      , PreloadMetadata
      , PreloadAuto
      )
  , preloadToBytes
  , preloadToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Preload
  = PreloadNone
  | PreloadMetadata
  | PreloadAuto

preloadToBytes :: Preload -> LBS.ByteString
preloadToBytes preload =
  case preload of
    PreloadNone     -> "none"
    PreloadMetadata -> "metadata"
    PreloadAuto     -> "auto"

preloadToText :: Preload -> T.Text
preloadToText preload =
  case preload of
    PreloadNone     -> "none"
    PreloadMetadata -> "metadata"
    PreloadAuto     -> "auto"
