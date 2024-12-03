module Brigid.HTML.Render.Internal.Attribute
  ( foldAttrMap
  ) where

import Data.Foldable (foldl')
import Data.Map.Ordered.Strict (OMap)

foldAttrMap :: OMap k v -> [v]
foldAttrMap =
  foldl' (flip (:)) []
