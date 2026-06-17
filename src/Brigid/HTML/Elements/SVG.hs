module Brigid.HTML.Elements.SVG
  ( Tags.Group, g
  , Tags.Path, path
  , Tags.SVG, svg
  ) where

import Brigid.HTML.Attributes.Internal (Attribute)
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Internal (ChildHTML (..))
import Brigid.HTML.Elements.Tags qualified as Tags

g :: ValidChild Tags.Group parent grandparent
  => [Attribute Tags.Group]
  -> [ChildHTML Tags.Group parent]
  -> ChildHTML parent grandparent
g = Tag_Group

path :: ValidChild Tags.Path parent grandparent
     => [Attribute Tags.Path]
     -> [ChildHTML Tags.Path parent]
     -> ChildHTML parent grandparent
path = Tag_Path

svg :: ValidChild Tags.SVG parent grandparent
    => [Attribute Tags.SVG]
    -> [ChildHTML Tags.SVG parent]
    -> ChildHTML parent grandparent
svg = Tag_SVG
