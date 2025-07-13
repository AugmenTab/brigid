module Brigid.HTML.Elements.SVG
  ( Tags.Group, g
  , Tags.Path, path
  , Tags.SVG, svg
  ) where

import Data.Containers.ListUtils (nubOrdOn)

import Brigid.HTML.Attributes.Internal (Attribute, attributeText)
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Internal (ChildHTML (..))
import Brigid.HTML.Elements.Tags qualified as Tags

g :: ValidChild Tags.Group parent grandparent
  => [Attribute Tags.Group]
  -> [ChildHTML Tags.Group parent]
  -> ChildHTML parent grandparent
g = Tag_Group . nubOrdOn attributeText

path :: ValidChild Tags.Path parent grandparent
     => [Attribute Tags.Path]
     -> [ChildHTML Tags.Path parent]
     -> ChildHTML parent grandparent
path = Tag_Path . nubOrdOn attributeText

svg :: ValidChild Tags.SVG parent grandparent
    => [Attribute Tags.SVG]
    -> [ChildHTML Tags.SVG parent]
    -> ChildHTML parent grandparent
svg = Tag_SVG . nubOrdOn attributeText
