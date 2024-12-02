module Brigid.HTML.Elements.Safe.Image
  ( imgMap
  ) where

import Brigid.HTML.Attributes.Internal (Attribute)
import Brigid.HTML.Attributes.Scoped (ismap)
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Internal (ChildHTML (..))
import Brigid.HTML.Elements.Tags qualified as Tags

imgMap :: ValidChild Tags.Image Tags.Anchor grandparent
       => [Attribute Tags.Image] -> ChildHTML Tags.Anchor grandparent
imgMap =
  E.img . (ismap :)
