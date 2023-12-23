module HTML.Elements.AddAttribute
  ( addDivisionAttribute
  ) where

import Data.Map qualified as Map

import HTML.Attributes.Internal (Attribute, attributeText)
import HTML.Elements.Internal (ChildHTML(..))
import HTML.Elements.Tags qualified as Tags

addDivisionAttribute :: ChildHTML parent
                     -> Attribute Tags.Division
                     -> ChildHTML parent
addDivisionAttribute tag attr =
  case tag of
    Tag_Division attrs content ->
      Tag_Division (Map.insert (attributeText attr) attr attrs) content

    _ ->
      tag
