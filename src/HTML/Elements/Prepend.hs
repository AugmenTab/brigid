module HTML.Elements.Prepend
  ( prependDivision
  ) where

import HTML.Elements.Children (ValidChild)
import HTML.Elements.Internal (ChildHTML(..))
import HTML.Elements.Tags qualified as Tags

prependDivision :: ValidChild Tags.Division parent
                => ChildHTML Tags.Division
                -> ChildHTML parent
                -> ChildHTML parent
prependDivision child tag =
  case tag of
    Tag_Division attrs content ->
      Tag_Division attrs $ child : content

    _ ->
      tag
