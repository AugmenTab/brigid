module HTML.Elements.Append
  ( appendDivision
  ) where

import HTML.Elements.Children (ValidChild)
import HTML.Elements.Internal (ChildHTML(..))
import HTML.Elements.Tags qualified as Tags

appendDivision :: ValidChild Tags.Division parent
               => ChildHTML Tags.Division
               -> ChildHTML parent
               -> ChildHTML parent
appendDivision child tag =
  case tag of
    Tag_Division attrs content ->
      Tag_Division attrs $ content <> [ child ]

    _ ->
      tag

