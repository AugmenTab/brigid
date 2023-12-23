module HTML.Elements.Intersperse
  ( intersperseDivision
  ) where

import Data.List qualified as L

import HTML.Elements.Children (ValidChild)
import HTML.Elements.Internal (ChildHTML(..))
import HTML.Elements.Tags qualified as Tags

intersperseDivision :: ValidChild Tags.Division parent
                    => ChildHTML Tags.Division
                    -> ChildHTML parent
                    -> ChildHTML parent
intersperseDivision child tag =
  case tag of
    Tag_Division attrs content ->
      Tag_Division attrs $ L.intersperse child content

    _ ->
      tag
