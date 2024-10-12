module Brigid.HTML.Attributes
  ( noAttribute
  , customAttribute
  , module Global
  , module HTMX
  , module Scoped
  , module Other
  ) where

import Data.Text qualified as T

import Brigid.HTML.Attributes.Internal (Attribute(..))
import Brigid.HTML.Attributes.Global as Global
import Brigid.HTML.Attributes.HTMX as HTMX
import Brigid.HTML.Attributes.Scoped as Scoped
import Brigid.HTML.Attributes.Other as Other

noAttribute :: Attribute tag
noAttribute = Attr_NoAttribute

customAttribute :: T.Text -> T.Text -> Attribute tag
customAttribute = Attr_Custom