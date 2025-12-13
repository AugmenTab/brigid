module Brigid.HTML.Attributes
  ( Attribute
  , noAttribute
  , customAttribute
  , customBooleanAttribute
  , module Global
  , module Scoped
  , module Aria
  , module Event
  , module HTMX
  , module Other
  ) where

import Data.Text qualified as T

import Brigid.HTML.Attributes.Aria as Aria
import Brigid.HTML.Attributes.Event as Event
import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Attributes.Global as Global
import Brigid.HTML.Attributes.HTMX as HTMX
import Brigid.HTML.Attributes.Scoped as Scoped
import Brigid.HTML.Attributes.Other as Other

noAttribute :: Attribute tag
noAttribute = Attr_NoAttribute

customAttribute :: T.Text -> T.Text -> Attribute tag
customAttribute = Attr_Custom

customBooleanAttribute :: T.Text -> Bool -> Attribute tag
customBooleanAttribute = Attr_CustomBoolean
