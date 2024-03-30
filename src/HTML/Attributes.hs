module HTML.Attributes
  ( customAttribute
  , module Global
  , module HTMX
  , module Scoped
  , module Other
  ) where

import Data.Text qualified as T

import HTML.Attributes.Internal (Attribute(..))
import HTML.Attributes.Global as Global
import HTML.Attributes.HTMX as HTMX
import HTML.Attributes.Scoped as Scoped
import HTML.Attributes.Other as Other

customAttribute :: T.Text -> T.Text -> Attribute tag
customAttribute = Attr_Custom
