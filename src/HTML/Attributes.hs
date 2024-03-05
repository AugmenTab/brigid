module HTML.Attributes
  ( customAttribute
  , module Global
  , module HTMX
  , module Scoped
  ) where

import Data.Text qualified as T

import HTML.Attributes.Internal (Attribute(..))
import HTML.Attributes.Global as Global
import HTML.Attributes.HTMX as HTMX
import HTML.Attributes.Scoped as Scoped

customAttribute :: T.Text -> T.Text -> Attribute tag
customAttribute = Attr_Custom
