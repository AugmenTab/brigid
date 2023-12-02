{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML.Attributes
  ( ValidAttributeOf
  , unwrapAttribute

  -- Global Attributes
  , id
  ) where

import           Prelude hiding (id)
import           Data.Kind (Type)
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Data.HTML.Elements.Tags as Tags
import           Data.HTML.Types (Elem)

type family ValidElementsFor attribute :: [Type] where
  ValidElementsFor Id = '[Tags.Anchor]

class Attribute attr where
  toAttribute :: attr -> H.Attribute

data ValidAttributeOf element where
  WrapAttribute :: (Attribute attr, Elem element (ValidElementsFor attr))
                => attr -> ValidAttributeOf element

unwrapAttribute :: ValidAttributeOf element -> H.Attribute
unwrapAttribute (WrapAttribute attr) = toAttribute attr

newtype Id = Id T.Text

instance Attribute Id where
  toAttribute (Id idText) = A.id $ H.toValue idText

{-| The id global attribute defines an identifier (ID) which must be unique in
   the whole document. Its purpose is to identify the element when linking
   (using a fragment identifier), scripting, or styling (with CSS).
-}
id :: Elem element (ValidElementsFor Id) => T.Text -> ValidAttributeOf element
id = WrapAttribute . Id -- TODO: Escape the provided id string.
