{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML.Attributes
  ( ValidAttributeOf
  , unwrapAttribute

  -- Global Attributes
  , id
  ) where

import Prelude hiding (id)
import Data.Kind (Type)
import Data.Text qualified as T
import Text.Blaze qualified as Blaze
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

import Data.HTML.Elements.Tags qualified as Tags
import Data.HTML.Types (Elem)

type family ValidElementsFor attribute :: [Type] where
  ValidElementsFor Id = '[Tags.Anchor]

class Attribute attr where
  toAttribute :: attr -> H.Attribute

type IsValidAttribute element attr =
  Elem element (ValidElementsFor attr)

data ValidAttributeOf element where
  WrapAttribute :: (Attribute attr, IsValidAttribute element attr)
                => attr -> ValidAttributeOf element

unwrapAttribute :: ValidAttributeOf element -> H.Attribute
unwrapAttribute (WrapAttribute attr) = toAttribute attr

newtype Id = Id T.Text

instance Attribute Id where
  toAttribute (Id idText) = A.id $ Blaze.textValue idText

{-| The id global attribute defines an identifier (ID) which must be unique in
   the whole document. Its purpose is to identify the element when linking
   (using a fragment identifier), scripting, or styling (with CSS).
-}
id :: IsValidAttribute element Id => T.Text -> ValidAttributeOf element
id = WrapAttribute . Id -- TODO: Escape the provided id string.

-- class_ :: IsValidAttribute element Class => T.Text -> ValidAttributeOf element
-- class_ = WrapAttribute . Class

-- classes :: IsValidAttribute element Class
--         => [T.Text] -> ValidAttributeOf element
-- classes = class_ . T.unwords
