{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HXML.Attributes
  ( noAttribute
  , customAttribute
  , hide
  , id
  , safeArea
  , scroll
  , scrollOrientation
  , showsScrollIndicator
  , style
  , styles
  , xmlns
  ) where

import Prelude hiding (id)
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Internal.Render qualified as Render
import Brigid.HXML.Attributes.AttributeType (AttributeType (..))
import Brigid.HXML.Attributes.Internal (Attribute (..))
import Brigid.HXML.Attributes.Elements (ValidAttribute)
import Brigid.HXML.Types qualified as Types

noAttribute :: Attribute tag
noAttribute = Attr_NoAttribute

customAttribute :: T.Text -> T.Text -> Attribute tag
customAttribute = Attr_Custom

hide :: ValidAttribute 'Hide tag => Bool -> Attribute tag
hide = Attr_Hide

id :: ValidAttribute 'Id tag => Types.Id -> Attribute tag
id = Attr_Id

safeArea :: ValidAttribute 'SafeArea tag => Bool -> Attribute tag
safeArea = Attr_SafeArea

scroll :: ValidAttribute 'Scroll tag => Bool -> Attribute tag
scroll = Attr_Scroll

scrollOrientation :: ValidAttribute 'ScrollOrientation tag
                  => Types.ScrollOrientation -> Attribute tag
scrollOrientation = Attr_ScrollOrientation

showsScrollIndicator :: ValidAttribute 'ShowsScrollIndicator tag
                     => Bool -> Attribute tag
showsScrollIndicator = Attr_ShowsScrollIndicator

style :: ValidAttribute 'Style tag => T.Text -> Attribute tag
style = Attr_Style

styles :: ValidAttribute 'Style tag => [T.Text] -> Attribute tag
styles = style . Render.foldToTextWithSeparator (\x -> x) " "

xmlns :: ( KnownNat branchIndex
         , branchIndex ~ FirstIndexOf url Types.URLTypes
         , ValidAttribute 'XMLNS tag
         )
      => url -> Attribute tag
xmlns =
  Attr_XMLNS . Types.mkURL
