{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HXML.Attributes
  ( noAttribute
  , customAttribute
  , avoidKeyboard
  , contentContainerStyle
  , hide
  , id
  , keyboardDismissMode
  , safeArea
  , scroll
  , scrollOrientation
  , scrollToInputOffset
  , showsScrollIndicator
  , sticky
  , style
  , styles
  , xmlns
  ) where

import Prelude hiding (id)
import Data.List qualified as L
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HXML.Attributes.AttributeType (AttributeType (..))
import Brigid.HXML.Attributes.Internal (Attribute (..))
import Brigid.HXML.Attributes.Elements (ValidAttribute)
import Brigid.HXML.Types qualified as Types

noAttribute :: Attribute tag
noAttribute = Attr_NoAttribute

customAttribute :: T.Text -> T.Text -> Attribute tag
customAttribute = Attr_Custom

avoidKeyboard :: ValidAttribute 'AvoidKeyboard tag => Bool -> Attribute tag
avoidKeyboard = Attr_AvoidKeyboard

contentContainerStyle :: ValidAttribute 'ContentContainerStyle tag
                      => [Types.Id] -> Attribute tag
contentContainerStyle = Attr_ContentContainerStyle

hide :: ValidAttribute 'Hide tag => Bool -> Attribute tag
hide = Attr_Hide

id :: ValidAttribute 'Id tag => Types.Id -> Attribute tag
id = Attr_Id

keyboardDismissMode :: ValidAttribute 'KeyboardDismissMode tag
                    => Types.KeyboardDismissMode -> Attribute tag
keyboardDismissMode = Attr_KeyboardDismissMode

safeArea :: ValidAttribute 'SafeArea tag => Bool -> Attribute tag
safeArea = Attr_SafeArea

scroll :: ValidAttribute 'Scroll tag => Bool -> Attribute tag
scroll = Attr_Scroll

scrollOrientation :: ValidAttribute 'ScrollOrientation tag
                  => Types.ScrollOrientation -> Attribute tag
scrollOrientation = Attr_ScrollOrientation

scrollToInputOffset :: ValidAttribute 'ScrollToInputOffset tag
                    => Integer -> Attribute tag
scrollToInputOffset = Attr_ScrollToInputOffset

showsScrollIndicator :: ValidAttribute 'ShowsScrollIndicator tag
                     => Bool -> Attribute tag
showsScrollIndicator = Attr_ShowsScrollIndicator

sticky :: ValidAttribute 'Sticky tag => Bool -> Attribute tag
sticky = Attr_Sticky

style :: ValidAttribute 'Style tag => Types.Id -> Attribute tag
style = styles . L.singleton

styles :: ValidAttribute 'Style tag => [Types.Id] -> Attribute tag
styles = Attr_Style

xmlns :: ( KnownNat branchIndex
         , branchIndex ~ FirstIndexOf url Types.URLTypes
         , ValidAttribute 'XMLNS tag
         )
      => url -> Attribute tag
xmlns =
  Attr_XMLNS . Types.mkURL
