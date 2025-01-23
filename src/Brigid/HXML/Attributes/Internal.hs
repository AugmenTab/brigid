{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Attributes.Internal
  ( Attribute
      ( Attr_NoAttribute
      , Attr_Custom
      , Attr_AdjustsFontSizeToFit
      , Attr_AvoidKeyboard
      , Attr_ContentContainerStyle
      , Attr_Hide
      , Attr_Id
      , Attr_KeyboardDismissMode
      , Attr_NumberOfLines
      , Attr_Preformatted
      , Attr_SafeArea
      , Attr_Scroll
      , Attr_ScrollOrientation
      , Attr_ScrollToInputOffset
      , Attr_Selectable
      , Attr_ShowsScrollIndicator
      , Attr_Sticky
      , Attr_Style
      , Attr_XMLNS
      )
  , attributeText
  ) where

import Data.Text qualified as T
import Integer (Positive)

import Brigid.HXML.Attributes.AttributeType (AttributeType (..))
import Brigid.HXML.Attributes.Elements (ValidAttribute)
import Brigid.HXML.Elements.TagType (TagType)
import Brigid.HXML.Types qualified as Types

data Attribute (tag :: TagType) where
  Attr_NoAttribute
    :: Attribute tag

  Attr_Custom
    :: T.Text
    -> T.Text
    -> Attribute tag

  Attr_AdjustsFontSizeToFit
    :: ValidAttribute 'AdjustsFontSizeToFit tag
    => Bool
    -> Attribute tag

  Attr_AvoidKeyboard
    :: ValidAttribute 'AvoidKeyboard tag
    => Bool
    -> Attribute tag

  Attr_ContentContainerStyle
    :: ValidAttribute 'ContentContainerStyle tag
    => [Types.Id]
    -> Attribute tag

  Attr_Hide
    :: ValidAttribute 'Hide tag
    => Bool
    -> Attribute tag

  Attr_Id
    :: ValidAttribute 'Id tag
    => Types.Id
    -> Attribute tag

  Attr_KeyboardDismissMode
    :: ValidAttribute 'KeyboardDismissMode tag
    => Types.KeyboardDismissMode
    -> Attribute tag

  Attr_NumberOfLines
    :: ValidAttribute 'NumberOfLines tag
    => Positive
    -> Attribute tag

  Attr_Preformatted
    :: ValidAttribute 'Preformatted tag
    => Bool
    -> Attribute tag

  Attr_SafeArea
    :: ValidAttribute 'SafeArea tag
    => Bool
    -> Attribute tag

  Attr_Scroll
    :: ValidAttribute 'Scroll tag
    => Bool
    -> Attribute tag

  Attr_ScrollOrientation
    :: ValidAttribute 'ScrollOrientation tag
    => Types.ScrollOrientation
    -> Attribute tag

  Attr_ScrollToInputOffset
    :: ValidAttribute 'ScrollToInputOffset tag
    => Integer
    -> Attribute tag

  Attr_Selectable
    :: ValidAttribute 'Selectable tag
    => Bool
    -> Attribute tag

  Attr_ShowsScrollIndicator
    :: ValidAttribute 'ShowsScrollIndicator tag
    => Bool
    -> Attribute tag

  Attr_Sticky
    :: ValidAttribute 'Sticky tag
    => Bool
    -> Attribute tag

  Attr_Style
    :: ValidAttribute 'Style tag
    => [Types.Id]
    -> Attribute tag

  Attr_XMLNS
    :: ValidAttribute 'XMLNS tag
    => Types.URL
    -> Attribute tag

attributeText :: Attribute tag -> T.Text
attributeText attr =
  case attr of
    Attr_NoAttribute ->
      "no_attribute"

    Attr_Custom name _value ->
      name

    Attr_AdjustsFontSizeToFit _adjustsFontSizeToFit ->
      "adjustsFontSizeToFit"

    Attr_AvoidKeyboard _avoidKeyboard ->
      "avoid-keyboard"

    Attr_ContentContainerStyle _contentContainerStyle ->
      "content-container-style"

    Attr_Hide _hide ->
      "hide"

    Attr_Id _id ->
      "id"

    Attr_KeyboardDismissMode _keyboardDismissMode ->
      "keyboard-dismiss-mode"

    Attr_NumberOfLines _numberOfLines ->
      "numberOfLines"

    Attr_Preformatted _preformatted ->
      "preformatted"

    Attr_SafeArea _safeArea ->
      "safe-area"

    Attr_Scroll _scroll ->
      "scroll"

    Attr_ScrollOrientation _scrollOrientation ->
      "scroll-orientation"

    Attr_ScrollToInputOffset _scrollToInputOffset ->
      "scroll-to-input-offset"

    Attr_Selectable _selectable ->
      "selectable"

    Attr_ShowsScrollIndicator _showsScrollIndicator ->
      "shows-scroll-indicator"

    Attr_Sticky _sticky ->
      "sticky"

    Attr_Style _style ->
      "style"

    Attr_XMLNS _xmlns ->
      "xmlns"
