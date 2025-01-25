{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Attributes.Internal
  ( Attribute
      ( Attr_NoAttribute
      , Attr_Custom
      , Attr_ActivityIndicatorColor
      , Attr_AdjustsFontSizeToFit
      , Attr_AllowDeselect
      , Attr_AutoFocus
      , Attr_AvoidKeyboard
      , Attr_Color
      , Attr_ContentContainerStyle
      , Attr_CursorColor
      , Attr_Hide
      , Attr_Html
      , Attr_Id
      , Attr_InjectedJavaScript
      , Attr_ItemHeight
      , Attr_Key
      , Attr_KeyboardDismissMode
      , Attr_KeyboardShouldPersistTaps
      , Attr_KeyboardType
      , Attr_Mask
      , Attr_Multiline
      , Attr_Name
      , Attr_NumberOfLines
      , Attr_Placeholder
      , Attr_PlaceholderTextColor
      , Attr_Preformatted
      , Attr_SafeArea
      , Attr_Scroll
      , Attr_ScrollOrientation
      , Attr_ScrollToInputOffset
      , Attr_SecureText
      , Attr_Selectable
      , Attr_SelectionColor
      , Attr_SelectionHandleColor
      , Attr_ShowLoadingIndicator
      , Attr_ShowsScrollIndicator
      , Attr_Source
      , Attr_Sticky
      , Attr_StickySectionTitles
      , Attr_Style
      , Attr_Url
      , Attr_Value
      , Attr_XMLNS
      )
  , attributeText
  ) where

import Data.ByteString.Lazy qualified as LBS
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

  Attr_ActivityIndicatorColor
    :: ValidAttribute 'ActivityIndicatorColor tag
    => Types.Color
    -> Attribute tag

  Attr_AdjustsFontSizeToFit
    :: ValidAttribute 'AdjustsFontSizeToFit tag
    => Bool
    -> Attribute tag

  Attr_AllowDeselect
    :: ValidAttribute 'AllowDeselect tag
    => Bool
    -> Attribute tag

  Attr_AutoFocus
    :: ValidAttribute 'AutoFocus tag
    => Bool
    -> Attribute tag

  Attr_AvoidKeyboard
    :: ValidAttribute 'AvoidKeyboard tag
    => Bool
    -> Attribute tag

  Attr_Color
    :: ValidAttribute 'Color tag
    => Types.HexColor
    -> Attribute tag

  Attr_ContentContainerStyle
    :: ValidAttribute 'ContentContainerStyle tag
    => [Types.Id]
    -> Attribute tag

  Attr_CursorColor
    :: ValidAttribute 'CursorColor tag
    => Types.Color
    -> Attribute tag

  Attr_Hide
    :: ValidAttribute 'Hide tag
    => Bool
    -> Attribute tag

  Attr_Html
    :: ValidAttribute 'Html tag
    => LBS.ByteString
    -> Attribute tag

  Attr_Id
    :: ValidAttribute 'Id tag
    => Types.Id
    -> Attribute tag

  Attr_InjectedJavaScript
    :: ValidAttribute InjectedJavaScript tag
    => Types.RawJavaScript
    -> Attribute tag

  Attr_ItemHeight
    :: ValidAttribute 'ItemHeight tag
    => Positive
    -> Attribute tag

  Attr_Key
    :: ValidAttribute 'Key tag
    => Types.Key
    -> Attribute tag

  Attr_KeyboardDismissMode
    :: ValidAttribute 'KeyboardDismissMode tag
    => Types.KeyboardDismissMode
    -> Attribute tag

  Attr_KeyboardShouldPersistTaps
    :: ValidAttribute 'KeyboardShouldPersistTaps tag
    => Types.KeyboardShouldPersistTaps
    -> Attribute tag

  Attr_KeyboardType
    :: ValidAttribute 'KeyboardType tag
    => Types.KeyboardType
    -> Attribute tag

  Attr_Mask
    :: ValidAttribute 'Mask tag
    => Types.Mask
    -> Attribute tag

  Attr_Multiline
    :: ValidAttribute 'Multiline tag
    => Bool
    -> Attribute tag

  Attr_Name
    :: ValidAttribute 'Name tag
    => Types.Name
    -> Attribute tag

  Attr_NumberOfLines
    :: ValidAttribute 'NumberOfLines tag
    => Positive
    -> Attribute tag

  Attr_Placeholder
    :: ValidAttribute 'Placeholder tag
    => T.Text
    -> Attribute tag

  Attr_PlaceholderTextColor
    :: ValidAttribute 'PlaceholderTextColor tag
    => Types.Color
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

  Attr_SecureText
    :: ValidAttribute 'SecureText tag
    => Bool
    -> Attribute tag

  Attr_Selectable
    :: ValidAttribute 'Selectable tag
    => Bool
    -> Attribute tag

  Attr_SelectionColor
    :: ValidAttribute 'SelectionColor tag
    => Types.Color
    -> Attribute tag

  Attr_SelectionHandleColor
    :: ValidAttribute 'SelectionHandleColor tag
    => Types.Color
    -> Attribute tag

  Attr_ShowLoadingIndicator
    :: ValidAttribute 'ShowLoadingIndicator tag
    => Types.ShowLoadingIndicator
    -> Attribute tag

  Attr_ShowsScrollIndicator
    :: ValidAttribute 'ShowsScrollIndicator tag
    => Bool
    -> Attribute tag

  Attr_Source
    :: ValidAttribute 'Source tag
    => Types.URL
    -> Attribute tag

  Attr_Sticky
    :: ValidAttribute 'Sticky tag
    => Bool
    -> Attribute tag

  Attr_StickySectionTitles
    :: ValidAttribute 'StickySectionTitles tag
    => Bool
    -> Attribute tag

  Attr_Style
    :: ValidAttribute 'Style tag
    => [Types.Id]
    -> Attribute tag

  Attr_Url
    :: ValidAttribute 'Url tag
    => Types.URL
    -> Attribute tag

  Attr_Value
    :: ValidAttribute 'Value tag
    => T.Text
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

    Attr_ActivityIndicatorColor _activityIndicatorColor ->
      "activity-indicator-color"

    Attr_AdjustsFontSizeToFit _adjustsFontSizeToFit ->
      "adjustsFontSizeToFit"

    Attr_AllowDeselect _allowDeselect ->
      "allow-deselect"

    Attr_AutoFocus _autoFocus ->
      "auto-focus"

    Attr_AvoidKeyboard _avoidKeyboard ->
      "avoid-keyboard"

    Attr_Color _color ->
      "color"

    Attr_ContentContainerStyle _contentContainerStyle ->
      "content-container-style"

    Attr_CursorColor _cursorColor ->
      "cursorColor"

    Attr_Hide _hide ->
      "hide"

    Attr_Html _html ->
      "html"

    Attr_Id _id ->
      "id"

    Attr_InjectedJavaScript _injectedJavaScript ->
      "injected-java-script"

    Attr_ItemHeight _itemHeight ->
      "itemHeight"

    Attr_Key _key ->
      "key"

    Attr_KeyboardDismissMode _keyboardDismissMode ->
      "keyboard-dismiss-mode"

    Attr_KeyboardShouldPersistTaps _keyboardShouldPersistTaps ->
      "keyboard-should-persist-taps"

    Attr_KeyboardType _keyboardType ->
      "keyboard-type"

    Attr_Mask _mask ->
      "mask"

    Attr_Multiline _multiline ->
      "multiline"

    Attr_Name _name ->
      "name"

    Attr_NumberOfLines _numberOfLines ->
      "numberOfLines"

    Attr_Placeholder _placeholder ->
      "placeholder"

    Attr_PlaceholderTextColor _placeholderTextColor ->
      "placeholderTextColor"

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

    Attr_SecureText _secureText ->
      "secure-text"

    Attr_Selectable _selectable ->
      "selectable"

    Attr_SelectionColor _selectionColor ->
      "selectionColor"

    Attr_SelectionHandleColor _selectionHandleColor ->
      "selectionHandleColor"

    Attr_ShowLoadingIndicator _showLoadingIndicator ->
      "show-loading-indicator"

    Attr_ShowsScrollIndicator _showsScrollIndicator ->
      "shows-scroll-indicator"

    Attr_Source _source ->
      "source"

    Attr_Sticky _sticky ->
      "sticky"

    Attr_StickySectionTitles _stickySectionTitles ->
      "sticky-section-titles"

    Attr_Style _style ->
      "style"

    Attr_Url _url ->
      "url"

    Attr_Value _value ->
      "value"

    Attr_XMLNS _xmlns ->
      "xmlns"
