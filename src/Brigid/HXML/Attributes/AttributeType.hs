{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Attributes.AttributeType
  ( AttributeErrorMessage
  , AttributeType
      ( ActivityIndicatorColor
      , AdjustsFontSizeToFit
      , AllowDeselect
      , AutoFocus
      , AvoidKeyboard
      , CancelLabel
      , Color
      , ContentContainerStyle
      , CursorColor
      , DoneLabel
      , FieldStyle
      , FieldTextStyle
      , Focused
      , Hide
      , Html
      , Id
      , InjectedJavaScript
      , ItemHeight
      , Key
      , KeyboardDismissMode
      , KeyboardShouldPersistTaps
      , KeyboardType
      , Label
      , Mask
      , Merge
      , Modal
      , ModalStyle
      , ModalTextStyle
      , Multiline
      , Name
      , NumberOfLines
      , Placeholder
      , PlaceholderTextColor
      , Preformatted
      , Pressed
      , SafeArea
      , Scroll
      , ScrollOrientation
      , ScrollToInputOffset
      , SecureText
      , Selectable
      , Selected
      , SelectionColor
      , SelectionHandleColor
      , ShowLoadingIndicator
      , ShowsScrollIndicator
      , Source
      , Sticky
      , StickySectionTitles
      , Style
      , Type
      , Url
      , Value
      , XMLNS
      )
  ) where

import GHC.TypeLits (ErrorMessage (..))

data AttributeType
  = ActivityIndicatorColor
  | AdjustsFontSizeToFit
  | AllowDeselect
  | AutoFocus
  | AvoidKeyboard
  | CancelLabel
  | Color
  | ContentContainerStyle
  | CursorColor
  | DoneLabel
  | FieldStyle
  | FieldTextStyle
  | Focused
  | Hide
  | Html
  | Id
  | InjectedJavaScript
  | ItemHeight
  | Key
  | KeyboardDismissMode
  | KeyboardShouldPersistTaps
  | KeyboardType
  | Label
  | Mask
  | Merge
  | Modal
  | ModalStyle
  | ModalTextStyle
  | Multiline
  | Name
  | NumberOfLines
  | Placeholder
  | PlaceholderTextColor
  | Preformatted
  | Pressed
  | SafeArea
  | Scroll
  | ScrollOrientation
  | ScrollToInputOffset
  | SecureText
  | Selectable
  | Selected
  | SelectionColor
  | SelectionHandleColor
  | ShowLoadingIndicator
  | ShowsScrollIndicator
  | Source
  | Sticky
  | StickySectionTitles
  | Style
  | Type
  | Url
  | Value
  | XMLNS

type family AttributeErrorMessage (attr :: AttributeType) :: ErrorMessage where
  AttributeErrorMessage 'ActivityIndicatorColor    = 'Text "ActivityIndicatorColor (activity-indicator-color)"
  AttributeErrorMessage 'AdjustsFontSizeToFit      = 'Text "AdjustsFontSizeToFit (adjustsFontSizeToFit)"
  AttributeErrorMessage 'AllowDeselect             = 'Text "AllowDeselect (allow-deselect)"
  AttributeErrorMessage 'AutoFocus                 = 'Text "AutoFocus (auto-focus)"
  AttributeErrorMessage 'AvoidKeyboard             = 'Text "AvoidKeyboard (avoid-keyboard)"
  AttributeErrorMessage 'CancelLabel               = 'Text "CancelLabel (cancel-label)"
  AttributeErrorMessage 'Color                     = 'Text "Color (color)"
  AttributeErrorMessage 'ContentContainerStyle     = 'Text "ContentContainerStyle (content-container-style)"
  AttributeErrorMessage 'CursorColor               = 'Text "CursorColor (cursorColor)"
  AttributeErrorMessage 'DoneLabel                 = 'Text "DoneLabel (done-label)"
  AttributeErrorMessage 'FieldStyle                = 'Text "FieldStyle (field-style)"
  AttributeErrorMessage 'FieldTextStyle            = 'Text "FieldTextStyle (field-text-style)"
  AttributeErrorMessage 'Focused                   = 'Text "Focused (focused)"
  AttributeErrorMessage 'Hide                      = 'Text "Hide (hide)"
  AttributeErrorMessage 'Html                      = 'Text "Html (html)"
  AttributeErrorMessage 'Id                        = 'Text "Id (id)"
  AttributeErrorMessage 'InjectedJavaScript        = 'Text "InjectedJavaScript (injected-java-script)"
  AttributeErrorMessage 'ItemHeight                = 'Text "ItemHeight (itemHeight)"
  AttributeErrorMessage 'Key                       = 'Text "Key (key)"
  AttributeErrorMessage 'KeyboardDismissMode       = 'Text "KeyboardDismissMode (keyboard-dismiss-mode)"
  AttributeErrorMessage 'KeyboardShouldPersistTaps = 'Text "KeyboardShouldPersistTaps (keyboard-should-persist-taps)"
  AttributeErrorMessage 'KeyboardType              = 'Text "KeyboardType (keyboard-type)"
  AttributeErrorMessage 'Label                     = 'Text "Label (label)"
  AttributeErrorMessage 'Mask                      = 'Text "Mask (mask)"
  AttributeErrorMessage 'Merge                     = 'Text "Merge (merge)"
  AttributeErrorMessage 'Modal                     = 'Text "Modal (modal)"
  AttributeErrorMessage 'ModalStyle                = 'Text "ModalStyle (modal-style)"
  AttributeErrorMessage 'ModalTextStyle            = 'Text "ModalTextStyle (modal-text-style)"
  AttributeErrorMessage 'Multiline                 = 'Text "Multiline (multiline)"
  AttributeErrorMessage 'Name                      = 'Text "Name (name)"
  AttributeErrorMessage 'NumberOfLines             = 'Text "NumberOfLines (numberOfLines)"
  AttributeErrorMessage 'Placeholder               = 'Text "Placeholder (placeholder)"
  AttributeErrorMessage 'PlaceholderTextColor      = 'Text "PlaceholderTextColor (placeholderTextColor)"
  AttributeErrorMessage 'Preformatted              = 'Text "Preformatted (preformatted)"
  AttributeErrorMessage 'Pressed                   = 'Text "Pressed (pressed)"
  AttributeErrorMessage 'SafeArea                  = 'Text "SafeArea (safe-area)"
  AttributeErrorMessage 'Scroll                    = 'Text "Scroll (scroll)"
  AttributeErrorMessage 'ScrollOrientation         = 'Text "ScrollOrientation (scroll-orientation)"
  AttributeErrorMessage 'ScrollToInputOffset       = 'Text "ScrollToInputOffset (scroll-to-input-offset)"
  AttributeErrorMessage 'SecureText                = 'Text "SecureText (secure-text)"
  AttributeErrorMessage 'Selectable                = 'Text "Selectable (selectable)"
  AttributeErrorMessage 'Selected                  = 'Text "Selected (selected)"
  AttributeErrorMessage 'SelectionColor            = 'Text "SelectionColor (selectionColor)"
  AttributeErrorMessage 'SelectionHandleColor      = 'Text "SelectionHandleColor (selectionHandleColor)"
  AttributeErrorMessage 'ShowLoadingIndicator      = 'Text "ShowLoadingIndicator (show-loading-indicator)"
  AttributeErrorMessage 'ShowsScrollIndicator      = 'Text "ShowsScrollIndicator (shows-scroll-indicator)"
  AttributeErrorMessage 'Source                    = 'Text "Source (source)"
  AttributeErrorMessage 'Sticky                    = 'Text "Sticky (sticky)"
  AttributeErrorMessage 'StickySectionTitles       = 'Text "StickySectionTitles (sticky-section-titles)"
  AttributeErrorMessage 'Style                     = 'Text "Style (style)"
  AttributeErrorMessage 'Type                      = 'Text "Type (type)"
  AttributeErrorMessage 'Url                       = 'Text "Url (url)"
  AttributeErrorMessage 'Value                     = 'Text "Value (value)"
  AttributeErrorMessage 'XMLNS                     = 'Text "XMLNS (xmlns)"
