{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Attributes.AttributeType
  ( AttributeErrorMessage
  , AttributeType
      ( ActivityIndicatorColor
      , AdjustsFontSizeToFit
      , AutoFocus
      , AvoidKeyboard
      , Color
      , ContentContainerStyle
      , CursorColor
      , Hide
      , Html
      , Id
      , InjectedJavaScript
      , ItemHeight
      , Key
      , KeyboardDismissMode
      , KeyboardShouldPersistTaps
      , KeyboardType
      , Mask
      , Multiline
      , Name
      , NumberOfLines
      , Placeholder
      , PlaceholderTextColor
      , Preformatted
      , SafeArea
      , Scroll
      , ScrollOrientation
      , ScrollToInputOffset
      , SecureText
      , Selectable
      , SelectionColor
      , SelectionHandleColor
      , ShowLoadingIndicator
      , ShowsScrollIndicator
      , Source
      , Sticky
      , StickySectionTitles
      , Style
      , Url
      , Value
      , XMLNS
      )
  ) where

import GHC.TypeLits (ErrorMessage (..))

data AttributeType
  = ActivityIndicatorColor
  | AdjustsFontSizeToFit
  | AutoFocus
  | AvoidKeyboard
  | Color
  | ContentContainerStyle
  | CursorColor
  | Hide
  | Html
  | Id
  | InjectedJavaScript
  | ItemHeight
  | Key
  | KeyboardDismissMode
  | KeyboardShouldPersistTaps
  | KeyboardType
  | Mask
  | Multiline
  | Name
  | NumberOfLines
  | Placeholder
  | PlaceholderTextColor
  | Preformatted
  | SafeArea
  | Scroll
  | ScrollOrientation
  | ScrollToInputOffset
  | SecureText
  | Selectable
  | SelectionColor
  | SelectionHandleColor
  | ShowLoadingIndicator
  | ShowsScrollIndicator
  | Source
  | Sticky
  | StickySectionTitles
  | Style
  | Url
  | Value
  | XMLNS

type family AttributeErrorMessage (attr :: AttributeType) :: ErrorMessage where
  AttributeErrorMessage 'ActivityIndicatorColor    = 'Text "ActivityIndicatorColor (activity-indicator-color)"
  AttributeErrorMessage 'AdjustsFontSizeToFit      = 'Text "AdjustsFontSizeToFit (adjustsFontSizeToFit)"
  AttributeErrorMessage 'AutoFocus                 = 'Text "AutoFocus (auto-focus)"
  AttributeErrorMessage 'AvoidKeyboard             = 'Text "AvoidKeyboard (avoid-keyboard)"
  AttributeErrorMessage 'Color                     = 'Text "Color (color)"
  AttributeErrorMessage 'ContentContainerStyle     = 'Text "ContentContainerStyle (content-container-style)"
  AttributeErrorMessage 'CursorColor               = 'Text "CursorColor (cursorColor)"
  AttributeErrorMessage 'Hide                      = 'Text "Hide (hide)"
  AttributeErrorMessage 'Html                      = 'Text "Html (html)"
  AttributeErrorMessage 'Id                        = 'Text "Id (id)"
  AttributeErrorMessage 'InjectedJavaScript        = 'Text "InjectedJavaScript (injected-java-script)"
  AttributeErrorMessage 'ItemHeight                = 'Text "ItemHeight (itemHeight)"
  AttributeErrorMessage 'Key                       = 'Text "Key (key)"
  AttributeErrorMessage 'KeyboardDismissMode       = 'Text "KeyboardDismissMode (keyboard-dismiss-mode)"
  AttributeErrorMessage 'KeyboardShouldPersistTaps = 'Text "KeyboardShouldPersistTaps (keyboard-should-persist-taps)"
  AttributeErrorMessage 'KeyboardType              = 'Text "KeyboardType (keyboard-type)"
  AttributeErrorMessage 'Mask                      = 'Text "Mask (mask)"
  AttributeErrorMessage 'Multiline                 = 'Text "Multiline (multiline)"
  AttributeErrorMessage 'Name                      = 'Text "Name (name)"
  AttributeErrorMessage 'NumberOfLines             = 'Text "NumberOfLines (numberOfLines)"
  AttributeErrorMessage 'Placeholder               = 'Text "Placeholder (placeholder)"
  AttributeErrorMessage 'PlaceholderTextColor      = 'Text "PlaceholderTextColor (placeholderTextColor)"
  AttributeErrorMessage 'Preformatted              = 'Text "Preformatted (preformatted)"
  AttributeErrorMessage 'SafeArea                  = 'Text "SafeArea (safe-area)"
  AttributeErrorMessage 'Scroll                    = 'Text "Scroll (scroll)"
  AttributeErrorMessage 'ScrollOrientation         = 'Text "ScrollOrientation (scroll-orientation)"
  AttributeErrorMessage 'ScrollToInputOffset       = 'Text "ScrollToInputOffset (scroll-to-input-offset)"
  AttributeErrorMessage 'SecureText                = 'Text "SecureText (secure-text)"
  AttributeErrorMessage 'SelectionColor            = 'Text "SelectionColor (selectionColor)"
  AttributeErrorMessage 'SelectionHandleColor      = 'Text "SelectionHandleColor (selectionHandleColor)"
  AttributeErrorMessage 'Selectable                = 'Text "Selectable (selectable)"
  AttributeErrorMessage 'ShowLoadingIndicator      = 'Text "ShowLoadingIndicator (show-loading-indicator)"
  AttributeErrorMessage 'ShowsScrollIndicator      = 'Text "ShowsScrollIndicator (shows-scroll-indicator)"
  AttributeErrorMessage 'Source                    = 'Text "Source (source)"
  AttributeErrorMessage 'Sticky                    = 'Text "Sticky (sticky)"
  AttributeErrorMessage 'StickySectionTitles       = 'Text "StickySectionTitles (sticky-section-titles)"
  AttributeErrorMessage 'Style                     = 'Text "Style (style)"
  AttributeErrorMessage 'Url                       = 'Text "Url (url)"
  AttributeErrorMessage 'Value                     = 'Text "Value (value)"
  AttributeErrorMessage 'XMLNS                     = 'Text "XMLNS (xmlns)"
