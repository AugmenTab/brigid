{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Attributes.AttributeType
  ( AttributeErrorMessage
  , AttributeType
      ( ActivityIndicatorColor
      , AdjustsFontSizeToFit
      , AvoidKeyboard
      , Color
      , ContentContainerStyle
      , Hide
      , Html
      , Id
      , InjectedJavaScript
      , ItemHeight
      , Key
      , KeyboardDismissMode
      , KeyboardShouldPersistTaps
      , NumberOfLines
      , Preformatted
      , SafeArea
      , Scroll
      , ScrollOrientation
      , ScrollToInputOffset
      , Selectable
      , ShowLoadingIndicator
      , ShowsScrollIndicator
      , Source
      , Sticky
      , StickySectionTitles
      , Style
      , Url
      , XMLNS
      )
  ) where

import GHC.TypeLits (ErrorMessage (..))

data AttributeType
  = ActivityIndicatorColor
  | AdjustsFontSizeToFit
  | AvoidKeyboard
  | Color
  | ContentContainerStyle
  | Hide
  | Html
  | Id
  | InjectedJavaScript
  | ItemHeight
  | Key
  | KeyboardDismissMode
  | KeyboardShouldPersistTaps
  | NumberOfLines
  | Preformatted
  | SafeArea
  | Scroll
  | ScrollOrientation
  | ScrollToInputOffset
  | Selectable
  | ShowLoadingIndicator
  | ShowsScrollIndicator
  | Source
  | Sticky
  | StickySectionTitles
  | Style
  | Url
  | XMLNS

type family AttributeErrorMessage (attr :: AttributeType) :: ErrorMessage where
  AttributeErrorMessage 'ActivityIndicatorColor    = 'Text "ActivityIndicatorColor (activity-indicator-color)"
  AttributeErrorMessage 'AdjustsFontSizeToFit      = 'Text "AdjustsFontSizeToFit (adjustsFontSizeToFit)"
  AttributeErrorMessage 'AvoidKeyboard             = 'Text "AvoidKeyboard (avoid-keyboard)"
  AttributeErrorMessage 'Color                     = 'Text "Color (color)"
  AttributeErrorMessage 'ContentContainerStyle     = 'Text "ContentContainerStyle (content-container-style)"
  AttributeErrorMessage 'Hide                      = 'Text "Hide (hide)"
  AttributeErrorMessage 'Html                      = 'Text "Html (html)"
  AttributeErrorMessage 'Id                        = 'Text "Id (id)"
  AttributeErrorMessage 'InjectedJavaScript        = 'Text "InjectedJavaScript (injected-java-script)"
  AttributeErrorMessage 'ItemHeight                = 'Text "ItemHeight (itemHeight)"
  AttributeErrorMessage 'Key                       = 'Text "Key (key)"
  AttributeErrorMessage 'KeyboardDismissMode       = 'Text "KeyboardDismissMode (keyboard-dismiss-mode)"
  AttributeErrorMessage 'KeyboardShouldPersistTaps = 'Text "KeyboardShouldPersistTaps (keyboard-should-persist-taps)"
  AttributeErrorMessage 'NumberOfLines             = 'Text "NumberOfLines (numberOfLines)"
  AttributeErrorMessage 'Preformatted              = 'Text "Preformatted (preformatted)"
  AttributeErrorMessage 'SafeArea                  = 'Text "SafeArea (safe-area)"
  AttributeErrorMessage 'Scroll                    = 'Text "Scroll (scroll)"
  AttributeErrorMessage 'ScrollOrientation         = 'Text "ScrollOrientation (scroll-orientation)"
  AttributeErrorMessage 'ScrollToInputOffset       = 'Text "ScrollToInputOffset (scroll-to-input-offset)"
  AttributeErrorMessage 'Selectable                = 'Text "Selectable (selectable)"
  AttributeErrorMessage 'ShowLoadingIndicator      = 'Text "ShowLoadingIndicator (show-loading-indicator)"
  AttributeErrorMessage 'ShowsScrollIndicator      = 'Text "ShowsScrollIndicator (shows-scroll-indicator)"
  AttributeErrorMessage 'Source                    = 'Text "Source (source)"
  AttributeErrorMessage 'Sticky                    = 'Text "Sticky (sticky)"
  AttributeErrorMessage 'StickySectionTitles       = 'Text "StickySectionTitles (sticky-section-titles)"
  AttributeErrorMessage 'Style                     = 'Text "Style (style)"
  AttributeErrorMessage 'Url                       = 'Text "Url (url)"
  AttributeErrorMessage 'XMLNS                     = 'Text "XMLNS (xmlns)"
