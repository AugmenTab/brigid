{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Attributes.AttributeType
  ( AttributeErrorMessage
  , AttributeType
      ( AdjustsFontSizeToFit
      , AvoidKeyboard
      , ContentContainerStyle
      , Hide
      , Id
      , KeyboardDismissMode
      , NumberOfLines
      , Preformatted
      , SafeArea
      , Scroll
      , ScrollOrientation
      , ScrollToInputOffset
      , Selectable
      , ShowsScrollIndicator
      , Sticky
      , Style
      , XMLNS
      )
  ) where

import GHC.TypeLits (ErrorMessage (..))

data AttributeType
  = AdjustsFontSizeToFit
  | AvoidKeyboard
  | ContentContainerStyle
  | Hide
  | Id
  | KeyboardDismissMode
  | NumberOfLines
  | Preformatted
  | SafeArea
  | Scroll
  | ScrollOrientation
  | ScrollToInputOffset
  | Selectable
  | ShowsScrollIndicator
  | Sticky
  | Style
  | XMLNS

type family AttributeErrorMessage (attr :: AttributeType) :: ErrorMessage where
  AttributeErrorMessage 'AdjustsFontSizeToFit  = 'Text "AdjustsFontSizeToFit (adjustsFontSizeToFit)"
  AttributeErrorMessage 'AvoidKeyboard         = 'Text "AvoidKeyboard (avoid-keyboard)"
  AttributeErrorMessage 'ContentContainerStyle = 'Text "ContentContainerStyle (content-container-style)"
  AttributeErrorMessage 'Hide                  = 'Text "Hide (hide)"
  AttributeErrorMessage 'Id                    = 'Text "Id (id)"
  AttributeErrorMessage 'KeyboardDismissMode   = 'Text "KeyboardDismissMode (keyboard-dismiss-mode)"
  AttributeErrorMessage 'NumberOfLines         = 'Text "NumberOfLines (numberOfLines)"
  AttributeErrorMessage 'Preformatted          = 'Text "Preformatted (preformatted)"
  AttributeErrorMessage 'SafeArea              = 'Text "SafeArea (safe-area)"
  AttributeErrorMessage 'Scroll                = 'Text "Scroll (scroll)"
  AttributeErrorMessage 'ScrollOrientation     = 'Text "ScrollOrientation (scroll-orientation)"
  AttributeErrorMessage 'ScrollToInputOffset   = 'Text "ScrollToInputOffset (scroll-to-input-offset)"
  AttributeErrorMessage 'Selectable            = 'Text "Selectable (selectable)"
  AttributeErrorMessage 'ShowsScrollIndicator  = 'Text "ShowsScrollIndicator (shows-scroll-indicator)"
  AttributeErrorMessage 'Sticky                = 'Text "Sticky (sticky)"
  AttributeErrorMessage 'Style                 = 'Text "Style (style)"
  AttributeErrorMessage 'XMLNS                 = 'Text "XMLNS (xmlns)"
