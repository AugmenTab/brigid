{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Attributes.AttributeType
  ( AttributeErrorMessage
  , AttributeType
      ( Hide
      , Id
      , SafeArea
      , Scroll
      , ScrollOrientation
      , ShowsScrollIndicator
      , Style
      , XMLNS
      )
  ) where

import GHC.TypeLits (ErrorMessage (..))

data AttributeType
  = Hide
  | Id
  | SafeArea
  | Scroll
  | ScrollOrientation
  | ShowsScrollIndicator
  | Style
  | XMLNS

type family AttributeErrorMessage (attr :: AttributeType) :: ErrorMessage where
  AttributeErrorMessage Hide                 = 'Text "Hide (hide)"
  AttributeErrorMessage Id                   = 'Text "Id (id)"
  AttributeErrorMessage SafeArea             = 'Text "SafeArea (safe-area)"
  AttributeErrorMessage Scroll               = 'Text "Scroll (scroll)"
  AttributeErrorMessage ScrollOrientation    = 'Text "ScrollOrientation (scroll-orientation)"
  AttributeErrorMessage ShowsScrollIndicator = 'Text "ShowsScrollIndicator (shows-scroll-indicator)"
  AttributeErrorMessage Style                = 'Text "Style (style)"
  AttributeErrorMessage XMLNS                = 'Text "XMLNS (xmlns)"
