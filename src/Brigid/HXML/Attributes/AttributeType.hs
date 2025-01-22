{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Attributes.AttributeType
  ( AttributeErrorMessage
  , AttributeType
      ( Hide
      , Id
      , SafeArea
      , Style
      , XMLNS
      )
  ) where

import GHC.TypeLits (ErrorMessage (..))

data AttributeType
  = Hide
  | Id
  | SafeArea
  | Style
  | XMLNS

type family AttributeErrorMessage (attr :: AttributeType) :: ErrorMessage where
  AttributeErrorMessage Hide      = 'Text "Hide (hide)"
  AttributeErrorMessage Id        = 'Text "Id (id)"
  AttributeErrorMessage SafeArea  = 'Text "SafeArea (safe-area)"
  AttributeErrorMessage Style     = 'Text "Style (style)"
  AttributeErrorMessage XMLNS     = 'Text "XMLNS (xmlns)"
