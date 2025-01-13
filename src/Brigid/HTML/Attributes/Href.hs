{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HTML.Attributes.Href
  ( ValidHref
  , HrefTypeErrorMessage
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.TagType (TagErrorMessage, TagType)
import Brigid.HTML.Internal.TagOperations (Elem)
import Brigid.HTML.Types qualified as Types

type ValidHref href tag =
  AlertHref (Elem tag (ValidHrefsFor href)) href tag ~ 'True

type family ValidHrefsFor (href :: Type) :: [TagType] where
  ValidHrefsFor Types.AbsoluteURL             = TagGroups.HrefTags
  ValidHrefsFor (Types.RelativeURL Types.Get) = TagGroups.HrefTags
  ValidHrefsFor (Types.RelativeURL method)    = '[]
  ValidHrefsFor Types.Id                      = TagGroups.URLTags
  ValidHrefsFor Types.Email                   = TagGroups.URLTags
  ValidHrefsFor Types.RawURL                  = TagGroups.HrefTags

type family AlertHref (member :: Bool) (href :: Type) (tag :: TagType) :: Bool where
  AlertHref 'True href tag =
    'True

  AlertHref 'False href tag =
    TypeError
      ( 'Text "The "
          ':<>: HrefTypeErrorMessage href
          ':<>: 'Text " href type is not valid for the "
          ':<>: TagErrorMessage tag
          ':<>: 'Text " element."
      )

type family HrefTypeErrorMessage (href :: Type) :: ErrorMessage where
  HrefTypeErrorMessage Types.AbsoluteURL =
    'Text "AbsoluteURL"

  HrefTypeErrorMessage (Types.RelativeURL method) =
    'Text "RelativeURL " ':<>: MethodTypeErrorMessage method

  HrefTypeErrorMessage Types.Id =
    'Text "Id"

  HrefTypeErrorMessage Types.Email =
    'Text "Email"

  HrefTypeErrorMessage Types.RawURL =
    'Text "RawURL"

type family MethodTypeErrorMessage (method :: Types.Method) :: ErrorMessage where
  MethodTypeErrorMessage Types.Get    = 'Text "Get"
  MethodTypeErrorMessage Types.Post   = 'Text "Post"
  MethodTypeErrorMessage Types.Delete = 'Text "Delete"
  MethodTypeErrorMessage Types.Put    = 'Text "Put"
  MethodTypeErrorMessage Types.Patch  = 'Text "Patch"
