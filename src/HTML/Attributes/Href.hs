{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module HTML.Attributes.Href
  ( ValidHref
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage(..), TypeError)

import HTML.Elements.TagType (TagErrorMessage, TagType(Anchor, Area, Base, Link))
import HTML.Internal.TagOperations (Elem)
import HTML.Types qualified as Types

type ValidHref href tag =
  AlertHref (Elem tag (ValidHrefsFor href)) href tag ~ 'True

type family ValidHrefsFor (href :: Type) :: [TagType] where
  ValidHrefsFor Types.AbsoluteURL = [ 'Anchor, 'Area, 'Base, 'Link ]
  ValidHrefsFor Types.RelativeURL = [ 'Anchor, 'Area, 'Base, 'Link ]
  ValidHrefsFor Types.Id          = [ 'Anchor, 'Area ]
  ValidHrefsFor Types.Email       = [ 'Anchor, 'Area ]
  ValidHrefsFor Types.RawURL      = [ 'Anchor, 'Area, 'Base, 'Link ]

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
  HrefTypeErrorMessage Types.AbsoluteURL = 'Text "AbsoluteURL"
  HrefTypeErrorMessage Types.RelativeURL = 'Text "RelativeURL"
  HrefTypeErrorMessage Types.Id          = 'Text "Id"
  HrefTypeErrorMessage Types.Email       = 'Text "Email"
  HrefTypeErrorMessage Types.RawURL      = 'Text "RawURL"
