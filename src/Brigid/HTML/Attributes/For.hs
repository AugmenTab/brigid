{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HTML.Attributes.For
  ( ValidFor
  ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Elements.TagType (TagErrorMessage, TagType)
import Brigid.HTML.Internal.TagOperations (Elem)
import Brigid.Types qualified as Types

type ValidFor for tag =
  AlertFor (Elem tag (ValidForsFor for)) for tag ~ 'True

type family ValidForsFor (for :: Type) :: [TagType] where
  ValidForsFor Types.Id            = '[ Tags.Label ]
  ValidForsFor (NonEmpty Types.Id) = '[ Tags.Output ]

type family AlertFor (member :: Bool) (for :: Type) (tag :: TagType) :: Bool where
  AlertFor 'True for tag =
    'True

  AlertFor 'False for tag =
    TypeError
      ( 'Text "The "
          ':<>: ForTypeErrorMessage for
          ':<>: 'Text " for option type is not valid for the "
          ':<>: TagErrorMessage tag
          ':<>: 'Text " element."
      )

type family ForTypeErrorMessage (for :: Type) :: ErrorMessage where
  ForTypeErrorMessage Types.Id                = 'Text "Id"
  ForTypeErrorMessage (NonEmpty Types.Id) = 'Text "NonEmpty Id"
