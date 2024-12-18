{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HTML.Attributes.Type
  ( ValidTypeOption
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage(..), TypeError)

import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.TagType (TagErrorMessage, TagType)
import Brigid.HTML.Internal.TagOperations (Elem)
import Brigid.HTML.Types qualified as Types

type ValidTypeOption type_ tag =
  AlertTypeOption (Elem tag (ValidTypeOptionsFor type_)) type_ tag ~ 'True

type family ValidTypeOptionsFor (type_ :: Type) :: [TagType] where
  ValidTypeOptionsFor Types.InputType     = TagGroups.InputTags
  ValidTypeOptionsFor Types.RawTypeOption = TagGroups.TypeableTags

type family AlertTypeOption (member :: Bool) (type_ :: Type) (tag :: TagType) :: Bool where
  AlertTypeOption 'True type_ tag =
    'True

  AlertTypeOption 'False type_ tag =
    TypeError
      ( 'Text "The "
          ':<>: TypeOptionTypeErrorMessage type_
          ':<>: 'Text " type option is not valid for the "
          ':<>: TagErrorMessage tag
          ':<>: 'Text " element."
      )

type family TypeOptionTypeErrorMessage (type_ :: Type) :: ErrorMessage where
  TypeOptionTypeErrorMessage Types.InputType = 'Text "InputType"
