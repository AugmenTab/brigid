{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HTML.Attributes.Name
  ( ValidName
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Elements.TagType (TagErrorMessage, TagType)
import Brigid.HTML.Internal.TagOperations (Elem, Remove)
import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types

type ValidName name tag =
  AlertName (Elem tag (ValidNamesFor name)) name tag ~ 'True

type family ValidNamesFor (name :: Type) :: [TagType] where
  ValidNamesFor Types.Name         = Remove Tags.Meta TagGroups.NameTags
  ValidNamesFor Types.MetadataName = '[ Tags.Meta ]

type family AlertName (member :: Bool) (name :: Type) (tag :: TagType) :: Bool where
  AlertName 'True name tag =
    'True

  AlertName 'False name tag =
    TypeError
      ( 'Text "The "
          ':<>: NameTypeErrorMessage name
          ':<>: 'Text " name type is not valid for the "
          ':<>: TagErrorMessage tag
          ':<>: 'Text " element."
      )

type family NameTypeErrorMessage (name :: Type) :: ErrorMessage where
  NameTypeErrorMessage Types.Name         = 'Text "Name"
  NameTypeErrorMessage Types.MetadataName = 'Text "MetadataName"
