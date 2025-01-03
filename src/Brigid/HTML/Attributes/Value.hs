{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HTML.Attributes.Value
  ( ValidValue
  ) where

import Data.Kind (Type)
import Data.Text qualified as T
import GHC.TypeLits (ErrorMessage(..), TypeError)

import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Elements.TagType (TagErrorMessage, TagType)
import Brigid.HTML.Internal.TagOperations (Elem)
import Brigid.HTML.Types qualified as Types

type ValidValue val tag =
  AlertValue (Elem tag (ValidValuesFor val)) val tag ~ 'True

type family ValidValuesFor (val :: Type) :: [TagType] where
  ValidValuesFor Types.HexColor    = [ Tags.Input, Tags.InputColor ]
  ValidValuesFor Types.Date        = [ Tags.Input, Tags.InputDate ]
  ValidValuesFor Types.Email       = [ Tags.Input, Tags.InputEmail ]
  ValidValuesFor Types.Month       = [ Tags.Input, Tags.InputMonth ]
  ValidValuesFor Types.PhoneNumber = [ Tags.Input, Tags.InputTel ]
  ValidValuesFor T.Text            = TagGroups.TextValueTags
  ValidValuesFor Types.Time        = [ Tags.Input, Tags.InputTime ]
  ValidValuesFor Types.AbsoluteURL = [ Tags.Input, Tags.InputUrl ]
  ValidValuesFor Types.RawURL      = [ Tags.Input, Tags.InputUrl ]
  ValidValuesFor Types.Week        = [ Tags.Input, Tags.InputWeek ]

type family AlertValue (member :: Bool) (val :: Type) (tag :: TagType) :: Bool where
  AlertValue 'True val tag =
    'True

  AlertValue 'False val tag =
    TypeError
      ( 'Text "The "
          ':<>: ValueTypeErrorMessage val
          ':<>: 'Text " value type is not valid for the "
          ':<>: TagErrorMessage tag
          ':<>: 'Text " element."
      )

type family ValueTypeErrorMessage (val :: Type) :: ErrorMessage where
  ValueTypeErrorMessage _ = 'Text "_"
