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
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Elements.TagType (TagErrorMessage, TagType)
import Brigid.HTML.Internal.TagOperations (Elem)
import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types

type ValidValue val tag =
  AlertValue (Elem tag (ValidValuesFor val)) val tag ~ 'True

type family ValidValuesFor (val :: Type) :: [TagType] where
  ValidValuesFor Types.HexColor                 = [ Tags.CustomHTML, Tags.Input, Tags.InputColor ]
  ValidValuesFor Types.Date                     = [ Tags.CustomHTML, Tags.Input, Tags.InputDate ]
  ValidValuesFor Types.DatetimeLocal            = [ Tags.CustomHTML, Tags.Input, Tags.InputDatetimeLocal ]
  ValidValuesFor Types.EmailAddress             = [ Tags.CustomHTML, Tags.Input, Tags.InputEmail ]
  ValidValuesFor Integer                        = [ Tags.CustomHTML, Tags.ListItem ]
  ValidValuesFor Types.Month                    = [ Tags.CustomHTML, Tags.Input, Tags.InputMonth ]
  ValidValuesFor Types.Number                   = [ Tags.CustomHTML, Tags.Input, Tags.InputNumber, Tags.InputRange, Tags.Meter, Tags.Progress ]
  ValidValuesFor Types.PhoneNumber              = [ Tags.CustomHTML, Tags.Input, Tags.InputTel ]
  ValidValuesFor T.Text                         = TagGroups.TextValueTags
  ValidValuesFor Types.Time                     = [ Tags.CustomHTML, Tags.Input, Tags.InputTime ]
  ValidValuesFor Types.AbsoluteURL              = [ Tags.CustomHTML, Tags.Input, Tags.InputUrl ]
  ValidValuesFor (Types.RelativeURL Types.Get)  = [ Tags.CustomHTML, Tags.Input, Tags.InputUrl ]
  ValidValuesFor (Types.RelativeURL Types.Post) = [ Tags.CustomHTML, Tags.Input, Tags.InputUrl ]
  ValidValuesFor Types.RawURL                   = [ Tags.CustomHTML, Tags.Input, Tags.InputUrl ]
  ValidValuesFor Types.Week                     = [ Tags.CustomHTML, Tags.Input, Tags.InputWeek ]

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
  ValueTypeErrorMessage Types.HexColor                 = 'Text "HexColor"
  ValueTypeErrorMessage Types.Date                     = 'Text "Date"
  ValueTypeErrorMessage Types.DatetimeLocal            = 'Text "DatetimeLocal"
  ValueTypeErrorMessage Types.EmailAddress             = 'Text "Email"
  ValueTypeErrorMessage Types.Month                    = 'Text "Month"
  ValueTypeErrorMessage Types.Number                   = 'Text "Number"
  ValueTypeErrorMessage Types.PhoneNumber              = 'Text "PhoneNumber"
  ValueTypeErrorMessage T.Text                         = 'Text "Text"
  ValueTypeErrorMessage Types.Time                     = 'Text "Time"
  ValueTypeErrorMessage Types.AbsoluteURL              = 'Text "AbsoluteURL"
  ValueTypeErrorMessage (Types.RelativeURL Types.Get)  = 'Text "RelativeURL Get"
  ValueTypeErrorMessage (Types.RelativeURL Types.Post) = 'Text "RelativeURL Post"
  ValueTypeErrorMessage Types.RawURL                   = 'Text "RawURL"
  ValueTypeErrorMessage Types.Week                     = 'Text "Week"
