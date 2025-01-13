{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HTML.Attributes.RangeBound
  ( ValidRangeBound
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Elements.TagType (TagErrorMessage, TagType)
import Brigid.HTML.Internal.TagOperations (Elem)
import Brigid.HTML.Types qualified as Types

type ValidRangeBound rangeBound tag =
  AlertRangeBound (Elem tag (ValidRangeBoundsFor rangeBound)) rangeBound tag ~ 'True

type family ValidRangeBoundsFor (rangeBound :: Type) :: [TagType] where
  ValidRangeBoundsFor Types.Date          = [ Tags.Input, Tags.InputDate ]
  ValidRangeBoundsFor Types.DatetimeLocal = [ Tags.Input, Tags.InputDatetimeLocal ]
  ValidRangeBoundsFor Types.Month         = [ Tags.Input, Tags.InputMonth ]
  ValidRangeBoundsFor Types.Number        = TagGroups.RangedNumberTags
  ValidRangeBoundsFor Types.Time          = [ Tags.Input, Tags.InputTime ]
  ValidRangeBoundsFor Types.Week          = [ Tags.Input, Tags.InputWeek ]
  ValidRangeBoundsFor Types.RawRangeBound = TagGroups.RangedTags

type family AlertRangeBound (member :: Bool) (rangeBound :: Type) (tag :: TagType) :: Bool where
  AlertRangeBound 'True rangeBound tag =
    'True

  AlertRangeBound 'False rangeBound tag =
    TypeError
      ( 'Text "The "
          ':<>: RangeBoundTypeErrorMessage rangeBound
          ':<>: 'Text " range bound type is not valid for the "
          ':<>: TagErrorMessage tag
          ':<>: 'Text " element."
      )

type family RangeBoundTypeErrorMessage (rangeBound :: Type) :: ErrorMessage where
  RangeBoundTypeErrorMessage Types.Date          = 'Text "Date"
  RangeBoundTypeErrorMessage Types.DatetimeLocal = 'Text "DatetimeLocal"
  RangeBoundTypeErrorMessage Types.Month         = 'Text "Month"
  RangeBoundTypeErrorMessage Types.Number        = 'Text "Number"
  RangeBoundTypeErrorMessage Types.Time          = 'Text "Time"
  RangeBoundTypeErrorMessage Types.Week          = 'Text "Week"
  RangeBoundTypeErrorMessage Types.RawRangeBound = 'Text "RawRangeBound"
