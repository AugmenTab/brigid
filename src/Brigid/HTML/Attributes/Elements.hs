{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Attributes.Elements
  ( ValidAttribute
  ) where

import Brigid.HTML.Attributes.AttributeType (AttributeType(..))
import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.TagType qualified as TagType
import Brigid.HTML.Internal.TagOperations (AlertAttribute, Elem)

type ValidAttribute attr tag =
  AlertAttribute (Elem tag (ValidElementsFor attr)) attr tag ~ 'True

type family ValidElementsFor (attribute :: AttributeType) :: [TagType.TagType] where
  ValidElementsFor CrossOrigin = TagGroups.CrossOriginTags
  ValidElementsFor Disabled    = TagGroups.DisableableTags
  ValidElementsFor Href        = TagGroups.HrefTags

  ValidElementsFor HxValidate  = '[ TagType.Form ]
