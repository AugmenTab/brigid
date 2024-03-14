{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Attributes.Elements
  ( ValidAttribute
  ) where

import HTML.Attributes.AttributeType (AttributeType(..))
import HTML.Elements.TagGroups qualified as TagGroups
import HTML.Elements.TagType qualified as TagType
import HTML.Internal.TagOperations (AlertAttribute, Elem)

type ValidAttribute attr tag =
  AlertAttribute (Elem tag (ValidElementsFor attr)) attr tag ~ 'True

type family ValidElementsFor (attribute :: AttributeType) :: [TagType.TagType] where
  ValidElementsFor CrossOrigin = TagGroups.CrossOriginTags
  ValidElementsFor Disabled    = TagGroups.DisableableTags
  ValidElementsFor Href        = TagGroups.HrefTags

  ValidElementsFor HxValidate  = '[ TagType.Form ]
