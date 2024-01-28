{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HTML.Attributes.Elements
  ( ValidAttribute
  ) where

import HTML.Attributes.AttributeType (AttributeType(..))
import HTML.Elements.TagGroups qualified as TagGroups
import HTML.Elements.TagType (TagType(..))
import HTML.Contains (Contains)

type ValidAttribute attributeType tag =
  Contains (ValidElementsFor attributeType) tag

type family ValidElementsFor (attribute :: AttributeType) :: [TagType] where
  ValidElementsFor CrossOrigin = TagGroups.CrossOriginTags
  ValidElementsFor Disabled    = TagGroups.DisableableTags

