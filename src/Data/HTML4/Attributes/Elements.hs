{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML4.Attributes.Elements
  ( ValidAttribute
  ) where

import Data.HTML4.Attributes.AttributeType (AttributeType(..))
import Data.HTML4.Elements.TagType (TagType(..))
import Data.HTML4.Contains (Contains)

type ValidAttribute attributeType tag =
  Contains (ValidElementsFor attributeType) tag

type family ValidElementsFor (attribute :: AttributeType) :: [TagType] where
  ValidElementsFor Width    = '[ Paragraph ]
  ValidElementsFor Disabled = '[ Anchor ]

