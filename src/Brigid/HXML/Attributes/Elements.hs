{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HXML.Attributes.Elements
  ( ValidAttribute
  ) where

import Brigid.HXML.Attributes.AttributeType (AttributeType (..))
import Brigid.HXML.Elements.Tags qualified as Tags
import Brigid.HXML.Elements.TagType (TagType)
import Brigid.HXML.Internal.TagOperations (AlertAttribute, Elem)

type ValidAttribute attr tag =
  AlertAttribute (Elem tag (ValidElementsFor attr)) attr tag ~ 'True

type family ValidElementsFor (attribute :: AttributeType) :: [TagType] where
  ValidElementsFor 'Hide     = '[ Tags.Header ]
  ValidElementsFor 'Id       = '[ Tags.Header, Tags.Screen ]
  ValidElementsFor 'SafeArea = '[ Tags.Header ]
  ValidElementsFor 'Style    = '[ Tags.Header ]
  ValidElementsFor 'XMLNS    = '[ Tags.Document ]
