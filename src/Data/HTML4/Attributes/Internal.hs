{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML4.Attributes.Internal
  ( ValidAttribute
  , Attribute
      ( Id
      , Class
      , Width
      , Disabled
      )
  ) where

import Data.Text qualified as T

import Data.HTML4.Attributes.AttributeType as A
import Data.HTML4.Elements.TagType as E
import Data.HTML4.Types.Contains (Contains)

type ValidAttribute attributeType tag =
  Contains (ValidElementsFor attributeType) tag

type family ValidElementsFor (attribute :: A.AttributeType) :: [E.TagType] where
  ValidElementsFor A.Id       = [ 'E.Division, 'E.Span ]
  ValidElementsFor A.Class    = '[ E.Division ]
  ValidElementsFor A.Width    = '[ E.Paragraph ]
  ValidElementsFor A.Disabled = '[ E.Anchor ]

-- TODO: Add HTMX, Aria, and Event Listener attributes
data Attribute (tag :: E.TagType) where
  -- Global Attributes
  Id    :: T.Text -> Attribute tag
  Class :: T.Text -> Attribute tag

  -- Scoped Attributes
  Width    :: ValidAttribute 'A.Width    tag => Int  -> Attribute tag
  Disabled :: ValidAttribute 'A.Disabled tag => Bool -> Attribute tag
