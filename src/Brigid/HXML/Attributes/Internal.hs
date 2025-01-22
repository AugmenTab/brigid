{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HXML.Attributes.Internal
  ( Attribute
      ( Attr_NoAttribute
      , Attr_Custom
      , Attr_Hide
      , Attr_Id
      , Attr_SafeArea
      , Attr_Style
      , Attr_XMLNS
      )
  , attributeText
  ) where

import Data.Text qualified as T

import Brigid.HXML.Attributes.AttributeType (AttributeType (..))
import Brigid.HXML.Attributes.Elements (ValidAttribute)
import Brigid.HXML.Elements.TagType (TagType)
import Brigid.HXML.Types qualified as Types

data Attribute (tag :: TagType) where
  Attr_NoAttribute
    :: Attribute tag

  Attr_Custom
    :: T.Text
    -> T.Text
    -> Attribute tag

  Attr_Hide
    :: ValidAttribute 'Hide tag
    => Bool
    -> Attribute tag

  Attr_Id
    :: ValidAttribute 'Id tag
    => Types.Id
    -> Attribute tag

  Attr_SafeArea
    :: ValidAttribute 'SafeArea tag
    => Bool
    -> Attribute tag

  Attr_Style
    :: ValidAttribute 'Style tag
    => T.Text
    -> Attribute tag

  Attr_XMLNS
    :: ValidAttribute 'XMLNS tag
    => Types.URL
    -> Attribute tag

attributeText :: Attribute tag -> T.Text
attributeText attr =
  case attr of
    Attr_NoAttribute ->
      "no_attribute"

    Attr_Custom name _value ->
      name

    Attr_Hide _hide ->
      "hide"

    Attr_Id _id ->
      "id"

    Attr_SafeArea _safeArea ->
      "safe-area"

    Attr_Style _style ->
      "style"

    Attr_XMLNS _xmlns ->
      "xmlns"
