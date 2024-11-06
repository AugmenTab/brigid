{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Attributes.Elements
  ( ValidAttribute
  ) where

import Brigid.HTML.Attributes.AttributeType (AttributeType(..))
import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Elements.TagType qualified as TagType
import Brigid.HTML.Internal.TagOperations (AlertAttribute, Elem)

type ValidAttribute attr tag =
  AlertAttribute (Elem tag (ValidElementsFor attr)) attr tag ~ 'True

type family ValidElementsFor (attribute :: AttributeType) :: [TagType.TagType] where
  ValidElementsFor Async          = '[ Tags.Script ]
  ValidElementsFor Autoplay       = TagGroups.MediaContent
  ValidElementsFor Charset        = [ Tags.Meta, Tags.Script ]
  ValidElementsFor Content        = '[ Tags.Meta ]
  ValidElementsFor CrossOrigin    = TagGroups.CrossOriginTags
  ValidElementsFor Defer          = '[ Tags.Script ]
  ValidElementsFor Disabled       = TagGroups.DisableableTags
  ValidElementsFor Headers        = '[ Tags.TableDataCell ]
  ValidElementsFor Height         = TagGroups.SizableTags
  ValidElementsFor Href           = TagGroups.HrefTags
  ValidElementsFor Name           = TagGroups.NameTags
  ValidElementsFor NoModule       = '[ Tags.Script ]
  ValidElementsFor ReferrerPolicy = '[ Tags.Script ]
  ValidElementsFor Rel            = TagGroups.RelTags
  ValidElementsFor Src            = TagGroups.SrcTags
  ValidElementsFor Width          = TagGroups.SizableTags

  ValidElementsFor HxValidate     = '[ TagType.Form ]
