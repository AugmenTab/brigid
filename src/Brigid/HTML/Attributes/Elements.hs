{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Attributes.Elements
  ( ValidAttribute
  ) where

import Brigid.HTML.Attributes.AttributeType (AttributeType (..))
import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Elements.TagType qualified as TagType
import Brigid.HTML.Internal.TagOperations (AlertAttribute, Elem)

type ValidAttribute attr tag =
  AlertAttribute (Elem tag (ValidElementsFor attr)) attr tag ~ 'True

type family ValidElementsFor (attribute :: AttributeType) :: [TagType.TagType] where
  ValidElementsFor AcceptCharset           = '[ Tags.Form ]
  ValidElementsFor Alt                     = '[ Tags.Area, Tags.Image ]
  ValidElementsFor Async                   = '[ Tags.Script ]
  ValidElementsFor Autoplay                = TagGroups.MediaContent
  ValidElementsFor Charset                 = [ Tags.Meta, Tags.Script ]
  ValidElementsFor Cite                    = TagGroups.CitableTags
  ValidElementsFor Cols                    = '[ Tags.TextArea ]
  ValidElementsFor Colspan                 = TagGroups.TableCells
  ValidElementsFor Coords                  = '[ Tags.Area ]
  ValidElementsFor Content                 = '[ Tags.Meta ]
  ValidElementsFor Controls                = TagGroups.MediaContent
  ValidElementsFor ControlsList            = TagGroups.MediaContent
  ValidElementsFor CrossOrigin             = TagGroups.CrossOriginTags
  ValidElementsFor Datetime                = TagGroups.TimestampableTags
  ValidElementsFor Default                 = '[ Tags.Track ]
  ValidElementsFor Defer                   = '[ Tags.Script ]
  ValidElementsFor Disabled                = TagGroups.DisableableTags
  ValidElementsFor DisablePictureInPicture = '[ Tags.Video ]
  ValidElementsFor DisableRemotePlayback   = TagGroups.MediaContent
  ValidElementsFor Headers                 = '[ Tags.TableDataCell ]
  ValidElementsFor Height                  = TagGroups.SizableTags
  ValidElementsFor Href                    = TagGroups.HrefTags
  ValidElementsFor IsMap                   = '[ Tags.Image ]
  ValidElementsFor Kind                    = '[ Tags.Track ]
  ValidElementsFor Label                   = TagGroups.LabelableTags
  ValidElementsFor Loop                    = TagGroups.MediaContent
  ValidElementsFor MaxLength               = TagGroups.LengthTags
  ValidElementsFor MinLength               = TagGroups.LengthTags
  ValidElementsFor Muted                   = TagGroups.MediaContent
  ValidElementsFor Name                    = TagGroups.NameTags
  ValidElementsFor NoModule                = '[ Tags.Script ]
  ValidElementsFor Ping                    = '[ Tags.Anchor ]
  ValidElementsFor PlaysInline             = '[ Tags.Video ]
  ValidElementsFor Poster                  = '[ Tags.Video ]
  ValidElementsFor Preload                 = TagGroups.MediaContent
  ValidElementsFor ReferrerPolicy          = '[ Tags.Script ]
  ValidElementsFor Rel                     = TagGroups.RelTags
  ValidElementsFor Rows                    = '[ Tags.TextArea ]
  ValidElementsFor Rowspan                 = TagGroups.TableCells
  ValidElementsFor Shape                   = '[ Tags.Area ]
  ValidElementsFor Src                     = TagGroups.SrcTags
  ValidElementsFor SrcLang                 = '[ Tags.Track ]
  ValidElementsFor Target                  = TagGroups.TargetableTags
  ValidElementsFor Type                    = TagGroups.TypeableTags
  ValidElementsFor Width                   = TagGroups.SizableTags
  ValidElementsFor Wrap                    = '[ Tags.TextArea ]
  ValidElementsFor XMLNS                   = '[ Tags.Html ] -- SVG, MathML

  ValidElementsFor HxValidate              = '[ TagType.Form ]
