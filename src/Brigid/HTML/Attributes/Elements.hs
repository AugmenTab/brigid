{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
  ValidElementsFor Alt                     = TagGroups.AltTags
  ValidElementsFor Async                   = '[ Tags.Script ]
  ValidElementsFor Autoplay                = TagGroups.MediaContent
  ValidElementsFor Charset                 = [ Tags.Meta, Tags.Script ]
  ValidElementsFor Checked                 = TagGroups.CheckableTags
  ValidElementsFor Cite                    = TagGroups.CitableTags
  ValidElementsFor Cols                    = '[ Tags.TextArea ]
  ValidElementsFor Colspan                 = TagGroups.TableCells
  ValidElementsFor Coords                  = '[ Tags.Area ]
  ValidElementsFor Content                 = '[ Tags.Meta ]
  ValidElementsFor Controls                = TagGroups.MediaContent
  ValidElementsFor ControlsList            = TagGroups.MediaContent
  ValidElementsFor CrossOrigin             = TagGroups.CrossOriginTags
  ValidElementsFor Datetime                = TagGroups.TimestampableTags
  ValidElementsFor Decoding                = '[ Tags.Image ]
  ValidElementsFor Default                 = '[ Tags.Track ]
  ValidElementsFor Defer                   = '[ Tags.Script ]
  ValidElementsFor Dirname                 = TagGroups.DirnameableTags
  ValidElementsFor Disabled                = TagGroups.DisableableTags
  ValidElementsFor DisablePictureInPicture = '[ Tags.Video ]
  ValidElementsFor DisableRemotePlayback   = TagGroups.MediaContent
  ValidElementsFor Download                = '[ Tags.Anchor ]
  ValidElementsFor For                     = '[ Tags.Label, Tags.Output ]
  ValidElementsFor Form                    = TagGroups.InputTags
  ValidElementsFor FormMethod              = TagGroups.FormMethodTags
  ValidElementsFor FormNoValidate          = TagGroups.FormNoValidateTags
  ValidElementsFor FormTarget              = TagGroups.FormTargetTags
  ValidElementsFor Headers                 = '[ Tags.TableDataCell ]
  ValidElementsFor Height                  = TagGroups.SizableTags
  ValidElementsFor High                    = '[ Tags.Meter ]
  ValidElementsFor Href                    = TagGroups.HrefTags
  ValidElementsFor HrefLang                = TagGroups.HrefLangTags
  ValidElementsFor IsMap                   = '[ Tags.Image ]
  ValidElementsFor Kind                    = '[ Tags.Track ]
  ValidElementsFor Label                   = TagGroups.LabelableTags
  ValidElementsFor List                    = TagGroups.InputTags
  ValidElementsFor Loop                    = TagGroups.MediaContent
  ValidElementsFor Low                     = '[ Tags.Meter ]
  ValidElementsFor Max                     = TagGroups.RangedTags
  ValidElementsFor MaxLength               = TagGroups.LengthTags
  ValidElementsFor Method                  = '[ Tags.Form ]
  ValidElementsFor Min                     = TagGroups.RangedTags
  ValidElementsFor MinLength               = TagGroups.LengthTags
  ValidElementsFor Muted                   = TagGroups.MediaContent
  ValidElementsFor Name                    = TagGroups.NameTags
  ValidElementsFor NoModule                = '[ Tags.Script ]
  ValidElementsFor NoValidate              = '[ Tags.Form ]
  ValidElementsFor Optimum                 = '[ Tags.Meter ]
  ValidElementsFor Ping                    = '[ Tags.Anchor ]
  ValidElementsFor Placeholder             = TagGroups.PlaceholderableTags
  ValidElementsFor PlaysInline             = '[ Tags.Video ]
  ValidElementsFor Poster                  = '[ Tags.Video ]
  ValidElementsFor Preload                 = TagGroups.MediaContent
  ValidElementsFor ReadOnly                = TagGroups.ReadOnlyTags
  ValidElementsFor ReferrerPolicy          = '[ Tags.Script ]
  ValidElementsFor Rel                     = TagGroups.RelTags
  ValidElementsFor Required                = TagGroups.RequireableTags
  ValidElementsFor Reversed                = '[ Tags.OrderedList ]
  ValidElementsFor Rows                    = '[ Tags.TextArea ]
  ValidElementsFor Rowspan                 = TagGroups.TableCells
  ValidElementsFor Shape                   = '[ Tags.Area ]
  ValidElementsFor Src                     = TagGroups.SrcTags
  ValidElementsFor SrcLang                 = '[ Tags.Track ]
  ValidElementsFor Start                   = '[ Tags.OrderedList ]
  ValidElementsFor Target                  = TagGroups.TargetableTags
  ValidElementsFor Type                    = TagGroups.TypeableTags
  ValidElementsFor Value                   = TagGroups.ValuableTags
  ValidElementsFor Width                   = TagGroups.SizableTags
  ValidElementsFor Wrap                    = '[ Tags.TextArea ]
  ValidElementsFor XMLNS                   = '[ Tags.Html ] -- SVG, MathML

  ValidElementsFor HxValidate              = '[ TagType.Form ]
