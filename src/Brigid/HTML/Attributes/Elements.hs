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
import Brigid.HTML.Elements.TagType (TagType)
import Brigid.HTML.Internal.TagOperations (Add, AlertAttribute, Elem, Remove)

type ValidAttribute attr tag =
  AlertAttribute (Elem tag (ValidElementsFor attr)) attr tag ~ 'True

type family ValidElementsFor (attribute :: AttributeType) :: [TagType] where
  ValidElementsFor 'Accept                  = '[ Tags.Input, Tags.InputFile ]
  ValidElementsFor 'AcceptCharset           = '[ Tags.Form ]
  ValidElementsFor 'Action                  = '[ Tags.Form ]
  ValidElementsFor 'Allow                   = '[ Tags.IFrame ]
  ValidElementsFor 'Alt                     = TagGroups.AltTags
  ValidElementsFor 'Async                   = '[ Tags.Script ]
  ValidElementsFor 'Autocomplete            = TagGroups.AutocompletableTags
  ValidElementsFor 'Autoplay                = TagGroups.MediaContent
  ValidElementsFor 'Capture                 = [ Tags.Input, Tags.InputFile ]
  ValidElementsFor 'Charset                 = [ Tags.Meta, Tags.Script ]
  ValidElementsFor 'Checked                 = TagGroups.CheckableTags
  ValidElementsFor 'Cite                    = TagGroups.CitableTags
  ValidElementsFor 'Cols                    = '[ Tags.TextArea ]
  ValidElementsFor 'Colspan                 = TagGroups.TableCells
  ValidElementsFor 'Coords                  = '[ Tags.Area ]
  ValidElementsFor 'Content                 = '[ Tags.Meta ]
  ValidElementsFor 'Controls                = TagGroups.MediaContent
  ValidElementsFor 'ControlsList            = TagGroups.MediaContent
  ValidElementsFor 'CrossOrigin             = TagGroups.CrossOriginTags
  ValidElementsFor 'Data                    = '[ Tags.Object ]
  ValidElementsFor 'Datetime                = TagGroups.TimestampableTags
  ValidElementsFor 'Decoding                = '[ Tags.Image ]
  ValidElementsFor 'Default                 = '[ Tags.Track ]
  ValidElementsFor 'Defer                   = '[ Tags.Script ]
  ValidElementsFor 'Dirname                 = TagGroups.DirnameableTags
  ValidElementsFor 'Disabled                = TagGroups.DisableableTags
  ValidElementsFor 'DisablePictureInPicture = '[ Tags.Video ]
  ValidElementsFor 'DisableRemotePlayback   = TagGroups.MediaContent
  ValidElementsFor 'Download                = '[ Tags.Anchor ]
  ValidElementsFor 'Enctype                 = '[ Tags.Form ]
  ValidElementsFor 'For                     = [ Tags.Label, Tags.Output ]
  ValidElementsFor 'Form                    = TagGroups.InputTags
  ValidElementsFor 'FormAction              = TagGroups.FormSubmitTags
  ValidElementsFor 'FormEnctype             = Remove Tags.InputImage TagGroups.FormSubmitTags
  ValidElementsFor 'FormMethod              = TagGroups.FormMethodTags
  ValidElementsFor 'FormNoValidate          = TagGroups.FormNoValidateTags
  ValidElementsFor 'FormTarget              = TagGroups.FormSubmitTags
  ValidElementsFor 'Headers                 = '[ Tags.TableDataCell ]
  ValidElementsFor 'Height                  = TagGroups.SizableTags
  ValidElementsFor 'High                    = '[ Tags.Meter ]
  ValidElementsFor 'Href                    = TagGroups.HrefTags
  ValidElementsFor 'HrefLang                = TagGroups.HrefLangTags
  ValidElementsFor 'HttpEquiv               = '[ Tags.Meta ]
  ValidElementsFor 'Integrity               = TagGroups.IntegrityTags
  ValidElementsFor 'IsMap                   = '[ Tags.Image ]
  ValidElementsFor 'Kind                    = '[ Tags.Track ]
  ValidElementsFor 'Label                   = TagGroups.LabelableTags
  ValidElementsFor 'List                    = TagGroups.InputTags
  ValidElementsFor 'Loop                    = TagGroups.MediaContent
  ValidElementsFor 'Low                     = '[ Tags.Meter ]
  ValidElementsFor 'Max                     = TagGroups.RangedTags
  ValidElementsFor 'MaxLength               = Add Tags.TextArea TagGroups.FreeTextInputTags
  ValidElementsFor 'Media                   = TagGroups.MediaTags
  ValidElementsFor 'Method                  = '[ Tags.Form ]
  ValidElementsFor 'Min                     = TagGroups.RangedTags
  ValidElementsFor 'MinLength               = Add Tags.TextArea TagGroups.FreeTextInputTags
  ValidElementsFor 'Multiple                = [ Tags.Input, Tags.InputFile, Tags.Select ]
  ValidElementsFor 'Muted                   = TagGroups.MediaContent
  ValidElementsFor 'Name                    = TagGroups.NameTags
  ValidElementsFor 'NoModule                = '[ Tags.Script ]
  ValidElementsFor 'NoValidate              = '[ Tags.Form ]
  ValidElementsFor 'Open                    = '[ Tags.Details ]
  ValidElementsFor 'Optimum                 = '[ Tags.Meter ]
  ValidElementsFor 'Pattern                 = TagGroups.FreeTextInputTags
  ValidElementsFor 'Ping                    = '[ Tags.Anchor ]
  ValidElementsFor 'Placeholder             = Add Tags.TextArea TagGroups.FreeTextInputTags
  ValidElementsFor 'PlaysInline             = '[ Tags.Video ]
  ValidElementsFor 'Poster                  = '[ Tags.Video ]
  ValidElementsFor 'PopoverTarget           = Add Tags.Button TagGroups.InputTags
  ValidElementsFor 'PopoverTargetAction     = Add Tags.Button TagGroups.InputTags
  ValidElementsFor 'Preload                 = TagGroups.MediaContent
  ValidElementsFor 'ReadOnly                = TagGroups.ReadOnlyTags
  ValidElementsFor 'ReferrerPolicy          = '[ Tags.Script ]
  ValidElementsFor 'Rel                     = TagGroups.RelTags
  ValidElementsFor 'Required                = TagGroups.RequireableTags
  ValidElementsFor 'Reversed                = '[ Tags.OrderedList ]
  ValidElementsFor 'Rows                    = '[ Tags.TextArea ]
  ValidElementsFor 'Rowspan                 = TagGroups.TableCells
  ValidElementsFor 'Sandbox                 = '[ Tags.IFrame ]
  ValidElementsFor 'Scope                   = '[ Tags.TableHeader ]
  ValidElementsFor 'Selected                = '[ Tags.Option ]
  ValidElementsFor 'Shape                   = '[ Tags.Area ]
  ValidElementsFor 'Size                    = Add Tags.Select TagGroups.FreeTextInputTags
  ValidElementsFor 'Sizes                   = '[ Tags.Image ]
  ValidElementsFor 'Span                    = [ Tags.TableColumn, Tags.TableColumnGroup ]
  ValidElementsFor 'Src                     = TagGroups.SrcTags
  ValidElementsFor 'SrcDoc                  = '[ Tags.IFrame ]
  ValidElementsFor 'SrcLang                 = '[ Tags.Track ]
  ValidElementsFor 'SrcSet                  = '[ Tags.Image, Tags.Source ]
  ValidElementsFor 'Start                   = '[ Tags.OrderedList ]
  ValidElementsFor 'Step                    = TagGroups.SteppableTags
  ValidElementsFor 'Target                  = TagGroups.TargetableTags
  ValidElementsFor 'Type                    = TagGroups.TypeableTags
  ValidElementsFor 'UseMap                  = [ Tags.Image, Tags.Object ]
  ValidElementsFor 'Value                   = TagGroups.ValuableTags
  ValidElementsFor 'Width                   = TagGroups.SizableTags
  ValidElementsFor 'Wrap                    = '[ Tags.TextArea ]
  ValidElementsFor 'XMLNS                   = '[ Tags.Html ] -- SVG, MathML

  ValidElementsFor 'HxValidate              = '[ Tags.Form ]
