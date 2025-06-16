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
  ValidElementsFor 'Abbreviation             = [ Tags.CustomHTML, Tags.TableHeader ]
  ValidElementsFor 'Accept                   = [ Tags.CustomHTML, Tags.Input, Tags.InputFile ]
  ValidElementsFor 'AcceptCharset            = [ Tags.CustomHTML, Tags.Form ]
  ValidElementsFor 'Action                   = [ Tags.CustomHTML, Tags.Form ]
  ValidElementsFor 'Allow                    = [ Tags.CustomHTML, Tags.IFrame ]
  ValidElementsFor 'Alt                      = TagGroups.AltTags
  ValidElementsFor 'As                       = [ Tags.CustomHTML, Tags.Link ]
  ValidElementsFor 'Async                    = [ Tags.CustomHTML, Tags.Script ]
  ValidElementsFor 'Autocomplete             = TagGroups.AutocompletableTags
  ValidElementsFor 'Autoplay                 = TagGroups.MediaContent
  ValidElementsFor 'Blocking                 = TagGroups.BlockingTags
  ValidElementsFor 'Capture                  = [ Tags.CustomHTML, Tags.Input, Tags.InputFile ]
  ValidElementsFor 'Charset                  = [ Tags.CustomHTML, Tags.Meta, Tags.Script ]
  ValidElementsFor 'Checked                  = TagGroups.CheckableTags
  ValidElementsFor 'Cite                     = TagGroups.CitableTags
  ValidElementsFor 'Cols                     = [ Tags.CustomHTML, Tags.TextArea ]
  ValidElementsFor 'Colspan                  = TagGroups.TableCells
  ValidElementsFor 'Command                  = [ Tags.Button, Tags.CustomHTML ]
  ValidElementsFor 'CommandFor               = [ Tags.Button, Tags.CustomHTML ]
  ValidElementsFor 'Coords                   = [ Tags.Area, Tags.CustomHTML ]
  ValidElementsFor 'Content                  = [ Tags.CustomHTML, Tags.Meta ]
  ValidElementsFor 'Controls                 = TagGroups.MediaContent
  ValidElementsFor 'ControlsList             = TagGroups.MediaContent
  ValidElementsFor 'CrossOrigin              = TagGroups.CrossOriginTags
  ValidElementsFor 'Data                     = [ Tags.CustomHTML, Tags.Object ]
  ValidElementsFor 'Datetime                 = TagGroups.TimestampableTags
  ValidElementsFor 'Decoding                 = [ Tags.CustomHTML, Tags.Image ]
  ValidElementsFor 'Default                  = [ Tags.CustomHTML, Tags.Track ]
  ValidElementsFor 'Defer                    = [ Tags.CustomHTML, Tags.Script ]
  ValidElementsFor 'Dirname                  = TagGroups.DirnameableTags
  ValidElementsFor 'Disabled                 = TagGroups.DisableableTags
  ValidElementsFor 'DisablePictureInPicture  = [ Tags.CustomHTML, Tags.Video ]
  ValidElementsFor 'DisableRemotePlayback    = TagGroups.MediaContent
  ValidElementsFor 'Download                 = [ Tags.Anchor, Tags.Area, Tags.CustomHTML ]
  ValidElementsFor 'ElementTiming            = TagGroups.ElementTimingTags
  ValidElementsFor 'Enctype                  = [ Tags.CustomHTML, Tags.Form ]
  ValidElementsFor 'FetchPriority            = [ Tags.CustomHTML, Tags.Image, Tags.Link, Tags.Script ]
  ValidElementsFor 'For                      = [ Tags.CustomHTML, Tags.Label, Tags.Output ]
  ValidElementsFor 'Form                     = TagGroups.FormTags
  ValidElementsFor 'FormAction               = TagGroups.FormSubmitTags
  ValidElementsFor 'FormEnctype              = Remove Tags.InputImage TagGroups.FormSubmitTags
  ValidElementsFor 'FormMethod               = TagGroups.FormMethodTags
  ValidElementsFor 'FormNoValidate           = TagGroups.FormNoValidateTags
  ValidElementsFor 'FormTarget               = TagGroups.FormSubmitTags
  ValidElementsFor 'Headers                  = [ Tags.CustomHTML, Tags.TableDataCell, Tags.TableHeader ]
  ValidElementsFor 'Height                   = TagGroups.SizableTags
  ValidElementsFor 'High                     = [ Tags.CustomHTML, Tags.Meter ]
  ValidElementsFor 'Href                     = TagGroups.HrefTags
  ValidElementsFor 'HrefLang                 = TagGroups.HrefLangTags
  ValidElementsFor 'HttpEquiv                = [ Tags.CustomHTML, Tags.Meta ]
  ValidElementsFor 'ImageSizes               = [ Tags.CustomHTML, Tags.Link ]
  ValidElementsFor 'ImageSrcset              = [ Tags.CustomHTML, Tags.Link ]
  ValidElementsFor 'Integrity                = TagGroups.IntegrityTags
  ValidElementsFor 'IsMap                    = [ Tags.CustomHTML, Tags.Image ]
  ValidElementsFor 'Kind                     = [ Tags.CustomHTML, Tags.Track ]
  ValidElementsFor 'Label                    = TagGroups.LabelableTags
  ValidElementsFor 'List                     = TagGroups.InputTags
  ValidElementsFor 'Loading                  = [ Tags.CustomHTML, Tags.Image, Tags.IFrame ]
  ValidElementsFor 'Loop                     = TagGroups.MediaContent
  ValidElementsFor 'Low                      = [ Tags.CustomHTML, Tags.Meter ]
  ValidElementsFor 'Max                      = TagGroups.RangedTags
  ValidElementsFor 'MaxLength                = Add Tags.TextArea TagGroups.FreeTextInputTags
  ValidElementsFor 'Media                    = TagGroups.MediaTags
  ValidElementsFor 'Method                   = [ Tags.CustomHTML, Tags.Form ]
  ValidElementsFor 'Min                      = TagGroups.RangedTags
  ValidElementsFor 'MinLength                = Add Tags.TextArea TagGroups.FreeTextInputTags
  ValidElementsFor 'Multiple                 = [ Tags.CustomHTML, Tags.Input, Tags.InputFile, Tags.Select ]
  ValidElementsFor 'Muted                    = TagGroups.MediaContent
  ValidElementsFor 'Name                     = TagGroups.NameTags
  ValidElementsFor 'NoModule                 = [ Tags.CustomHTML, Tags.Script ]
  ValidElementsFor 'NoValidate               = [ Tags.CustomHTML, Tags.Form ]
  ValidElementsFor 'Open                     = [ Tags.CustomHTML, Tags.Details, Tags.Dialog ]
  ValidElementsFor 'Optimum                  = [ Tags.CustomHTML, Tags.Meter ]
  ValidElementsFor 'Pattern                  = TagGroups.FreeTextInputTags
  ValidElementsFor 'Ping                     = [ Tags.Anchor, Tags.Area, Tags.CustomHTML ]
  ValidElementsFor 'Placeholder              = Add Tags.TextArea TagGroups.FreeTextInputTags
  ValidElementsFor 'PlaysInline              = [ Tags.CustomHTML, Tags.Video ]
  ValidElementsFor 'Poster                   = [ Tags.CustomHTML, Tags.Video ]
  ValidElementsFor 'PopoverTarget            = Add Tags.Button TagGroups.InputTags
  ValidElementsFor 'PopoverTargetAction      = Add Tags.Button TagGroups.InputTags
  ValidElementsFor 'Preload                  = TagGroups.MediaContent
  ValidElementsFor 'ReadOnly                 = TagGroups.ReadOnlyTags
  ValidElementsFor 'ReferrerPolicy           = TagGroups.ReferrerPolicyTags
  ValidElementsFor 'Rel                      = TagGroups.RelTags
  ValidElementsFor 'Required                 = TagGroups.RequireableTags
  ValidElementsFor 'Reversed                 = [ Tags.CustomHTML, Tags.OrderedList ]
  ValidElementsFor 'Rows                     = [ Tags.CustomHTML, Tags.TextArea ]
  ValidElementsFor 'Rowspan                  = TagGroups.TableCells
  ValidElementsFor 'Sandbox                  = [ Tags.CustomHTML, Tags.IFrame ]
  ValidElementsFor 'Scope                    = [ Tags.CustomHTML, Tags.TableHeader ]
  ValidElementsFor 'Selected                 = [ Tags.CustomHTML, Tags.Option ]
  ValidElementsFor 'ShadowRootMode           = [ Tags.CustomHTML, Tags.ContentTemplate ]
  ValidElementsFor 'ShadowRootDelegatesFocus = [ Tags.CustomHTML, Tags.ContentTemplate ]
  ValidElementsFor 'ShadowRootClonable       = [ Tags.CustomHTML, Tags.ContentTemplate ]
  ValidElementsFor 'Shape                    = [ Tags.CustomHTML, Tags.Area ]
  ValidElementsFor 'Size                     = Add Tags.Select TagGroups.FreeTextInputTags
  ValidElementsFor 'Sizes                    = [ Tags.CustomHTML, Tags.Image, Tags.Link, Tags.Source ]
  ValidElementsFor 'Span                     = [ Tags.CustomHTML, Tags.TableColumn, Tags.TableColumnGroup ]
  ValidElementsFor 'Src                      = TagGroups.SrcTags
  ValidElementsFor 'SrcDoc                   = [ Tags.CustomHTML, Tags.IFrame ]
  ValidElementsFor 'SrcLang                  = [ Tags.CustomHTML, Tags.Track ]
  ValidElementsFor 'SrcSet                   = [ Tags.CustomHTML, Tags.Image, Tags.Source ]
  ValidElementsFor 'Start                    = [ Tags.CustomHTML, Tags.OrderedList ]
  ValidElementsFor 'Step                     = TagGroups.SteppableTags
  ValidElementsFor 'Target                   = TagGroups.TargetableTags
  ValidElementsFor 'Type                     = TagGroups.TypeableTags
  ValidElementsFor 'UseMap                   = [ Tags.CustomHTML, Tags.Image, Tags.Object ]
  ValidElementsFor 'Value                    = TagGroups.ValuableTags
  ValidElementsFor 'Width                    = TagGroups.SizableTags
  ValidElementsFor 'Wrap                     = [ Tags.CustomHTML, Tags.TextArea ]
  ValidElementsFor 'XMLNS                    = [ Tags.CustomHTML, Tags.Html ] -- SVG, MathML

  ValidElementsFor 'HxValidate               = [ Tags.CustomHTML, Tags.Form ]
