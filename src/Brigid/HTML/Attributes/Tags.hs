{-# LANGUAGE DataKinds #-}

module Brigid.HTML.Attributes.Tags
  ( AccessKey
  , Autocapitalize
  , Autofocus
  , Class
  , ContentEditable
  , CustomData
  , Dir
  , Draggable
  , EnterKeyHint
  , ExportParts
  , Hidden
  , Id
  , Inert
  , InputMode
  , Is
  , ItemId
  , ItemProp
  , ItemRef
  , ItemScope
  , ItemType
  , Lang
  , Nonce
  , Part
  , Popover
  , Role
  , Slot
  , Spellcheck
  , Style
  , TabIndex
  , Title
  , Translate
  , WritingSuggestions

  , Abbreviation
  , Accept
  , AcceptCharset
  , Action
  , Allow
  , Alt
  , As
  , Async
  , Autocomplete
  , Autoplay
  , Blocking
  , Capture
  , Charset
  , Checked
  , Cite
  , Cols
  , Colspan
  , Content
  , Controls
  , ControlsList
  , Coords
  , CrossOrigin
  , Data
  , Datetime
  , Decoding
  , Default
  , Defer
  , Dirname
  , DisablePictureInPicture
  , Disabled
  , DisableRemotePlayback
  , Download
  , Enctype
  , FetchPriority
  , For
  , Form
  , FormAction
  , FormEnctype
  , FormMethod
  , FormNoValidate
  , FormTarget
  , Headers
  , Height
  , High
  , Href
  , HrefLang
  , HttpEquiv
  , ImageSizes
  , ImageSrcset
  , Integrity
  , IsMap
  , Kind
  , Label
  , List
  , Loading
  , Loop
  , Low
  , Max
  , MaxLength
  , MinLength
  , Media
  , Method
  , Min
  , Multiple
  , Muted
  , Name
  , NoModule
  , NoValidate
  , Open
  , Optimum
  , Pattern
  , Ping
  , Placeholder
  , PlaysInline
  , PopoverTarget
  , PopoverTargetAction
  , Poster
  , Preload
  , ReadOnly
  , ReferrerPolicy
  , Rel
  , Required
  , Reversed
  , Rows
  , Rowspan
  , Sandbox
  , Scope
  , Selected
  , Shape
  , Size
  , Sizes
  , Span
  , Src
  , SrcDoc
  , SrcLang
  , SrcSet
  , Start
  , Step
  , Target
  , Type
  , UseMap
  , Value
  , Width
  , Wrap
  , XMLNS

  , Htmx
  , HxGet
  , HxPost
  , HxPushURL
  , HxSelect
  , HxSelectOOB
  , HxSwap
  , HxSwapOOB
  , HxTarget
  , HxTrigger
  , HxVals
  , HxBoost
  , HxConfirm
  , HxDelete
  , HxDisable
  , HxDisabledElt
  , HxDisinherit
  , HxEncoding
  , HxExt
  , HxHeaders
  , HxHistory
  , HxHistoryElt
  , HxInclude
  , HxIndicator
  , HxOn
  , HxParams
  , HxPatch
  , HxPreserve
  , HxPrompt
  , HxPut
  , HxReplaceURL
  , HxValidate

  , HyperScript
  ) where

import Brigid.HTML.Attributes.AttributeType as AttributeType

type AccessKey = 'AttributeType.AccessKey
type Autocapitalize = 'AttributeType.Autocapitalize
type Autofocus = 'AttributeType.Autofocus
type Class = 'AttributeType.Class
type ContentEditable = 'AttributeType.ContentEditable
type CustomData = 'AttributeType.CustomData
type Dir = 'AttributeType.Dir
type Draggable = 'AttributeType.Draggable
type EnterKeyHint = 'AttributeType.EnterKeyHint
type ExportParts = 'AttributeType.ExportParts
type Hidden = 'AttributeType.Hidden
type Id = 'AttributeType.Id
type Inert = 'AttributeType.Inert
type InputMode = 'AttributeType.InputMode
type Is = 'AttributeType.Is
type ItemId = 'AttributeType.ItemId
type ItemProp = 'AttributeType.ItemProp
type ItemRef = 'AttributeType.ItemRef
type ItemScope = 'AttributeType.ItemScope
type ItemType = 'AttributeType.ItemType
type Lang = 'AttributeType.Lang
type Nonce = 'AttributeType.Nonce
type Part = 'AttributeType.Part
type Popover = 'AttributeType.Popover
type Role = 'AttributeType.Role
type Slot = 'AttributeType.Slot
type Spellcheck = 'AttributeType.Spellcheck
type Style = 'AttributeType.Style
type TabIndex = 'AttributeType.TabIndex
type Title = 'AttributeType.Title
type Translate = 'AttributeType.Translate
type WritingSuggestions = 'AttributeType.WritingSuggestions

type Abbreviation = 'AttributeType.Abbreviation
type Accept = 'AttributeType.Accept
type AcceptCharset = 'AttributeType.AcceptCharset
type Action = 'AttributeType.Action
type Allow = 'AttributeType.Allow
type Alt = 'AttributeType.Alt
type As = 'AttributeType.As
type Async = 'AttributeType.Async
type Autocomplete = 'AttributeType.Autocomplete
type Autoplay = 'AttributeType.Autoplay
type Blocking = 'AttributeType.Blocking
type Capture = 'AttributeType.Capture
type Charset = 'AttributeType.Charset
type Checked = 'AttributeType.Checked
type Cite = 'AttributeType.Cite
type Cols = 'AttributeType.Cols
type Colspan = 'AttributeType.Colspan
type Content = 'AttributeType.Content
type Controls = 'AttributeType.Controls
type ControlsList = 'AttributeType.ControlsList
type Coords = 'AttributeType.Coords
type CrossOrigin = 'AttributeType.CrossOrigin
type Data = 'AttributeType.Data
type Datetime = 'AttributeType.Datetime
type Decoding = 'AttributeType.Decoding
type Default = 'AttributeType.Default
type Defer = 'AttributeType.Defer
type Dirname = 'AttributeType.Dirname
type DisablePictureInPicture = 'AttributeType.DisablePictureInPicture
type Disabled = 'AttributeType.Disabled
type DisableRemotePlayback = 'AttributeType.DisableRemotePlayback
type Download = 'AttributeType.Download
type Enctype = 'AttributeType.Enctype
type FetchPriority = 'AttributeType.FetchPriority
type For = 'AttributeType.For
type Form = 'AttributeType.Form
type FormAction = 'AttributeType.FormAction
type FormEnctype = 'AttributeType.FormEnctype
type FormMethod = 'AttributeType.FormMethod
type FormNoValidate = 'AttributeType.FormNoValidate
type FormTarget = 'AttributeType.FormTarget
type Headers = 'AttributeType.Headers
type Height = 'AttributeType.Height
type High = 'AttributeType.High
type Href = 'AttributeType.Href
type HrefLang = 'AttributeType.HrefLang
type HttpEquiv = 'AttributeType.HttpEquiv
type ImageSizes = 'AttributeType.ImageSizes
type ImageSrcset = 'AttributeType.ImageSrcset
type Integrity = 'AttributeType.Integrity
type IsMap = 'AttributeType.IsMap
type Kind = 'AttributeType.Kind
type Label = 'AttributeType.Label
type List = 'AttributeType.List
type Loading = 'AttributeType.Loading
type Loop = 'AttributeType.Loop
type Low = 'AttributeType.Low
type Max = 'AttributeType.Max
type MaxLength = 'AttributeType.MaxLength
type MinLength = 'AttributeType.MinLength
type Media = 'AttributeType.Media
type Method = 'AttributeType.Method
type Min = 'AttributeType.Min
type Multiple = 'AttributeType.Multiple
type Muted = 'AttributeType.Muted
type Name = 'AttributeType.Name
type NoModule = 'AttributeType.NoModule
type NoValidate = 'AttributeType.NoValidate
type Open = 'AttributeType.Open
type Optimum = 'AttributeType.Optimum
type Pattern = 'AttributeType.Pattern
type Ping = 'AttributeType.Ping
type Placeholder = 'AttributeType.Placeholder
type PlaysInline = 'AttributeType.PlaysInline
type PopoverTarget = 'AttributeType.PopoverTarget
type PopoverTargetAction = 'AttributeType.PopoverTargetAction
type Poster = 'AttributeType.Poster
type Preload = 'AttributeType.Preload
type ReadOnly = 'AttributeType.ReadOnly
type ReferrerPolicy = 'AttributeType.ReferrerPolicy
type Rel = 'AttributeType.Rel
type Required = 'AttributeType.Required
type Reversed = 'AttributeType.Reversed
type Rows = 'AttributeType.Rows
type Rowspan = 'AttributeType.Rowspan
type Sandbox = 'AttributeType.Sandbox
type Scope = 'AttributeType.Scope
type Selected = 'AttributeType.Selected
type Shape = 'AttributeType.Shape
type Size = 'AttributeType.Size
type Sizes = 'AttributeType.Sizes
type Span = 'AttributeType.Span
type Src = 'AttributeType.Src
type SrcDoc = 'AttributeType.SrcDoc
type SrcLang = 'AttributeType.SrcLang
type SrcSet = 'AttributeType.SrcSet
type Start = 'AttributeType.Start
type Step = 'AttributeType.Step
type Target = 'AttributeType.Target
type Type = 'AttributeType.Type
type UseMap = 'AttributeType.UseMap
type Value = 'AttributeType.Value
type Width = 'AttributeType.Width
type Wrap = 'AttributeType.Wrap
type XMLNS = 'AttributeType.XMLNS

type Htmx = 'AttributeType.Htmx
type HxGet = 'AttributeType.HxGet
type HxPost = 'AttributeType.HxPost
type HxPushURL = 'AttributeType.HxPushURL
type HxSelect = 'AttributeType.HxSelect
type HxSelectOOB = 'AttributeType.HxSelectOOB
type HxSwap = 'AttributeType.HxSwap
type HxSwapOOB = 'AttributeType.HxSwapOOB
type HxTarget = 'AttributeType.HxTarget
type HxTrigger = 'AttributeType.HxTrigger
type HxVals = 'AttributeType.HxVals
type HxBoost = 'AttributeType.HxBoost
type HxConfirm = 'AttributeType.HxConfirm
type HxDelete = 'AttributeType.HxDelete
type HxDisable = 'AttributeType.HxDisable
type HxDisabledElt = 'AttributeType.HxDisabledElt
type HxDisinherit = 'AttributeType.HxDisinherit
type HxEncoding = 'AttributeType.HxEncoding
type HxExt = 'AttributeType.HxExt
type HxHeaders = 'AttributeType.HxHeaders
type HxHistory = 'AttributeType.HxHistory
type HxHistoryElt = 'AttributeType.HxHistoryElt
type HxInclude = 'AttributeType.HxInclude
type HxIndicator = 'AttributeType.HxIndicator
type HxOn = 'AttributeType.HxOn
type HxParams = 'AttributeType.HxParams
type HxPatch = 'AttributeType.HxPatch
type HxPreserve = 'AttributeType.HxPreserve
type HxPrompt = 'AttributeType.HxPrompt
type HxPut = 'AttributeType.HxPut
type HxReplaceURL = 'AttributeType.HxReplaceURL
type HxValidate = 'AttributeType.HxValidate

type HyperScript = 'AttributeType.HyperScript
