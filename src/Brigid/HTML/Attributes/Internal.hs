{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HTML.Attributes.Internal
  ( Attribute
      ( Attr_NoAttribute
      , Attr_Custom
      , Attr_AccessKey
      , Attr_Autocapitalize
      , Attr_Autofocus
      , Attr_Class
      , Attr_ContentEditable
      , Attr_CustomData
      , Attr_Dir
      , Attr_Draggable
      , Attr_EnterKeyHint
      , Attr_ExportParts
      , Attr_Hidden
      , Attr_Id
      , Attr_Inert
   -- , Attr_InputMode
      , Attr_Is
   -- , Attr_ItemId
   -- , Attr_ItemProp
   -- , Attr_ItemRef
   -- , Attr_ItemScope
   -- , Attr_ItemType
      , Attr_Lang
   -- , Attr_Nonce
      , Attr_Part
      , Attr_Popover
   -- , Attr_Role
   -- , Attr_Slot
      , Attr_Spellcheck
      , Attr_Style
      , Attr_TabIndex
      , Attr_Title
      , Attr_Translate

      , Attr_Async
      , Attr_Autoplay
      , Attr_Charset
      , Attr_Content
      , Attr_CrossOrigin
      , Attr_Defer
      , Attr_Disabled
      , Attr_Headers
      , Attr_Height
      , Attr_Href
      , Attr_IsMap
      , Attr_MaxLength
      , Attr_MinLength
      , Attr_Name
      , Attr_NoModule
      , Attr_Ping
      , Attr_ReferrerPolicy
      , Attr_Rel
      , Attr_Src
      , Attr_Width

      , Attr_Htmx
      , Attr_HxBoost
      , Attr_HxConfirm
      , Attr_HxDisable
      , Attr_HxDisabledElt
      , Attr_HxDisinherit
      , Attr_HxEncoding
      , Attr_HxExt
      , Attr_HxHeaders
      , Attr_HxHistory
      , Attr_HxHistoryElt
      , Attr_HxInclude
      , Attr_HxIndicator
      , Attr_HxOn
      , Attr_HxParams
      , Attr_HxPreserve
      , Attr_HxPrompt
      , Attr_HxPushURL
      , Attr_HxReplaceURL
      , Attr_HxSelect
      , Attr_HxSelectOOB
      , Attr_HxSwap
      , Attr_HxSwapOOB
      , Attr_HxTarget
      , Attr_HxTrigger
      , Attr_HxValidate
      , Attr_HxVals

      , Attr_HyperScript
      )
  , attributeText
  ) where

import Data.LanguageCodes (ISO639_1)
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T

import Brigid.HTML.Attributes.AttributeType (AttributeType(..))
import Brigid.HTML.Attributes.Elements (ValidAttribute)
import Brigid.HTML.Elements.TagType (TagType)
import Brigid.HTML.Types qualified as Types

data Attribute (tag :: TagType) where
  -- No Attribute
  --
  Attr_NoAttribute
    :: Attribute tag

  -- Custom Attribute
  --
  Attr_Custom
    :: T.Text
    -> T.Text
    -> Attribute tag

  -- Global Attributes
  --
  Attr_AccessKey
    :: Char
    -> Attribute tag

  Attr_Autocapitalize
    :: Types.AutocapitalizeOption
    -> Attribute tag

  Attr_Autofocus
    :: Bool
    -> Attribute tag

  Attr_Class
    :: Types.Class
    -> Attribute tag

  Attr_ContentEditable
    :: Types.ContentEditableOption
    -> Attribute tag

  Attr_CustomData
    :: T.Text
    -> T.Text
    -> Attribute tag

  Attr_Dir
    :: Types.Directionality
    -> Attribute tag

  Attr_Draggable
    :: Bool -- Note: NOT a boolean attribute; prints string true/false
    -> Attribute tag

  Attr_EnterKeyHint
    :: Types.KeyHintOption
    -> Attribute tag

  Attr_ExportParts
    :: NEL.NonEmpty Types.ExportPart
    -> Attribute tag

  Attr_Hidden
    :: Bool
    -> Attribute tag

  Attr_Id
    :: Types.Id
    -> Attribute tag

  Attr_Inert
    :: Bool
    -> Attribute tag

  -- Attr_InputMode
  --   :: Types.InputMode
  --   -> Attribute tag

  Attr_Is
    :: T.Text
    -> Attribute tag

  -- Attr_ItemId
  --   :: T.Text -- TODO
  --   -> Attribute tag

  -- Attr_ItemProp
  --   :: T.Text -- TODO
  --   -> Attribute tag

  -- Attr_ItemRef
  --   :: T.Text -- TODO
  --   -> Attribute tag

  -- Attr_ItemScope
  --   :: T.Text -- TODO
  --   -> Attribute tag

  -- Attr_ItemType
  --   :: T.Text -- TODO
  --   -> Attribute tag

  Attr_Lang
    :: Maybe ISO639_1
    -> Attribute tag

  -- Attr_Nonce
  --   :: T.Text -- TODO
  --   -> Attribute tag

  Attr_Part
    :: NEL.NonEmpty Types.Part
    -> Attribute tag

  Attr_Popover
    :: Types.PopoverState
    -> Attribute tag

  -- Attr_Role
  --   :: T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Slot
  --   :: T.Text -- TODO
  --   -> Attribute tag

  Attr_Spellcheck
    :: Bool -- Note: NOT a boolean attribute; prints string true/false
    -> Attribute tag

  Attr_Style
    :: T.Text
    -> Attribute tag

  Attr_TabIndex
    :: Int
    -> Attribute tag

  Attr_Title
    :: T.Text
    -> Attribute tag

  Attr_Translate
    :: Bool -- Note: NOT a boolean attribute; prints string true/false
    -> Attribute tag

  -- Scoped Attributes
  --
  -- Attr_Accept
  --   :: ValidAttribute 'Accept tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_AcceptCharset
  --   :: ValidAttribute 'AcceptCharset tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Action
  --   :: ValidAttribute 'Action tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Allow
  --   :: ValidAttribute 'Allow tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Alt
  --   :: ValidAttribute 'Alt tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Async
    :: ValidAttribute 'Async tag
    => Attribute tag

  -- Attr_Autocomplete
  --   :: ValidAttribute 'Autocomplete tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Autoplay
    :: ValidAttribute 'Autoplay tag
    => Attribute tag

  -- Attr_Background
  --   :: ValidAttribute 'Background tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_BackgroundColor
  --   :: ValidAttribute 'BackgroundColor tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Border
  --   :: ValidAttribute 'Border tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Capture
  --   :: ValidAttribute 'Capture tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Charset
    :: ValidAttribute 'Charset tag
    => Attribute tag

  -- Attr_Checked
  --   :: ValidAttribute 'Checked tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Cite
  --   :: ValidAttribute 'Cite tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Color
  --   :: ValidAttribute 'Color tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Cols
  --   :: ValidAttribute 'Cols tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Colspan
  --   :: ValidAttribute 'Colspan tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Content
    :: ValidAttribute 'Content tag
    => T.Text
    -> Attribute tag

  -- Attr_Controls
  --   :: ValidAttribute 'Controls tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Coords
  --   :: ValidAttribute 'Coords tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_CrossOrigin
    :: ValidAttribute 'CrossOrigin tag
    => Types.CrossOriginFetch
    -> Attribute tag

  -- Attr_Data
  --   :: ValidAttribute 'Data tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Datetime
  --   :: ValidAttribute 'Datetime tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Decoding
  --   :: ValidAttribute 'Decoding tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Default
  --   :: ValidAttribute 'Default tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Defer
    :: ValidAttribute 'Defer tag
    => Attribute tag

  -- Attr_Dirname
  --   :: ValidAttribute 'Dirname tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Disabled
    :: ValidAttribute 'Disabled tag
    => Bool
    -> Attribute tag

  -- Attr_Download
  --   :: ValidAttribute 'Download tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Enctype
  --   :: ValidAttribute 'Enctype tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_For
  --   :: ValidAttribute 'For tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Form
  --   :: ValidAttribute 'Form tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_FormAction
  --   :: ValidAttribute 'FormAction tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_FormEnctype
  --   :: ValidAttribute 'FormEnctype tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_FormMethod
  --   :: ValidAttribute 'FormMethod tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_FormNoValidate
  --   :: ValidAttribute 'FormNoValidate tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_FormTarget
  --   :: ValidAttribute 'FormTarget tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Headers
    :: ValidAttribute 'Headers tag
    => NEL.NonEmpty Types.Id
    -> Attribute tag

  Attr_Height
    :: ValidAttribute 'Height tag
    => Word
    -> Attribute tag

  -- Attr_High
  --   :: ValidAttribute 'High tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Href
    :: ValidAttribute 'Href tag
    => Types.Href Types.Get
    -> Attribute tag

  -- Attr_HrefLang
  --   :: ValidAttribute 'HrefLang tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_HttpEquiv
  --   :: ValidAttribute 'HttpEquiv tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Integrity
  --   :: ValidAttribute 'Integrity tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_IsMap
    :: ValidAttribute 'IsMap tag
    => Attribute tag

  -- Attr_Kind
  --   :: ValidAttribute 'Kind tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Label
  --   :: ValidAttribute 'Label tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_List
  --   :: ValidAttribute 'List tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Loop
  --   :: ValidAttribute 'Loop tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Low
  --   :: ValidAttribute 'Low tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Max
  --   :: ValidAttribute 'Max tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_MaxLength
    :: ValidAttribute 'MaxLength tag
    => Word
    -> Attribute tag

  Attr_MinLength
    :: ValidAttribute 'MinLength tag
    => Word
    -> Attribute tag

  -- Attr_Media
  --   :: ValidAttribute 'Media tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Method
  --   :: ValidAttribute 'Method tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Min
  --   :: ValidAttribute 'Min tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Multiple
  --   :: ValidAttribute 'Multiple tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Muted
  --   :: ValidAttribute 'Muted tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Name
    :: ValidAttribute 'Name tag
    => T.Text
    -> Attribute tag

  Attr_NoModule
    :: ValidAttribute 'NoModule tag
    => Bool
    -> Attribute tag

  -- Attr_NoValidate
  --   :: ValidAttribute 'NoValidate tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Open
  --   :: ValidAttribute 'Open tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Optimum
  --   :: ValidAttribute 'Optimum tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Pattern
  --   :: ValidAttribute 'Pattern tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Ping
    :: ValidAttribute 'Ping tag
    => NEL.NonEmpty Types.Ping
    -> Attribute tag

  -- Attr_Placeholder
  --   :: ValidAttribute 'Placeholder tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_PlaysInline
  --   :: ValidAttribute 'PlaysInline tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Poster
  --   :: ValidAttribute 'Poster tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Preload
  --   :: ValidAttribute 'Preload tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_ReadOnly
  --   :: ValidAttribute 'ReadOnly tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_ReferrerPolicy
    :: ValidAttribute 'ReferrerPolicy tag
    => Types.ReferrerPolicy
    -> Attribute tag

  Attr_Rel
    :: ValidAttribute 'Rel tag
    => Types.Relationship
    -> Attribute tag

  -- Attr_Required
  --   :: ValidAttribute 'Required tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Reversed
  --   :: ValidAttribute 'Reversed tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Rows
  --   :: ValidAttribute 'Rows tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Rowspan
  --   :: ValidAttribute 'Rowspan tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Sandbox
  --   :: ValidAttribute 'Sandbox tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Scope
  --   :: ValidAttribute 'Scope tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Selected
  --   :: ValidAttribute 'Selected tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Shape
  --   :: ValidAttribute 'Shape tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Size
  --   :: ValidAttribute 'Size tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Sizes
  --   :: ValidAttribute 'Sizes tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Span
  --   :: ValidAttribute 'Span tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Src
    :: ValidAttribute 'Src tag
    => Types.URL
    -> Attribute tag

  -- Attr_SrcDoc
  --   :: ValidAttribute 'SrcDoc tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_SrcLang
  --   :: ValidAttribute 'SrcLang tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_SrcSet
  --   :: ValidAttribute 'SrcSet tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Start
  --   :: ValidAttribute 'Start tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Step
  --   :: ValidAttribute 'Step tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Target
  --   :: ValidAttribute 'Target tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Type
  --   :: ValidAttribute 'Type tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_UseMap
  --   :: ValidAttribute 'UseMap tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- Attr_Value
  --   :: ValidAttribute 'Value tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  Attr_Width
    :: ValidAttribute 'Width tag
    => Word
    -> Attribute tag

  -- Attr_Wrap
  --   :: ValidAttribute 'Wrap tag
  --   => T.Text -- TODO
  --   -> Attribute tag

  -- HTMX Attributes
  --
  Attr_Htmx
    :: Types.RelativeURL method
    -> Attribute tag

  Attr_HxBoost
    :: Bool -- Note: NOT a boolean attribute; prints string true/false
    -> Attribute tag

  Attr_HxConfirm
    :: T.Text
    -> Attribute tag

  Attr_HxDisable
    :: Bool
    -> Attribute tag

  Attr_HxDisabledElt
    :: NEL.NonEmpty Types.DisabledSelector
    -> Attribute tag

  Attr_HxDisinherit
    :: Types.Disinherit
    -> Attribute tag

  Attr_HxEncoding
    :: Attribute tag

  Attr_HxExt
    :: NEL.NonEmpty Types.Extension
    -> Attribute tag

  Attr_HxHeaders
    :: Types.HtmxHeaders
    -> Attribute tag

  Attr_HxHistory
    :: Attribute tag

  Attr_HxHistoryElt
    :: Attribute tag

  Attr_HxInclude
    :: Types.IncludeSelector
    -> Attribute tag

  Attr_HxIndicator
    :: Types.Indicator
    -> Attribute tag

  Attr_HxOn
    :: Types.Event
    -> T.Text
    -> Attribute tag

  Attr_HxParams
    :: Types.RequestParams
    -> Attribute tag

  Attr_HxPreserve
    :: Bool
    -> Attribute tag

  Attr_HxPrompt
    :: T.Text
    -> Attribute tag

  Attr_HxPushURL
    :: Types.PushURL
    -> Attribute tag

  Attr_HxReplaceURL
    :: Types.PushURL
    -> Attribute tag

  Attr_HxSelect
    :: Types.QuerySelector
    -> Attribute tag

  Attr_HxSelectOOB
    :: NEL.NonEmpty Types.OutOfBandSelect
    -> Attribute tag

  Attr_HxSwap
    :: Types.Swap
    -> Attribute tag

  Attr_HxSwapOOB
    :: Maybe Types.OutOfBandSwap
    -> Attribute tag

  Attr_HxTarget
    :: Types.Target
    -> Attribute tag

  Attr_HxTrigger
    :: NEL.NonEmpty Types.Trigger
    -> Attribute tag

  Attr_HxValidate
    :: ValidAttribute 'HxValidate tag
    => Attribute tag

  Attr_HxVals
    :: Types.HtmxVals
    -> Attribute tag

  -- Other
  --
  Attr_HyperScript
    :: Types.HyperScript
    -> Attribute tag

attributeText :: Attribute tag -> T.Text
attributeText attr =
  case attr of
    Attr_NoAttribute ->
      "no_attribute"

    Attr_Custom name _value ->
      name

    -- Global Attributes
    --

    Attr_AccessKey _key ->
      "accesskey"

    Attr_Autocapitalize _option ->
      "autocapitalize"

    Attr_Autofocus _autofocus ->
      "autofocus"

    Attr_Class _class ->
      "class"

    Attr_ContentEditable _option ->
      "contenteditable"

    Attr_CustomData data_ _value ->
      "data-" <> data_

    Attr_Dir _directionality ->
      "dir"

    Attr_Draggable _draggable ->
      "draggable"

    Attr_EnterKeyHint _option ->
      "enterkeyhint"

    Attr_ExportParts _parts ->
      "exportparts"

    Attr_Hidden _hidden ->
      "hidden"

    Attr_Id _id ->
      "id"

    Attr_Inert _inert ->
      "inert"

    -- Attr_InputMode _mode ->
    --   "inputmode"

    Attr_Is _is ->
      "is"

    -- Attr_ItemId

    -- Attr_ItemProp

    -- Attr_ItemRef

    -- Attr_ItemScope

    -- Attr_ItemType

    -- TODO: This is a minimal definition. Proper representation of BCP-47
    -- language codes will require A LOT more work.
    --
    Attr_Lang _lang ->
      "lang"

    -- Attr_Nonce

    Attr_Part _part ->
      "part"

    Attr_Popover _state ->
      "popover"

    -- Attr_Role

    -- Attr_Slot

    Attr_Spellcheck _spellcheck ->
      "spellcheck"

    Attr_Style _style ->
      "style"

    Attr_TabIndex _tabindex ->
      "tabindex"

    Attr_Title _title ->
      "title"

    Attr_Translate _translate ->
      "translate"

    -- Scoped Attributes
    --
    Attr_Async ->
      "async"

    Attr_Autoplay ->
      "autoplay"

    Attr_Charset ->
      "charset"

    Attr_Content _content ->
      "content"

    Attr_CrossOrigin _crossorigin ->
      "crossorigin"

    Attr_Defer ->
      "defer"

    Attr_Disabled _disabled ->
      "disabled"

    Attr_Headers _headers ->
      "headers"

    Attr_Height _height ->
      "height"

    Attr_Href _href ->
      "href"

    Attr_IsMap ->
      "ismap"

    Attr_MaxLength _maxlength ->
      "maxlength"

    Attr_MinLength _minlength ->
      "minlength"

    Attr_Name _name ->
      "name"

    Attr_NoModule _nomodule ->
      "nomodule"

    Attr_Ping _ping ->
      "ping"

    Attr_ReferrerPolicy _referrerpolicy ->
      "referrerpolicy"

    Attr_Rel _rel ->
      "rel"

    Attr_Src _src ->
      "src"

    Attr_Width _width ->
      "width"

    -- HTMX Attributes
    --
    Attr_Htmx _url ->
      "htmx"

    Attr_HxBoost _boosted ->
      "hx-boost"

    Attr_HxConfirm _confirmation ->
      "hx-confirm"

    Attr_HxDisable _disabled ->
      "hx-disable"

    Attr_HxDisabledElt _disabled ->
      "hx-disabled-elt"

    Attr_HxDisinherit _disinherit ->
      "hx-disinherit"

    Attr_HxEncoding ->
      "hx-encoding"

    Attr_HxExt _exts ->
      "hx-ext"

    Attr_HxHeaders _headers ->
      "hx-headers"

    Attr_HxHistory ->
      "hx-history"

    Attr_HxHistoryElt ->
      "hx-history-elt"

    Attr_HxInclude _include ->
      "hx-include"

    Attr_HxIndicator _indicator ->
      "hx-indicator"

    Attr_HxOn event _action ->
      "hx-on" <> Types.hxOnEventText event

    Attr_HxParams _params ->
      "hx-params"

    Attr_HxPreserve _preserved ->
      "hx-preserve"

    Attr_HxPrompt _prompt ->
      "hx-prompt"

    Attr_HxPushURL _url ->
      "hx-push-url"

    Attr_HxReplaceURL _url ->
      "hx-replace-url"

    Attr_HxSelect _selector ->
      "hx-select"

    Attr_HxSelectOOB _selects ->
      "hx-select-oob"

    Attr_HxSwap _swap ->
      "hx-swap"

    Attr_HxSwapOOB _mbSwap ->
      "hx-swap-oob"

    Attr_HxTarget _target ->
      "hx-target"

    Attr_HxTrigger _triggers ->
      "hx-trigger"

    Attr_HxValidate ->
      "hx-validate"

    Attr_HxVals _vals ->
      "hx-vals"

    -- Other
    --
    Attr_HyperScript _hyperscript ->
      "_hyperscript"
