{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HTML.Attributes.Internal
  ( Attribute
      ( Attr_NoAttribute
      , Attr_Custom
      , Attr_CustomBoolean
      , Attr_AccessKey
      , Attr_Autocapitalize
      , Attr_Autocorrect
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
      , Attr_InputMode
      , Attr_Is
      , Attr_ItemId
      , Attr_ItemProp
      , Attr_ItemRef
      , Attr_ItemScope
      , Attr_ItemType
      , Attr_Lang
      , Attr_Nonce
      , Attr_Part
      , Attr_Popover
      , Attr_Role
      , Attr_Slot
      , Attr_Spellcheck
      , Attr_Style
      , Attr_TabIndex
      , Attr_Title
      , Attr_Translate
      , Attr_WritingSuggestions

      , Attr_Abbreviation
      , Attr_Accept
      , Attr_AcceptCharset
      , Attr_Action
      , Attr_Allow
      , Attr_Alt
      , Attr_As
      , Attr_Async
      , Attr_Autocomplete
      , Attr_Autoplay
      , Attr_Blocking
      , Attr_Capture
      , Attr_Charset
      , Attr_Checked
      , Attr_Cite
      , Attr_Cols
      , Attr_Colspan
      , Attr_Command
      , Attr_CommandFor
      , Attr_Content
      , Attr_Controls
      , Attr_ControlsList
      , Attr_Coords
      , Attr_CrossOrigin
      , Attr_Data
      , Attr_Datetime
      , Attr_Decoding
      , Attr_Default
      , Attr_Defer
      , Attr_Dirname
      , Attr_Disabled
      , Attr_DisablePictureInPicture
      , Attr_DisableRemotePlayback
      , Attr_Download
      , Attr_ElementTiming
      , Attr_Enctype
      , Attr_FetchPriority
      , Attr_For
      , Attr_Form
      , Attr_FormAction
      , Attr_FormEnctype
      , Attr_FormMethod
      , Attr_FormNoValidate
      , Attr_FormTarget
      , Attr_Headers
      , Attr_Height
      , Attr_High
      , Attr_Href
      , Attr_HrefLang
      , Attr_HttpEquiv
      , Attr_ImageSizes
      , Attr_ImageSrcset
      , Attr_Integrity
      , Attr_IsMap
      , Attr_Kind
      , Attr_Label
      , Attr_List
      , Attr_Loading
      , Attr_Loop
      , Attr_Low
      , Attr_Max
      , Attr_MaxLength
      , Attr_Media
      , Attr_Method
      , Attr_Min
      , Attr_MinLength
      , Attr_Multiple
      , Attr_Muted
      , Attr_Name
      , Attr_NoModule
      , Attr_NoValidate
      , Attr_Open
      , Attr_Optimum
      , Attr_Pattern
      , Attr_Ping
      , Attr_Placeholder
      , Attr_PlaysInline
      , Attr_PopoverTarget
      , Attr_PopoverTargetAction
      , Attr_Poster
      , Attr_Preload
      , Attr_ReadOnly
      , Attr_ReferrerPolicy
      , Attr_Rel
      , Attr_Required
      , Attr_Reversed
      , Attr_Rows
      , Attr_Rowspan
      , Attr_Sandbox
      , Attr_Scope
      , Attr_Selected
      , Attr_ShadowRootMode
      , Attr_ShadowRootDelegatesFocus
      , Attr_ShadowRootClonable
      , Attr_Shape
      , Attr_Size
      , Attr_Sizes
      , Attr_Span
      , Attr_Src
      , Attr_SrcDoc
      , Attr_SrcLang
      , Attr_SrcSet
      , Attr_Start
      , Attr_Step
      , Attr_Target
      , Attr_Type
      , Attr_UseMap
      , Attr_Value
      , Attr_Width
      , Attr_Wrap
      , Attr_XMLNS

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

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Integer (Positive)
import Numeric.Natural (Natural)
import Ogma qualified

import Brigid.HTML.Attributes.AttributeType (AttributeType (..))
import Brigid.HTML.Attributes.Elements (ValidAttribute)
import Brigid.HTML.Elements.TagType (TagType)
import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types
import Brigid.Types.URL (eqRelativeURL)

data Attribute (tag :: TagType) where
  -- No Attribute
  --
  Attr_NoAttribute
    :: Attribute tag

  -- Custom Attributes
  --
  Attr_Custom
    :: T.Text
    -> T.Text
    -> Attribute tag

  Attr_CustomBoolean
    :: T.Text
    -> Bool
    -> Attribute tag

  -- Global Attributes
  --
  Attr_AccessKey
    :: Char
    -> Attribute tag

  Attr_Autocapitalize
    :: Types.AutocapitalizeOption
    -> Attribute tag

  Attr_Autocorrect
    :: Types.OnOff
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
    :: [Types.ExportPart]
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

  Attr_InputMode
    :: Types.InputMode
    -> Attribute tag

  Attr_Is
    :: T.Text
    -> Attribute tag

  Attr_ItemId
    :: T.Text
    -> Attribute tag

  Attr_ItemProp
    :: T.Text
    -> Attribute tag

  Attr_ItemRef
    :: NEL.NonEmpty Types.Id
    -> Attribute tag

  Attr_ItemScope
    :: Attribute tag

  Attr_ItemType
    :: Types.AbsoluteURL
    -> Attribute tag

  Attr_Lang
    :: Maybe Ogma.BCP_47
    -> Attribute tag

  Attr_Nonce
    :: T.Text
    -> Attribute tag

  Attr_Part
    :: [Types.Part]
    -> Attribute tag

  Attr_Popover
    :: Types.PopoverState
    -> Attribute tag

  Attr_Role
    :: Types.Role
    -> Attribute tag

  Attr_Slot
    :: Types.Name
    -> Attribute tag

  Attr_Spellcheck
    :: Bool -- Note: NOT a boolean attribute; prints string true/false
    -> Attribute tag

  Attr_Style
    :: T.Text
    -> Attribute tag

  Attr_TabIndex
    :: Integer
    -> Attribute tag

  Attr_Title
    :: T.Text
    -> Attribute tag

  Attr_Translate
    :: Types.YesNo
    -> Attribute tag

  Attr_WritingSuggestions
    :: Bool -- Note: NOT a boolean attribute; prints string true/false
    -> Attribute tag

  -- Scoped Attributes
  --
  Attr_Abbreviation
    :: ValidAttribute 'Abbreviation tag
    => T.Text
    -> Attribute tag

  Attr_Accept
    :: ValidAttribute 'Accept tag
    => BS.ByteString
    -> Attribute tag

  Attr_AcceptCharset
    :: ValidAttribute 'AcceptCharset tag
    => Attribute tag

  Attr_Action
    :: ValidAttribute 'Action tag
    => Types.Action
    -> Attribute tag

  Attr_Allow
    :: ValidAttribute 'Allow tag
    => [Types.FeaturePolicyDirective]
    -> Attribute tag

  Attr_Alt
    :: ValidAttribute 'Alt tag
    => T.Text
    -> Attribute tag

  Attr_As
    :: ValidAttribute 'As tag
    => Types.As
    -> Attribute tag

  Attr_Async
    :: ValidAttribute 'Async tag
    => Attribute tag

  Attr_Autocomplete
    :: ValidAttribute 'Autocomplete tag
    => Types.AutocompleteToken
    -> Attribute tag

  Attr_Autoplay
    :: ValidAttribute 'Autoplay tag
    => Attribute tag

  Attr_Blocking
    :: ValidAttribute 'Blocking tag
    => Types.BlockOption
    -> Attribute tag

  Attr_Capture
    :: ValidAttribute 'Capture tag
    => Maybe Types.CaptureMethod
    -> Attribute tag

  Attr_Charset
    :: ValidAttribute 'Charset tag
    => Attribute tag

  Attr_Checked
    :: ValidAttribute 'Checked tag
    => Bool
    -> Attribute tag

  Attr_Cite
    :: ValidAttribute 'Cite tag
    => Types.URL
    -> Attribute tag

  Attr_Cols
    :: ValidAttribute 'Cols tag
    => Natural
    -> Attribute tag

  Attr_Colspan
    :: ValidAttribute 'Colspan tag
    => Positive
    -> Attribute tag

  Attr_Command
    :: ValidAttribute 'Command tag
    => Types.CommandOption
    -> Attribute tag

  Attr_CommandFor
    :: ValidAttribute 'CommandFor tag
    => Types.Id
    -> Attribute tag

  Attr_Content
    :: ValidAttribute 'Content tag
    => T.Text
    -> Attribute tag

  Attr_Controls
    :: ValidAttribute 'Controls tag
    => Attribute tag

  Attr_ControlsList
    :: ValidAttribute 'ControlsList tag
    => Types.ControlsList
    -> Attribute tag

  Attr_Coords
    :: ValidAttribute 'Coords tag
    => NEL.NonEmpty Integer
    -> Attribute tag

  Attr_CrossOrigin
    :: ValidAttribute 'CrossOrigin tag
    => Types.CrossOriginFetch
    -> Attribute tag

  Attr_Data
    :: ValidAttribute 'Data tag
    => Types.URL
    -> Attribute tag

  Attr_Datetime
    :: ValidAttribute 'Datetime tag
    => String
    -> Attribute tag

  Attr_Decoding
    :: ValidAttribute 'Decoding tag
    => Types.Decoding
    -> Attribute tag

  Attr_Default
    :: ValidAttribute 'Default tag
    => Attribute tag

  Attr_Defer
    :: ValidAttribute 'Defer tag
    => Attribute tag

  Attr_Dirname
    :: ValidAttribute 'Dirname tag
    => T.Text
    -> Attribute tag

  Attr_Disabled
    :: ValidAttribute 'Disabled tag
    => Bool
    -> Attribute tag

  Attr_DisablePictureInPicture
    :: ValidAttribute 'DisablePictureInPicture tag
    => Attribute tag

  Attr_DisableRemotePlayback
    :: ValidAttribute 'DisableRemotePlayback tag
    => Attribute tag

  Attr_Download
    :: ValidAttribute 'Download tag
    => Maybe NET.NonEmptyText
    -> Attribute tag

  Attr_ElementTiming
    :: ValidAttribute 'ElementTiming tag
    => T.Text
    -> Attribute tag

  Attr_Enctype
    :: ValidAttribute 'Enctype tag
    => BS.ByteString
    -> Attribute tag

  Attr_FetchPriority
    :: ValidAttribute 'FetchPriority tag
    => Types.FetchPriority
    -> Attribute tag

  Attr_For
    :: ValidAttribute 'For tag
    => Types.ForOption
    -> Attribute tag

  Attr_Form
    :: ValidAttribute 'Form tag
    => Types.Id
    -> Attribute tag

  Attr_FormAction
    :: ValidAttribute 'FormAction tag
    => Types.Action
    -> Attribute tag

  Attr_FormEnctype
    :: ValidAttribute 'FormEnctype tag
    => BS.ByteString
    -> Attribute tag

  Attr_FormMethod
    :: ValidAttribute 'FormMethod tag
    => Types.FormMethod
    -> Attribute tag

  Attr_FormNoValidate
    :: ValidAttribute 'FormNoValidate tag
    => Attribute tag

  Attr_FormTarget
    :: ValidAttribute 'FormTarget tag
    => Types.Target
    -> Attribute tag

  Attr_Headers
    :: ValidAttribute 'Headers tag
    => [Types.Id]
    -> Attribute tag

  Attr_Height
    :: ValidAttribute 'Height tag
    => Positive
    -> Attribute tag

  Attr_High
    :: ValidAttribute 'High tag
    => Types.Number
    -> Attribute tag

  Attr_Href
    :: ValidAttribute 'Href tag
    => Types.Href
    -> Attribute tag

  Attr_HrefLang
    :: ValidAttribute 'HrefLang tag
    => Ogma.BCP_47
    -> Attribute tag

  Attr_HttpEquiv
    :: ValidAttribute 'HttpEquiv tag
    => Types.HttpEquivToken
    -> Attribute tag

  Attr_ImageSizes
    :: ValidAttribute 'ImageSizes tag
    => NEL.NonEmpty Types.Size
    -> Attribute tag

  Attr_ImageSrcset
    :: ValidAttribute 'ImageSrcset tag
    => NEL.NonEmpty Types.SrcsetCandidate
    -> Attribute tag

  Attr_Integrity
    :: ValidAttribute 'Integrity tag
    => Types.IntegrityEncoding
    -> BS.ByteString
    -> Attribute tag

  Attr_IsMap
    :: ValidAttribute 'IsMap tag
    => Attribute tag

  Attr_Kind
    :: ValidAttribute 'Kind tag
    => Types.TrackKind
    -> Attribute tag

  Attr_Label
    :: ValidAttribute 'Label tag
    => T.Text
    -> Attribute tag

  Attr_List
    :: ValidAttribute 'List tag
    => Types.Id
    -> Attribute tag

  Attr_Loading
    :: ValidAttribute 'Loading tag
    => Types.LoadOption
    -> Attribute tag

  Attr_Loop
    :: ValidAttribute 'Loop tag
    => Attribute tag

  Attr_Low
    :: ValidAttribute 'Low tag
    => Types.Number
    -> Attribute tag

  Attr_Max
    :: ValidAttribute 'Max tag
    => Types.RangeBound
    -> Attribute tag

  Attr_MaxLength
    :: ValidAttribute 'MaxLength tag
    => Natural
    -> Attribute tag

  Attr_Media
    :: ValidAttribute 'Media tag
    => NEL.NonEmpty Types.MediaQuery
    -> Attribute tag

  Attr_Method
    :: ValidAttribute 'Method tag
    => Types.FormMethod
    -> Attribute tag

  Attr_Min
    :: ValidAttribute 'Min tag
    => Types.RangeBound
    -> Attribute tag

  Attr_MinLength
    :: ValidAttribute 'MinLength tag
    => Natural
    -> Attribute tag

  Attr_Multiple
    :: ValidAttribute 'Multiple tag
    => Attribute tag

  Attr_Muted
    :: ValidAttribute 'Muted tag
    => Bool
    -> Attribute tag

  Attr_Name
    :: ValidAttribute 'Name tag
    => Types.NameOption
    -> Attribute tag

  Attr_NoModule
    :: ValidAttribute 'NoModule tag
    => Bool
    -> Attribute tag

  Attr_NoValidate
    :: ValidAttribute 'NoValidate tag
    => Bool
    -> Attribute tag

  Attr_Open
    :: ValidAttribute 'Open tag
    => Attribute tag

  Attr_Optimum
    :: ValidAttribute 'Optimum tag
    => Types.Number
    -> Attribute tag

  -- TODO: We may change this to a type with smart constructors that can guard
  -- against different JavaScript regex features that aren't supported by all
  -- browsers. This would allow us to offer users a safe `Pattern` type that has
  -- constructors guarding against KavaScript regex features they don't intend
  -- to support.
  --
  Attr_Pattern
    :: ValidAttribute 'Pattern tag
    => T.Text
    -> Attribute tag

  Attr_Ping
    :: ValidAttribute 'Ping tag
    => NEL.NonEmpty Types.Ping
    -> Attribute tag

  Attr_Placeholder
    :: ValidAttribute 'Placeholder tag
    => T.Text
    -> Attribute tag

  Attr_PlaysInline
    :: ValidAttribute 'PlaysInline tag
    => Bool
    -> Attribute tag

  Attr_PopoverTarget
    :: ValidAttribute 'PopoverTarget tag
    => Types.Id
    -> Attribute tag

  Attr_PopoverTargetAction
    :: ValidAttribute 'PopoverTargetAction tag
    => Types.PopoverTargetAction
    -> Attribute tag

  Attr_Poster
    :: ValidAttribute 'Poster tag
    => Types.URL
    -> Attribute tag

  Attr_Preload
    :: ValidAttribute 'Preload tag
    => Types.Preload
    -> Attribute tag

  Attr_ReadOnly
    :: ValidAttribute 'ReadOnly tag
    => Attribute tag

  Attr_ReferrerPolicy
    :: ValidAttribute 'ReferrerPolicy tag
    => Types.ReferrerPolicy
    -> Attribute tag

  Attr_Rel
    :: ValidAttribute 'Rel tag
    => Types.Relationship
    -> Attribute tag

  Attr_Required
    :: ValidAttribute 'Required tag
    => Bool
    -> Attribute tag

  Attr_Reversed
    :: ValidAttribute 'Reversed tag
    => Bool
    -> Attribute tag

  Attr_Rows
    :: ValidAttribute 'Rows tag
    => Natural
    -> Attribute tag

  Attr_Rowspan
    :: ValidAttribute 'Rowspan tag
    => Positive
    -> Attribute tag

  Attr_Sandbox
    :: ValidAttribute 'Sandbox tag
    => [Types.SandboxToken]
    -> Attribute tag

  Attr_Scope
    :: ValidAttribute 'Scope tag
    => Types.Scope
    -> Attribute tag

  Attr_Selected
    :: ValidAttribute 'Selected tag
    => Bool
    -> Attribute tag

  Attr_ShadowRootMode
    :: ValidAttribute 'ShadowRootMode tag
    => Types.OpenClosed
    -> Attribute tag

  Attr_ShadowRootDelegatesFocus
    :: ValidAttribute 'ShadowRootDelegatesFocus tag
    => Attribute tag

  Attr_ShadowRootClonable
    :: ValidAttribute 'ShadowRootClonable tag
    => Attribute tag

  Attr_Shape
    :: ValidAttribute 'Shape tag
    => Types.Shape
    -> Attribute tag

  Attr_Size
    :: ValidAttribute 'Size tag
    => Positive
    -> Attribute tag

  Attr_Sizes
    :: ValidAttribute 'Sizes tag
    => NEL.NonEmpty Types.Size
    -> Attribute tag

  Attr_Span
    :: ValidAttribute 'Span tag
    => Positive
    -> Attribute tag

  Attr_Src
    :: ValidAttribute 'Src tag
    => Types.URL
    -> Attribute tag

  Attr_SrcDoc
    :: ValidAttribute 'SrcDoc tag
    => LBS.ByteString
    -> Attribute tag

  Attr_SrcLang
    :: ValidAttribute 'SrcLang tag
    => Ogma.BCP_47
    -> Attribute tag

  Attr_SrcSet
    :: ValidAttribute 'SrcSet tag
    => NEL.NonEmpty Types.SrcsetCandidate
    -> Attribute tag

  Attr_Start
    :: ValidAttribute 'Start tag
    => Integer
    -> Attribute tag

  Attr_Step
    :: ValidAttribute 'Step tag
    => Types.Step
    -> Attribute tag

  Attr_Target
    :: ValidAttribute 'Target tag
    => Types.Target
    -> Attribute tag

  Attr_Type
    :: ValidAttribute 'Type tag
    => Types.TypeOption
    -> Attribute tag

  Attr_UseMap
    :: ValidAttribute 'UseMap tag
    => Types.Name
    -> Attribute tag

  Attr_Value
    :: ValidAttribute 'Value tag
    => Types.Value
    -> Attribute tag

  Attr_Width
    :: ValidAttribute 'Width tag
    => Positive
    -> Attribute tag

  Attr_Wrap
    :: ValidAttribute 'Wrap tag
    => Types.Wrap
    -> Attribute tag

  Attr_XMLNS
    :: ValidAttribute 'XMLNS tag
    => Types.URL
    -> Attribute tag

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
    :: Types.HxTarget
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

instance Eq (Attribute tag) where
  attr1 == attr2 =
    case (attr1, attr2) of
      (Attr_NoAttribute, Attr_NoAttribute) -> True
      (Attr_Custom d1 v1, Attr_Custom d2 v2) -> d1 == d2 && v1 == v2
      (Attr_CustomBoolean d1 v1, Attr_CustomBoolean d2 v2) -> d1 == d2 && v1 == v2
      (Attr_AccessKey a1, Attr_AccessKey a2) -> a1 == a2
      (Attr_Autocapitalize a1, Attr_Autocapitalize a2) -> a1 == a2
      (Attr_Autocorrect a1, Attr_Autocorrect a2) -> a1 == a2
      (Attr_Autofocus a1, Attr_Autofocus a2) -> a1 == a2
      (Attr_Class a1, Attr_Class a2) -> a1 == a2
      (Attr_ContentEditable a1, Attr_ContentEditable a2) -> a1 == a2
      (Attr_CustomData d1 v1, Attr_CustomData d2 v2) -> d1 == d2 && v1 == v2
      (Attr_Dir a1, Attr_Dir a2) -> a1 == a2
      (Attr_Draggable a1, Attr_Draggable a2) -> a1 == a2
      (Attr_EnterKeyHint a1, Attr_EnterKeyHint a2) -> a1 == a2
      (Attr_ExportParts a1, Attr_ExportParts a2) -> a1 == a2
      (Attr_Hidden a1, Attr_Hidden a2) -> a1 == a2
      (Attr_Id a1, Attr_Id a2) -> a1 == a2
      (Attr_Inert a1, Attr_Inert a2) -> a1 == a2
      (Attr_InputMode a1, Attr_InputMode a2) -> a1 == a2
      (Attr_Is a1, Attr_Is a2) -> a1 == a2
      (Attr_ItemId a1, Attr_ItemId a2) -> a1 == a2
      (Attr_ItemProp a1, Attr_ItemProp a2) -> a1 == a2
      (Attr_ItemRef a1, Attr_ItemRef a2) -> a1 == a2
      (Attr_ItemScope, Attr_ItemScope) -> True
      (Attr_ItemType a1, Attr_ItemType a2) -> a1 == a2
      (Attr_Lang a1, Attr_Lang a2) -> a1 == a2
      (Attr_Nonce a1, Attr_Nonce a2) -> a1 == a2
      (Attr_Part a1, Attr_Part a2) -> a1 == a2
      (Attr_Popover a1, Attr_Popover a2) -> a1 == a2
      (Attr_Role a1, Attr_Role a2) -> a1 == a2
      (Attr_Slot a1, Attr_Slot a2) -> a1 == a2
      (Attr_Spellcheck a1, Attr_Spellcheck a2) -> a1 == a2
      (Attr_Style a1, Attr_Style a2) -> a1 == a2
      (Attr_TabIndex a1, Attr_TabIndex a2) -> a1 == a2
      (Attr_Title a1, Attr_Title a2) -> a1 == a2
      (Attr_Translate a1, Attr_Translate a2) -> a1 == a2
      (Attr_WritingSuggestions a1, Attr_WritingSuggestions a2) -> a1 == a2
      (Attr_Abbreviation a1, Attr_Abbreviation a2) -> a1 == a2
      (Attr_Accept a1, Attr_Accept a2) -> a1 == a2
      (Attr_AcceptCharset, Attr_AcceptCharset) -> True
      (Attr_Action a1, Attr_Action a2) -> a1 == a2
      (Attr_Allow a1, Attr_Allow a2) -> a1 == a2
      (Attr_Alt a1, Attr_Alt a2) -> a1 == a2
      (Attr_As a1, Attr_As a2) -> a1 == a2
      (Attr_Async, Attr_Async) -> True
      (Attr_Autocomplete a1, Attr_Autocomplete a2) -> a1 == a2
      (Attr_Autoplay, Attr_Autoplay) -> True
      (Attr_Blocking a1, Attr_Blocking a2) -> a1 == a2
      (Attr_Capture a1, Attr_Capture a2) -> a1 == a2
      (Attr_Charset, Attr_Charset) -> True
      (Attr_Checked a1, Attr_Checked a2) -> a1 == a2
      (Attr_Cite a1, Attr_Cite a2) -> a1 == a2
      (Attr_Cols a1, Attr_Cols a2) -> a1 == a2
      (Attr_Colspan a1, Attr_Colspan a2) -> a1 == a2
      (Attr_Command a1, Attr_Command a2) -> a1 == a2
      (Attr_CommandFor a1, Attr_CommandFor a2) -> a1 == a2
      (Attr_Content a1, Attr_Content a2) -> a1 == a2
      (Attr_Controls, Attr_Controls) -> True
      (Attr_ControlsList a1, Attr_ControlsList a2) -> a1 == a2
      (Attr_Coords a1, Attr_Coords a2) -> a1 == a2
      (Attr_CrossOrigin a1, Attr_CrossOrigin a2) -> a1 == a2
      (Attr_Data a1, Attr_Data a2) -> a1 == a2
      (Attr_Datetime a1, Attr_Datetime a2) -> a1 == a2
      (Attr_Decoding a1, Attr_Decoding a2) -> a1 == a2
      (Attr_Default, Attr_Default) -> True
      (Attr_Defer, Attr_Defer) -> True
      (Attr_Dirname a1, Attr_Dirname a2) -> a1 == a2
      (Attr_Disabled a1, Attr_Disabled a2) -> a1 == a2
      (Attr_DisablePictureInPicture, Attr_DisablePictureInPicture) -> True
      (Attr_DisableRemotePlayback, Attr_DisableRemotePlayback) -> True
      (Attr_Download a1, Attr_Download a2) -> a1 == a2
      (Attr_ElementTiming a1, Attr_ElementTiming a2) -> a1 == a2
      (Attr_Enctype a1, Attr_Enctype a2) -> a1 == a2
      (Attr_FetchPriority a1, Attr_FetchPriority a2) -> a1 == a2
      (Attr_For a1, Attr_For a2) -> a1 == a2
      (Attr_Form a1, Attr_Form a2) -> a1 == a2
      (Attr_FormAction a1, Attr_FormAction a2) -> a1 == a2
      (Attr_FormEnctype a1, Attr_FormEnctype a2) -> a1 == a2
      (Attr_FormMethod a1, Attr_FormMethod a2) -> a1 == a2
      (Attr_FormNoValidate, Attr_FormNoValidate) -> True
      (Attr_FormTarget a1, Attr_FormTarget a2) -> a1 == a2
      (Attr_Headers a1, Attr_Headers a2) -> a1 == a2
      (Attr_Height a1, Attr_Height a2) -> a1 == a2
      (Attr_High a1, Attr_High a2) -> a1 == a2
      (Attr_Href a1, Attr_Href a2) -> a1 == a2
      (Attr_HrefLang a1, Attr_HrefLang a2) -> a1 == a2
      (Attr_HttpEquiv a1, Attr_HttpEquiv a2) -> a1 == a2
      (Attr_ImageSizes a1, Attr_ImageSizes a2) -> a1 == a2
      (Attr_ImageSrcset a1, Attr_ImageSrcset a2) -> a1 == a2
      (Attr_Integrity e1 i1, Attr_Integrity e2 i2) -> e1 == e2 && i1 == i2
      (Attr_IsMap, Attr_IsMap) -> True
      (Attr_Kind a1, Attr_Kind a2) -> a1 == a2
      (Attr_Label a1, Attr_Label a2) -> a1 == a2
      (Attr_List a1, Attr_List a2) -> a1 == a2
      (Attr_Loading a1, Attr_Loading a2) -> a1 == a2
      (Attr_Loop, Attr_Loop) -> True
      (Attr_Low a1, Attr_Low a2) -> a1 == a2
      (Attr_Max a1, Attr_Max a2) -> a1 == a2
      (Attr_MaxLength a1, Attr_MaxLength a2) -> a1 == a2
      (Attr_Media a1, Attr_Media a2) -> a1 == a2
      (Attr_Method a1, Attr_Method a2) -> a1 == a2
      (Attr_Min a1, Attr_Min a2) -> a1 == a2
      (Attr_MinLength a1, Attr_MinLength a2) -> a1 == a2
      (Attr_Multiple, Attr_Multiple) -> True
      (Attr_Muted a1, Attr_Muted a2) -> a1 == a2
      (Attr_Name a1, Attr_Name a2) -> a1 == a2
      (Attr_NoModule a1, Attr_NoModule a2) -> a1 == a2
      (Attr_NoValidate a1, Attr_NoValidate a2) -> a1 == a2
      (Attr_Open, Attr_Open) -> True
      (Attr_Optimum a1, Attr_Optimum a2) -> a1 == a2
      (Attr_Pattern a1, Attr_Pattern a2) -> a1 == a2
      (Attr_Ping a1, Attr_Ping a2) -> a1 == a2
      (Attr_Placeholder a1, Attr_Placeholder a2) -> a1 == a2
      (Attr_PlaysInline a1, Attr_PlaysInline a2) -> a1 == a2
      (Attr_PopoverTarget a1, Attr_PopoverTarget a2) -> a1 == a2
      (Attr_PopoverTargetAction a1, Attr_PopoverTargetAction a2) -> a1 == a2
      (Attr_Poster a1, Attr_Poster a2) -> a1 == a2
      (Attr_Preload a1, Attr_Preload a2) -> a1 == a2
      (Attr_ReadOnly, Attr_ReadOnly) -> True
      (Attr_ReferrerPolicy a1, Attr_ReferrerPolicy a2) -> a1 == a2
      (Attr_Rel a1, Attr_Rel a2) -> a1 == a2
      (Attr_Required a1, Attr_Required a2) -> a1 == a2
      (Attr_Reversed a1, Attr_Reversed a2) -> a1 == a2
      (Attr_Rows a1, Attr_Rows a2) -> a1 == a2
      (Attr_Rowspan a1, Attr_Rowspan a2) -> a1 == a2
      (Attr_Sandbox a1, Attr_Sandbox a2) -> a1 == a2
      (Attr_Scope a1, Attr_Scope a2) -> a1 == a2
      (Attr_Selected a1, Attr_Selected a2) -> a1 == a2
      (Attr_ShadowRootMode a1, Attr_ShadowRootMode a2) -> a1 == a2
      (Attr_ShadowRootDelegatesFocus, Attr_ShadowRootDelegatesFocus) -> True
      (Attr_ShadowRootClonable, Attr_ShadowRootClonable) -> True
      (Attr_Shape a1, Attr_Shape a2) -> a1 == a2
      (Attr_Size a1, Attr_Size a2) -> a1 == a2
      (Attr_Sizes a1, Attr_Sizes a2) -> a1 == a2
      (Attr_Span a1, Attr_Span a2) -> a1 == a2
      (Attr_Src a1, Attr_Src a2) -> a1 == a2
      (Attr_SrcDoc a1, Attr_SrcDoc a2) -> a1 == a2
      (Attr_SrcLang a1, Attr_SrcLang a2) -> a1 == a2
      (Attr_SrcSet a1, Attr_SrcSet a2) -> a1 == a2
      (Attr_Start a1, Attr_Start a2) -> a1 == a2
      (Attr_Step a1, Attr_Step a2) -> a1 == a2
      (Attr_Target a1, Attr_Target a2) -> a1 == a2
      (Attr_Type a1, Attr_Type a2) -> a1 == a2
      (Attr_UseMap a1, Attr_UseMap a2) -> a1 == a2
      (Attr_Value a1, Attr_Value a2) -> a1 == a2
      (Attr_Width a1, Attr_Width a2) -> a1 == a2
      (Attr_Wrap a1, Attr_Wrap a2) -> a1 == a2
      (Attr_XMLNS a1, Attr_XMLNS a2) -> a1 == a2
      (Attr_Htmx a1, Attr_Htmx a2) -> eqRelativeURL a1 a2
      (Attr_HxBoost a1, Attr_HxBoost a2) -> a1 == a2
      (Attr_HxConfirm a1, Attr_HxConfirm a2) -> a1 == a2
      (Attr_HxDisable a1, Attr_HxDisable a2) -> a1 == a2
      (Attr_HxDisabledElt a1, Attr_HxDisabledElt a2) -> a1 == a2
      (Attr_HxDisinherit a1, Attr_HxDisinherit a2) -> a1 == a2
      (Attr_HxEncoding, Attr_HxEncoding) -> True
      (Attr_HxExt a1, Attr_HxExt a2) -> a1 == a2
      (Attr_HxHeaders a1, Attr_HxHeaders a2) -> a1 == a2
      (Attr_HxHistory, Attr_HxHistory) -> True
      (Attr_HxHistoryElt, Attr_HxHistoryElt) -> True
      (Attr_HxInclude a1, Attr_HxInclude a2) -> a1 == a2
      (Attr_HxIndicator a1, Attr_HxIndicator a2) -> a1 == a2
      (Attr_HxOn e1 t1, Attr_HxOn e2 t2) -> e1 == e2 && t1 == t2
      (Attr_HxParams a1, Attr_HxParams a2) -> a1 == a2
      (Attr_HxPreserve a1, Attr_HxPreserve a2) -> a1 == a2
      (Attr_HxPrompt a1, Attr_HxPrompt a2) -> a1 == a2
      (Attr_HxPushURL a1, Attr_HxPushURL a2) -> a1 == a2
      (Attr_HxReplaceURL a1, Attr_HxReplaceURL a2) -> a1 == a2
      (Attr_HxSelect a1, Attr_HxSelect a2) -> a1 == a2
      (Attr_HxSelectOOB a1, Attr_HxSelectOOB a2) -> a1 == a2
      (Attr_HxSwap a1, Attr_HxSwap a2) -> a1 == a2
      (Attr_HxSwapOOB a1, Attr_HxSwapOOB a2) -> a1 == a2
      (Attr_HxTarget a1, Attr_HxTarget a2) -> a1 == a2
      (Attr_HxTrigger a1, Attr_HxTrigger a2) -> a1 == a2
      (Attr_HxValidate, Attr_HxValidate) -> True
      (Attr_HxVals a1, Attr_HxVals a2) -> a1 == a2
      (Attr_HyperScript a1, Attr_HyperScript a2) -> a1 == a2
      (_a1, _a2) -> False

deriving instance Show (Attribute tag)

attributeText :: Attribute tag -> T.Text
attributeText attr =
  case attr of
    Attr_NoAttribute ->
      "no_attribute"

    Attr_Custom name _value ->
      name

    Attr_CustomBoolean name _value ->
      name

    -- Global Attributes
    --

    Attr_AccessKey _key ->
      "accesskey"

    Attr_Autocapitalize _option ->
      "autocapitalize"

    Attr_Autocorrect _autocorrect ->
      "autocorrect"

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

    Attr_InputMode _inputmode ->
      "inputmode"

    Attr_Is _is ->
      "is"

    Attr_ItemId _itemid ->
      "itemid"

    Attr_ItemProp _itemprop ->
      "itemprop"

    Attr_ItemRef _itemref ->
      "itemref"

    Attr_ItemScope ->
      "itemscope"

    Attr_ItemType _itemtype ->
      "itemtype"

    Attr_Lang _lang ->
      "lang"

    Attr_Nonce _nonce ->
      "nonce"

    Attr_Part _part ->
      "part"

    Attr_Popover _state ->
      "popover"

    Attr_Role _role ->
      "role"

    Attr_Slot _slot ->
      "slot"

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

    Attr_WritingSuggestions _writingsuggestions ->
      "writingsuggestions"

    -- Scoped Attributes
    --
    Attr_Abbreviation _abbr ->
      "abbr"

    Attr_Accept _accept ->
      "accept"

    Attr_AcceptCharset ->
      "accept-charset"

    Attr_Action _action ->
      "action"

    Attr_Allow _allow ->
      "allow"

    Attr_Alt _alt ->
      "alt"

    Attr_As _as ->
      "as"

    Attr_Async ->
      "async"

    Attr_Autocomplete _autocomplete ->
      "autocomplete"

    Attr_Autoplay ->
      "autoplay"

    Attr_Blocking _blocking ->
      "blocking"

    Attr_Capture _capture ->
      "capture"

    Attr_Charset ->
      "charset"

    Attr_Checked _checked ->
      "checked"

    Attr_Cite _cite ->
      "cite"

    Attr_Cols _cols ->
      "cols"

    Attr_Colspan _colspan ->
      "colspan"

    Attr_Command _command ->
      "command"

    Attr_CommandFor _commandfor ->
      "commandfor"

    Attr_Content _content ->
      "content"

    Attr_Controls ->
      "controls"

    Attr_ControlsList _controlslist ->
      "controlslist"

    Attr_Coords _coords ->
      "coords"

    Attr_CrossOrigin _crossorigin ->
      "crossorigin"

    Attr_Data _data ->
      "data"

    Attr_Datetime _datetime ->
      "datetime"

    Attr_Decoding _decoding ->
      "decoding"

    Attr_Default ->
      "default"

    Attr_Defer ->
      "defer"

    Attr_Dirname _dirname ->
      "dirname"

    Attr_Disabled _disabled ->
      "disabled"

    Attr_DisablePictureInPicture ->
      "disablepictureinpicture"

    Attr_DisableRemotePlayback ->
      "disableremoteplayback"

    Attr_Download _download ->
      "download"

    Attr_ElementTiming _elementtiming ->
      "elementtiming"

    Attr_Enctype _enctype ->
      "enctype"

    Attr_FetchPriority _fetchpriority ->
      "fetchpriority"

    Attr_For _for ->
      "for"

    Attr_Form _form ->
      "form"

    Attr_FormAction _formaction ->
      "formaction"

    Attr_FormEnctype _enctype ->
      "formenctype"

    Attr_FormMethod _formmethod ->
      "formmethod"

    Attr_FormNoValidate ->
      "formnovalidate"

    Attr_FormTarget _formtarget ->
      "formtarget"

    Attr_Headers _headers ->
      "headers"

    Attr_Height _height ->
      "height"

    Attr_High _high ->
      "high"

    Attr_Href _href ->
      "href"

    Attr_HrefLang _hreflang ->
      "hreflang"

    Attr_HttpEquiv _httpEquiv ->
      "http-equiv"

    Attr_ImageSizes _imagesizes ->
      "imagesizes"

    Attr_ImageSrcset _imagesrcset ->
      "imagesrcset"

    Attr_Integrity _sha _content ->
      "integrity"

    Attr_IsMap ->
      "ismap"

    Attr_Kind _kind ->
      "kind"

    Attr_Label _label ->
      "label"

    Attr_List _list ->
      "list"

    Attr_Loading _loading ->
      "loading"

    Attr_Loop ->
      "loop"

    Attr_Low _low ->
      "low"

    Attr_Max _max ->
      "max"

    Attr_MaxLength _maxlength ->
      "maxlength"

    Attr_Media _media ->
      "media"

    Attr_Method _method ->
      "method"

    Attr_Min _min ->
      "min"

    Attr_MinLength _minlength ->
      "minlength"

    Attr_Multiple ->
      "multiple"

    Attr_Muted _muted ->
      "muted"

    Attr_Name _name ->
      "name"

    Attr_NoModule _nomodule ->
      "nomodule"

    Attr_NoValidate _novalidate ->
      "novalidate"

    Attr_Open ->
      "open"

    Attr_Optimum _optimum ->
      "optimum"

    Attr_Pattern _pattern ->
      "pattern"

    Attr_Ping _ping ->
      "ping"

    Attr_Placeholder _placeholder ->
      "placeholder"

    Attr_PlaysInline _playsinline ->
      "playsinline"

    Attr_PopoverTarget _popovertarget ->
      "popovertarget"

    Attr_PopoverTargetAction _popovertargetaction ->
      "popovertargetaction"

    Attr_Poster _poster ->
      "poster"

    Attr_Preload _preload ->
      "preload"

    Attr_ReadOnly ->
      "readonly"

    Attr_ReferrerPolicy _referrerpolicy ->
      "referrerpolicy"

    Attr_Rel _rel ->
      "rel"

    Attr_Required _required ->
      "required"

    Attr_Reversed _reversed ->
      "reversed"

    Attr_Rows _rows ->
      "rows"

    Attr_Rowspan _rowspan ->
      "rowspan"

    Attr_Sandbox _sandbox ->
      "sandbox"

    Attr_Scope _scope ->
      "scope"

    Attr_Selected _selected ->
      "selected"

    Attr_ShadowRootMode _shadowrootmode ->
      "shadowrootmode"

    Attr_ShadowRootDelegatesFocus ->
      "shadowrootdelegatesfocus"

    Attr_ShadowRootClonable ->
      "shadowrootclonable"

    Attr_Shape _shape ->
      "shape"

    Attr_Size _size ->
      "size"

    Attr_Sizes _sizes ->
      "sizes"

    Attr_Span _span ->
      "span"

    Attr_Src _src ->
      "src"

    Attr_SrcDoc _srcdoc ->
      "srcdoc"

    Attr_SrcLang _srclang ->
      "srclang"

    Attr_SrcSet _srcset ->
      "srcset"

    Attr_Start _start ->
      "start"

    Attr_Step _step ->
      "step"

    Attr_Target _target ->
      "target"

    Attr_Type _type ->
      "type"

    Attr_UseMap _usemap ->
      "usemap"

    Attr_Value _value ->
      "value"

    Attr_Width _width ->
      "width"

    Attr_Wrap _wrap ->
      "wrap"

    Attr_XMLNS _xmlns ->
      "xmlns"

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
