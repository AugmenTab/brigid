{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.QuerySelector
  ( QuerySelector
  , QuerySelectorTypes
  , mkQuerySelector
  , unQuerySelector
  , querySelectorToBytes
  , querySelectorToText
  , RawSelector (RawSelector)
  , rawSelectorToBytes
  , rawSelectorToText
  , ElementSelector
  , elementSelectorToBytes
  , elementSelectorToText
  , tag_custom
  , tag_a
  , tag_abbr
  , tag_address
  , tag_area
  , tag_article
  , tag_aside
  , tag_audio
  , tag_b
  , tag_base
  , tag_bdi
  , tag_bdo
  , tag_blockquote
  , tag_body
  , tag_br
  , tag_button
  , tag_canvas
  , tag_caption
  , tag_cite
  , tag_code
  , tag_col
  , tag_colgroup
  , tag_data
  , tag_datalist
  , tag_dd
  , tag_del
  , tag_details
  , tag_dfn
  , tag_dialog
  , tag_div
  , tag_dl
  , tag_dt
  , tag_em
  , tag_embed
  , tag_fieldset
  , tag_figcaption
  , tag_figure
  , tag_footer
  , tag_form
  , tag_h1
  , tag_h2
  , tag_h3
  , tag_h4
  , tag_h5
  , tag_h6
  , tag_head
  , tag_header
  , tag_hgroup
  , tag_hr
  , tag_html
  , tag_i
  , tag_iframe
  , tag_img
  , tag_input
  , tag_ins
  , tag_kbd
  , tag_label
  , tag_legend
  , tag_li
  , tag_link
  , tag_main
  , tag_map
  , tag_mark
  , tag_menu
  , tag_meta
  , tag_meter
  , tag_nav
  , tag_noscript
  , tag_object
  , tag_ol
  , tag_optgroup
  , tag_option
  , tag_output
  , tag_p
  , tag_picture
  , tag_pre
  , tag_progress
  , tag_q
  , tag_rp
  , tag_rt
  , tag_ruby
  , tag_s
  , tag_sample
  , tag_script
  , tag_search
  , tag_section
  , tag_select
  , tag_slot
  , tag_small
  , tag_source
  , tag_span
  , tag_strong
  , tag_style
  , tag_sub
  , tag_summary
  , tag_sup
  , tag_table
  , tag_tbody
  , tag_td
  , tag_template
  , tag_textarea
  , tag_tfoot
  , tag_th
  , tag_thead
  , tag_time
  , tag_title
  , tag_tr
  , tag_track
  , tag_u
  , tag_ul
  , tag_var
  , tag_video
  , tag_wbr
  , AttributeSelector
  , attributeSelectorToBytes
  , attributeSelectorToText
  , attr_custom
  , attr_accesskey
  , attr_autocapitalize
  , attr_autofocus
  , attr_blocking
  , attr_class
  , attr_contenteditable
  , attr_customData
  , attr_dir
  , attr_draggable
  , attr_enterkeyhint
  , attr_exportparts
  , attr_hidden
  , attr_id
  , attr_inert
  , attr_inputmode
  , attr_is
  , attr_itemid
  , attr_itemprop
  , attr_itemref
  , attr_itemscope
  , attr_itemtype
  , attr_lang
  , attr_nonce
  , attr_part
  , attr_popover
  , attr_role
  , attr_slot
  , attr_spellcheck
  , attr_style
  , attr_tabindex
  , attr_title
  , attr_translate
  , attr_writingsuggestions
  , attr_abbr
  , attr_accept
  , attr_acceptCharset
  , attr_action
  , attr_allow
  , attr_alt
  , attr_as
  , attr_async
  , attr_autocomplete
  , attr_autoplay
  , attr_capture
  , attr_charset
  , attr_checked
  , attr_cite
  , attr_cols
  , attr_colspan
  , attr_content
  , attr_controls
  , attr_controlslist
  , attr_coords
  , attr_crossorigin
  , attr_data
  , attr_datetime
  , attr_datetimeWithFormat
  , attr_decoding
  , attr_default
  , attr_defer
  , attr_dirname
  , attr_disabled
  , attr_disablepictureinpicture
  , attr_disableremoteplayback
  , attr_download
  , attr_enctype
  , attr_fetchpriority
  , attr_for
  , attr_form
  , attr_formaction
  , attr_formenctype
  , attr_formmethod
  , attr_formnovalidate
  , attr_formtarget
  , attr_headers
  , attr_height
  , attr_high
  , attr_href
  , attr_hreflang
  , attr_httpEquiv
  , attr_imagesizes
  , attr_imagesrcset
  , attr_integrity
  , attr_ismap
  , attr_kind
  , attr_label
  , attr_list
  , attr_loading
  , attr_loop
  , attr_low
  , attr_max
  , attr_maxlength
  , attr_media
  , attr_method
  , attr_min
  , attr_minlength
  , attr_multiple
  , attr_muted
  , attr_name
  , attr_nomodule
  , attr_novalidate
  , attr_open
  , attr_optimum
  , attr_pattern
  , attr_ping
  , attr_placeholder
  , attr_playsinline
  , attr_popovertarget
  , attr_popovertargetaction
  , attr_poster
  , attr_preload
  , attr_readonly
  , attr_referrerpolicy
  , attr_rel
  , attr_required
  , attr_reversed
  , attr_rows
  , attr_rowspan
  , attr_sandbox
  , attr_scope
  , attr_selected
  , attr_shape
  , attr_size
  , attr_sizes
  , attr_span
  , attr_src
  , attr_srcdoc
  , attr_srclang
  , attr_srcset
  , attr_start
  , attr_step
  , attr_target
  , attr_type
  , attr_usemap
  , attr_value
  , attr_width
  , attr_wrap
  , attr_xmlns
  , attr_hxGet
  , attr_hxPost
  , attr_hxOn
  , attr_hxPushURL
  , attr_hxSelect
  , attr_hxSelectOOB
  , attr_hxSwap
  , attr_hxSwapOOB
  , attr_hxTarget
  , attr_hxTrigger
  , attr_hxVals
  , attr_hxBoost
  , attr_hxConfirm
  , attr_hxDelete
  , attr_hxDisable
  , attr_hxDisabledElt
  , attr_hxDisinherit
  , attr_hxEncoding
  , attr_hxExt
  , attr_hxHeaders
  , attr_hxHistory
  , attr_hxHistoryElt
  , attr_hxInclude
  , attr_hxIndicator
  , attr_hxParams
  , attr_hxPatch
  , attr_hxPreserve
  , attr_hxPrompt
  , attr_hxPut
  , attr_hxReplaceURL
  , attr_hxRequest
  , attr_hxSync
  , attr_hxValidate
  , AttributeType
      ( Attr_CustomAttribute
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
      , Attr_Background
      , Attr_BackgroundColor
      , Attr_Blocking
      , Attr_Border
      , Attr_Capture
      , Attr_Charset
      , Attr_Checked
      , Attr_Cite
      , Attr_Color
      , Attr_Cols
      , Attr_Colspan
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
      , Attr_MinLength
      , Attr_Media
      , Attr_Method
      , Attr_Min
      , Attr_Multiple
      , Attr_Muted
      , Attr_Name
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
      , Attr_HxGet
      , Attr_HxPost
      , Attr_HxOn
      , Attr_HxPushURL
      , Attr_HxSelect
      , Attr_HxSelectOOB
      , Attr_HxSwap
      , Attr_HxSwapOOB
      , Attr_HxTarget
      , Attr_HxTrigger
      , Attr_HxVals
      , Attr_HxBoost
      , Attr_HxConfirm
      , Attr_HxDelete
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
      , Attr_HxParams
      , Attr_HxPatch
      , Attr_HxPreserve
      , Attr_HxPrompt
      , Attr_HxPut
      , Attr_HxReplaceURL
      , Attr_HxRequest
      , Attr_HxSync
      , Attr_HxValidate
      , Attr_HyperScript
      )
  , attributeTypeFromText
  , attributeTypeToText
  , DisabledSelector
  , disableThis
  , disableClosest
  , disabledSelector
  , disabledSelectorToBytes
  , disabledSelectorToText
  , IncludeSelector
  , includeThis
  , includeTarget
  , includeSelector
  , includeSelectorToBytes
  , includeSelectorToText
  , Indicator
  , indicateClosest
  , indicateSelector
  , indicatorToBytes
  , indicatorToText
  , HxTarget
  , HxTargetTypes
  , mkHxTarget
  , unHxTarget
  , hxTargetToBytes
  , hxTargetToText
  , Swap
  , SwapTypes
  , mkSwap
  , unSwap
  , swapToBytes
  , swapToText
  , swapInnerHTML
  , swapOuterHTML
  , swapBeforebegin
  , swapAfterbegin
  , swapBeforeend
  , swapAfterend
  , swapDelete
  , SwapDisplay
  , SwapDisplayTargetTypes
  , swapDisplayToBytes
  , swapDisplayToText
  , scroll
  , show
  , SwapDisplayType
      ( ScrollTo
      , Show
      )
  , swapDisplayTypeToBytes
  , swapDisplayTypeToText
  , SwapDisplayView
      ( SwapTop
      , SwapBottom
      )
  , swapDisplayViewToBytes
  , swapDisplayViewToText
  , RawSwap (RawSwap)
  , rawSwapToBytes
  , rawSwapToText
  , SwapSelector
  , swapSelectorToBytes
  , swapSelectorToText
  , selectSwapToBytes
  , selectSwapToText
  , swapSelectInnerHTML
  , swapSelectOuterHTML
  , swapSelectBeforebegin
  , swapSelectAfterbegin
  , swapSelectBeforeend
  , swapSelectAfterend
  , swapSelectDelete
  , swapSelectNone
  , OutOfBandSelect
  , OutOfBandSelectTypes
  , mkOutOfBandSelect
  , unOutOfBandSelect
  , outOfBandSelectToBytes
  , outOfBandSelectToText
  , OutOfBandSwap
  , OutOfBandSwapTypes
  , mkOutOfBandSwap
  , unOutOfBandSwap
  , outOfBandSwapToBytes
  , outOfBandSwapToText
  , TargetSelector
  , htmx_closest
  , htmx_find
  , htmx_next
  , htmx_previous
  , targetSelectorToBytes
  , targetSelectorToText
  , Trigger
  , TriggerTypes
  , mkTrigger
  , triggerToBytes
  , triggerToText
  , TriggerEvent
  , TriggerEventTypes
  , mkTriggerEvent
  , triggerEventToBytes
  , triggerEventToText
  , Intersect
  , IntersectTypes
  , intersectRoot
  , intersectThreshold
  , intersectToBytes
  , intersectToText
  , Root (Root)
  , rootToBytes
  , rootToText
  , TriggerModifier
  , triggerModifierToBytes
  , triggerModifierToText
  , triggerOnce
  , triggerChanged
  , triggerDelay
  , triggerThrottle
  , triggerFrom
  , triggerTarget
  , triggerConsume
  , triggerQueue
  , TriggerFrom
  , TriggerFromTypes
  , mkTriggerFrom
  , triggerFromToBytes
  , triggerFromToText
  , TriggerTarget (TriggerTarget)
  , triggerTargetToBytes
  , triggerTargetToText
  , CustomTrigger
  , customTrigger
  , customTriggerToBytes
  , customTriggerToText
  , RawTrigger (RawTrigger)
  , rawTriggerToBytes
  , rawTriggerToText
  ) where

import Prelude hiding (Show, div, head, map, max, min, show, span)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty qualified as NEL
import Data.Maybe (catMaybes)
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 (ISO8601, iso8601Show)
import GHC.TypeLits (KnownNat)
import Integer (Positive)
import Numeric.Natural (Natural)
import Ogma (BCP_47, bcp_47ToText)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)
import Text.Show qualified as Show

import Brigid.HTML.Types.Action (ActionTypes, actionToText, mkAction)
import Brigid.HTML.Types.As (As, asToText)
import Brigid.HTML.Types.AutocompleteToken (AutocompleteTokenTypes, autocompleteTokenToText, mkAutocompleteToken)
import Brigid.HTML.Types.Autocapitalize (AutocapitalizeOption, autocapitalizeOptionToText)
import Brigid.HTML.Types.BlockOption (BlockOption, blockOptionToText)
import Brigid.HTML.Types.CaptureMethod (CaptureMethod, captureMethodToText)
import Brigid.HTML.Types.Changed (Changed (Changed), changedToBytes, changedToText)
import Brigid.HTML.Types.Class qualified as Class
import Brigid.HTML.Types.ClassSelector qualified as CS
import Brigid.HTML.Types.Consume (Consume (Consume), consumeToBytes, consumeToText)
import Brigid.HTML.Types.ContentEditable (ContentEditableOption, contentEditableOptionToText)
import Brigid.HTML.Types.ControlsList (ControlsList, controlsListToText)
import Brigid.HTML.Types.CrossOrigin (CrossOriginFetch, crossOriginFetchToText)
import Brigid.HTML.Types.Decoding (Decoding, decodingToText)
import Brigid.HTML.Types.Delay (Delay, delay, delayToBytes, delayToText)
import Brigid.HTML.Types.Directionality (Directionality, directionalityToText)
import Brigid.HTML.Types.Disinherit (DisinheritTypes, disinheritToText, mkDisinherit)
import Brigid.HTML.Types.Document (Document, documentToBytes, documentToText)
import Brigid.HTML.Types.Event qualified as Event
import Brigid.HTML.Types.Every (Every, everyToBytes, everyToText)
import Brigid.HTML.Types.Extension (Extension, extensionToText)
import Brigid.HTML.Types.FeaturePolicyDirective (FeaturePolicyDirective, featurePolicyDirectiveToText)
import Brigid.HTML.Types.FetchPriority (FetchPriority, fetchPriorityToText)
import Brigid.HTML.Types.For (ForOptionTypes, forOptionToText, mkForOption)
import Brigid.HTML.Types.FocusScroll (FocusScroll, focusScrollToBytes, focusScrollToText)
import Brigid.HTML.Types.Headers (HtmxHeadersTypes, mkHtmxHeaders, htmxHeadersToText)
import Brigid.HTML.Types.Href (HrefSelectorTypes, hrefSelectorToText, mkHrefSelector)
import Brigid.HTML.Types.HttpEquivToken (HttpEquivToken, httpEquivTokenToText)
import Brigid.HTML.Types.IgnoreTitle (IgnoreTitle, ignoreTitleToBytes, ignoreTitleToText)
import Brigid.HTML.Types.InputMode (InputMode, inputModeToText)
import Brigid.HTML.Types.Integrity (IntegrityEncoding, integrityToText)
import Brigid.HTML.Types.KeyHint (KeyHintOption, keyHintOptionToText)
import Brigid.HTML.Types.LoadOption (LoadOption, loadOptionToText)
import Brigid.HTML.Types.MediaQuery (MediaQuery, mediaQueryToText)
import Brigid.HTML.Types.Number (Number, numberToText)
import Brigid.HTML.Types.None (None, noneToBytes, noneToText)
import Brigid.HTML.Types.Once (Once (Once), onceToBytes, onceToText)
import Brigid.HTML.Types.Part (ExportPart, Part, exportPartToText, partToText)
import Brigid.HTML.Types.PopoverState (PopoverState, popoverStateToText)
import Brigid.HTML.Types.PopoverTargetAction (PopoverTargetAction, popoverTargetActionToText)
import Brigid.HTML.Types.Preload (Preload, preloadToText)
import Brigid.HTML.Types.PushURL (PushURLTypes, mkPushURL, pushURLToText)
import Brigid.HTML.Types.QueueOption (QueueOption, queueOptionToBytes, queueOptionToText)
import Brigid.HTML.Types.RangeBound (RangeBound, rangeBoundToText)
import Brigid.HTML.Types.ReferrerPolicy (ReferrerPolicy, referrerPolicyToText)
import Brigid.HTML.Types.RequestParams (RequestParams, requestParamsToText)
import Brigid.HTML.Types.Relationship (RelationshipTypes, mkRelationship, relationshipToText)
import Brigid.HTML.Types.Role (Role, roleToText)
import Brigid.HTML.Types.SandboxToken (SandboxToken, sandboxTokenToText)
import Brigid.HTML.Types.Scope (Scope, scopeToText)
import Brigid.HTML.Types.Shape (Shape, shapeToText)
import Brigid.HTML.Types.Size (Size, sizeToText)
import Brigid.HTML.Types.SrcsetCandidate (SrcsetCandidate, srcsetCandidateToText)
import Brigid.HTML.Types.Step (Step, stepToText)
import Brigid.HTML.Types.Swap (SwapStyle (..), swapStyleToBytes, swapStyleToText)
import Brigid.HTML.Types.SwapTiming (SwapTiming, swapTimingToBytes, swapTimingToText)
import Brigid.HTML.Types.SwapTransition (SwapTransition, swapTransitionToBytes, swapTransitionToText)
import Brigid.HTML.Types.Target (Target, targetToText)
import Brigid.HTML.Types.TargetType (TargetType, targetTypeToBytes, targetTypeToText)
import Brigid.HTML.Types.This (This (This), thisToBytes, thisToText)
import Brigid.HTML.Types.Threshold (Threshold, thresholdToBytes, thresholdToText)
import Brigid.HTML.Types.Throttle (Throttle, throttle, throttleToBytes, throttleToText)
import Brigid.HTML.Types.TrackKind (TrackKind, trackKindToText)
import Brigid.HTML.Types.TriggerFilter (TriggerFilter, triggerFilterToBytes, triggerFilterToText)
import Brigid.HTML.Types.TypeOption (TypeOptionTypes, mkTypeOption, typeOptionToText)
import Brigid.HTML.Types.Vals (HtmxValsTypes, htmxValsToText, mkHtmxVals)
import Brigid.HTML.Types.Value (ValueTypes, mkValue, valueToText)
import Brigid.HTML.Types.Window (Window, windowToBytes, windowToText)
import Brigid.HTML.Types.Wrap (Wrap, wrapToText)
import Brigid.Internal.Render qualified as Render
import Brigid.Types.Id qualified as Id
import Brigid.Types.Method (FormMethod, Get, Post, Delete, Put, Patch, formMethodToText)
import Brigid.Types.Name (Name, NameOptionTypes, mkNameOption, nameOptionToText, nameToText)
import Brigid.Types.NoContent (NoContent)
import Brigid.Types.URL (AbsoluteURL, Ping, RelativeURL, URLTypes, absoluteURLToText, mkURL, pingToText, relativeURLToText, urlToText)

newtype QuerySelector =
  QuerySelector
    { unQuerySelector :: Shrubbery.Union QuerySelectorTypes
    }

type QuerySelectorTypes =
  [ Id.Id
  , Class.Class
  , ElementSelector
  , AttributeSelector
  , RawSelector
  ]

mkQuerySelector :: ( KnownNat branchIndex
                   , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                   )
                => querySelector -> QuerySelector
mkQuerySelector =
  QuerySelector . Shrubbery.unify

querySelectorToBytes :: QuerySelector -> LBS.ByteString
querySelectorToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Id.Id (("#" <>) . Id.idToBytes)
      . Shrubbery.branch @Class.Class (("." <>) . Class.classToBytes)
      . Shrubbery.branch @ElementSelector elementSelectorToBytes
      . Shrubbery.branch @AttributeSelector attributeSelectorToBytes
      . Shrubbery.branch @RawSelector rawSelectorToBytes
      $ Shrubbery.branchEnd
  ) . unQuerySelector

querySelectorToWrappedBytes :: QuerySelector -> LBS.ByteString
querySelectorToWrappedBytes selector =
  "(" <> querySelectorToBytes selector <> ")"

querySelectorToWrappedText :: QuerySelector -> T.Text
querySelectorToWrappedText selector =
  "(" <> querySelectorToText selector <> ")"

querySelectorToText :: QuerySelector -> T.Text
querySelectorToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Id.Id (T.cons '#' . Id.idToText)
      . Shrubbery.branch @Class.Class (T.cons '.' . Class.classToText)
      . Shrubbery.branch @ElementSelector elementSelectorToText
      . Shrubbery.branch @AttributeSelector attributeSelectorToText
      . Shrubbery.branch @RawSelector rawSelectorToText
      $ Shrubbery.branchEnd
  ) . unQuerySelector

newtype RawSelector =
  RawSelector
    { rawSelectorToText :: T.Text
    } deriving (Eq)

instance Show.Show RawSelector where
  show = mappend "RawSelector " . Show.show

rawSelectorToBytes :: RawSelector -> LBS.ByteString
rawSelectorToBytes =
  Render.textToLazyBytes . rawSelectorToText

data ElementSelector =
  ElementSelector
    { elementSelectorType :: ElementType
    , elementSelectorAttr :: Maybe AttributeSelector
    , elementSelectorClasses :: [CS.ClassSelector]
    , elementSelectorChild :: Maybe ElementSelector
    }

elementSelectorToBytes :: ElementSelector -> LBS.ByteString
elementSelectorToBytes element =
  LBS.concat
    [ elementTypeToBytes $ elementSelectorType element
    , maybe "" attributeSelectorToBytes $ elementSelectorAttr element
    , foldMap CS.classSelectorToBytes $ elementSelectorClasses element
    , maybe
        ""
        (LBS8.cons ' ' . elementSelectorToBytes)
        (elementSelectorChild element)
    ]

elementSelectorToText :: ElementSelector -> T.Text
elementSelectorToText element =
  T.concat
    [ elementTypeToText $ elementSelectorType element
    , maybe "" attributeSelectorToText $ elementSelectorAttr element
    , foldMap CS.classSelectorToText $ elementSelectorClasses element
    , maybe
        ""
        (T.cons ' ' . elementSelectorToText)
        (elementSelectorChild element)
    ]

tag_custom :: T.Text
           -> Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Either NoContent ElementSelector
           -> ElementSelector
tag_custom elementName mbAttr classes eiNoContentOrElement =
  ElementSelector
    (Tag_CustomElement elementName)
    mbAttr
    classes
    (either (const Nothing) Just eiNoContentOrElement)

tag_a :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tag_a = ElementSelector Tag_Anchor

tag_abbr :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_abbr = ElementSelector Tag_Abbreviation

tag_address :: Maybe AttributeSelector
            -> [CS.ClassSelector]
            -> Maybe ElementSelector
            -> ElementSelector
tag_address = ElementSelector Tag_ContactAddress

tag_area :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_area mbAttr classes = ElementSelector Tag_Area mbAttr classes Nothing

tag_article :: Maybe AttributeSelector
            -> [CS.ClassSelector]
            -> Maybe ElementSelector
            -> ElementSelector
tag_article = ElementSelector Tag_Article

tag_aside :: Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Maybe ElementSelector
          -> ElementSelector
tag_aside = ElementSelector Tag_Aside

tag_audio :: Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Maybe ElementSelector
          -> ElementSelector
tag_audio = ElementSelector Tag_Audio

tag_b :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tag_b = ElementSelector Tag_BringAttentionTo

tag_base :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_base mbAttr classes = ElementSelector Tag_Base mbAttr classes Nothing

tag_bdi :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_bdi = ElementSelector Tag_BidirectionalIsolation

tag_bdo :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_bdo = ElementSelector Tag_BidirectionalOverride

tag_blockquote :: Maybe AttributeSelector
               -> [CS.ClassSelector]
               -> Maybe ElementSelector
               -> ElementSelector
tag_blockquote = ElementSelector Tag_Blockquote

tag_body :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_body = ElementSelector Tag_Body

tag_br :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_br mbAttr classes = ElementSelector Tag_LineBreak mbAttr classes Nothing

tag_button :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_button = ElementSelector Tag_Button

tag_canvas :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_canvas = ElementSelector Tag_Canvas

tag_caption :: Maybe AttributeSelector
            -> [CS.ClassSelector]
            -> Maybe ElementSelector
            -> ElementSelector
tag_caption = ElementSelector Tag_TableCaption

tag_cite :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_cite = ElementSelector Tag_Citation

tag_code :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_code = ElementSelector Tag_Code

tag_col :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_col mbAttr classes = ElementSelector Tag_TableColumn mbAttr classes Nothing

tag_colgroup :: Maybe AttributeSelector
             -> [CS.ClassSelector]
             -> Maybe ElementSelector
             -> ElementSelector
tag_colgroup = ElementSelector Tag_TableColumnGroup

tag_data :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_data = ElementSelector Tag_Data

tag_datalist :: Maybe AttributeSelector
             -> [CS.ClassSelector]
             -> Maybe ElementSelector
             -> ElementSelector
tag_datalist = ElementSelector Tag_DataList

tag_dd :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_dd = ElementSelector Tag_DescriptionDetails

tag_del :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_del = ElementSelector Tag_DeletedText

tag_details :: Maybe AttributeSelector
            -> [CS.ClassSelector]
            -> Maybe ElementSelector
            -> ElementSelector
tag_details = ElementSelector Tag_Details

tag_dfn :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_dfn = ElementSelector Tag_Definition

tag_dialog :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_dialog = ElementSelector Tag_Dialog

tag_div :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_div = ElementSelector Tag_Division

tag_dl :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_dl = ElementSelector Tag_DescriptionList

tag_dt :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_dt = ElementSelector Tag_DescriptionTerm

tag_em :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_em = ElementSelector Tag_Emphasis

tag_embed :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_embed mbAttr classes = ElementSelector Tag_Embed mbAttr classes Nothing

tag_fieldset :: Maybe AttributeSelector
             -> [CS.ClassSelector]
             -> Maybe ElementSelector
             -> ElementSelector
tag_fieldset = ElementSelector Tag_Fieldset

tag_figcaption :: Maybe AttributeSelector
               -> [CS.ClassSelector]
               -> Maybe ElementSelector
               -> ElementSelector
tag_figcaption = ElementSelector Tag_FigureCaption

tag_figure :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_figure = ElementSelector Tag_Figure

tag_footer :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_footer = ElementSelector Tag_Footer

tag_form :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_form = ElementSelector Tag_Form

tag_h1 :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_h1 = ElementSelector Tag_H1

tag_h2 :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_h2 = ElementSelector Tag_H1

tag_h3 :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_h3 = ElementSelector Tag_H3

tag_h4 :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_h4 = ElementSelector Tag_H4

tag_h5 :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_h5 = ElementSelector Tag_H5

tag_h6 :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_h6 = ElementSelector Tag_H6

tag_head :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_head = ElementSelector Tag_Head

tag_header :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_header = ElementSelector Tag_Header

tag_hgroup :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_hgroup = ElementSelector Tag_HeadingGroup

tag_hr :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_hr mbAttr classes =
  ElementSelector Tag_HorizontalRule mbAttr classes Nothing

tag_html :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_html = ElementSelector Tag_Html

tag_i :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tag_i = ElementSelector Tag_IdiomaticText

tag_iframe :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_iframe mbAttr classes = ElementSelector Tag_IFrame mbAttr classes Nothing

tag_img :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_img mbAttr classes = ElementSelector Tag_Image mbAttr classes Nothing

tag_input :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_input mbAttr classes = ElementSelector Tag_Input mbAttr classes Nothing

tag_ins :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_ins = ElementSelector Tag_InsertedText

tag_kbd :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_kbd = ElementSelector Tag_KeyboardInput

tag_label :: Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Maybe ElementSelector
          -> ElementSelector
tag_label = ElementSelector Tag_Label

tag_legend :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_legend = ElementSelector Tag_Legend

tag_li :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_li = ElementSelector Tag_ListItem

tag_link :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_link mbAttr classes = ElementSelector Tag_Link mbAttr classes Nothing

tag_main :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_main = ElementSelector Tag_Main

tag_map :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_map = ElementSelector Tag_Map

tag_mark :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_mark = ElementSelector Tag_Mark

tag_menu :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_menu = ElementSelector Tag_Menu

tag_meta :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_meta mbAttr classes = ElementSelector Tag_Meta mbAttr classes Nothing

tag_meter :: Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Maybe ElementSelector
          -> ElementSelector
tag_meter = ElementSelector Tag_Meter

tag_nav :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_nav = ElementSelector Tag_Nav

tag_noscript :: Maybe AttributeSelector
             -> [CS.ClassSelector]
             -> Maybe ElementSelector
             -> ElementSelector
tag_noscript = ElementSelector Tag_NoScript

tag_object :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_object = ElementSelector Tag_Object

tag_ol :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_ol = ElementSelector Tag_OrderedList

tag_optgroup :: Maybe AttributeSelector
             -> [CS.ClassSelector]
             -> Maybe ElementSelector
             -> ElementSelector
tag_optgroup = ElementSelector Tag_OptionGroup

-- option does not take a child argument, since it only holds text values,
-- which cannot be targeted as part of a CSS query.
tag_option :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_option mbAttr classes = ElementSelector Tag_Option mbAttr classes Nothing

tag_output :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_output = ElementSelector Tag_Output

tag_p :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tag_p = ElementSelector Tag_Paragraph

tag_picture :: Maybe AttributeSelector
            -> [CS.ClassSelector]
            -> Maybe ElementSelector
            -> ElementSelector
tag_picture = ElementSelector Tag_Picture

tag_pre :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_pre = ElementSelector Tag_PreformattedText

tag_progress :: Maybe AttributeSelector
             -> [CS.ClassSelector]
             -> Maybe ElementSelector
             -> ElementSelector
tag_progress = ElementSelector Tag_Progress

tag_q :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tag_q = ElementSelector Tag_Quotation

-- rp does not take a child argument, since it only holds text values, which
-- cannot be targeted as part of a CSS query.
tag_rp :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_rp mbAttr classes =
  ElementSelector Tag_RubyParenthesis mbAttr classes Nothing

tag_rt :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_rt = ElementSelector Tag_RubyText

tag_ruby :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_ruby = ElementSelector Tag_Ruby

tag_s :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tag_s = ElementSelector Tag_Strikethrough

tag_sample :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_sample = ElementSelector Tag_Sample

tag_script :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_script mbAttr classes = ElementSelector Tag_Script mbAttr classes Nothing

tag_search :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_search = ElementSelector Tag_Search

tag_section :: Maybe AttributeSelector
            -> [CS.ClassSelector]
            -> Maybe ElementSelector
            -> ElementSelector
tag_section = ElementSelector Tag_Section

tag_select :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_select = ElementSelector Tag_Select

tag_slot :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_slot = ElementSelector Tag_Slot

tag_small :: Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Maybe ElementSelector
          -> ElementSelector
tag_small = ElementSelector Tag_SideComment

tag_source :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_source mbAttr classes = ElementSelector Tag_Source mbAttr classes Nothing

tag_span :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_span = ElementSelector Tag_Span

tag_strong :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
tag_strong = ElementSelector Tag_Strong

tag_style :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_style mbAttr classes = ElementSelector Tag_Style mbAttr classes Nothing

tag_sub :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_sub = ElementSelector Tag_Subscript

tag_summary :: Maybe AttributeSelector
            -> [CS.ClassSelector]
            -> Maybe ElementSelector
            -> ElementSelector
tag_summary = ElementSelector Tag_Summary

tag_sup :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_sup = ElementSelector Tag_Superscript

tag_table :: Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Maybe ElementSelector
          -> ElementSelector
tag_table = ElementSelector Tag_Table

tag_tbody :: Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Maybe ElementSelector
          -> ElementSelector
tag_tbody = ElementSelector Tag_TableBody

tag_td :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_td = ElementSelector Tag_TableDataCell

tag_template :: Maybe AttributeSelector
             -> [CS.ClassSelector]
             -> Maybe ElementSelector
             -> ElementSelector
tag_template = ElementSelector Tag_ContentTemplate

-- textarea does not take a child argument, since it only holds text values,
-- which cannot be targeted as part of a CSS query.
tag_textarea :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_textarea mbAttr classes =
  ElementSelector Tag_TextArea mbAttr classes Nothing

tag_tfoot :: Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Maybe ElementSelector
          -> ElementSelector
tag_tfoot = ElementSelector Tag_TableFoot

tag_th :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_th = ElementSelector Tag_TableHeader

tag_thead :: Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Maybe ElementSelector
          -> ElementSelector
tag_thead = ElementSelector Tag_TableHead

tag_time :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
tag_time = ElementSelector Tag_Time

-- title does not take a child argument, since it only holds text values, which
-- cannot be targeted as part of a CSS query.
tag_title :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_title mbAttr classes = ElementSelector Tag_Title mbAttr classes Nothing

tag_tr :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_tr = ElementSelector Tag_TableRow

tag_track :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_track mbAttr classes = ElementSelector Tag_Track mbAttr classes Nothing

tag_u :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tag_u = ElementSelector Tag_Underline

tag_ul :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
tag_ul = ElementSelector Tag_UnorderedList

tag_var :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
tag_var = ElementSelector Tag_Variable

tag_video :: Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Maybe ElementSelector
          -> ElementSelector
tag_video = ElementSelector Tag_Video

tag_wbr :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
tag_wbr mbAttr classes =
  ElementSelector Tag_WordBreakOpportunity mbAttr classes Nothing

data ElementType
  = Tag_CustomElement T.Text
  | Tag_Anchor
  | Tag_Abbreviation
  | Tag_ContactAddress
  | Tag_Area
  | Tag_Article
  | Tag_Aside
  | Tag_Audio
  | Tag_BringAttentionTo
  | Tag_Base
  | Tag_BidirectionalIsolation
  | Tag_BidirectionalOverride
  | Tag_Blockquote
  | Tag_Body
  | Tag_LineBreak
  | Tag_Button
  | Tag_Canvas
  | Tag_TableCaption
  | Tag_Citation
  | Tag_Code
  | Tag_TableColumn
  | Tag_TableColumnGroup
  | Tag_Data
  | Tag_DataList
  | Tag_DescriptionDetails
  | Tag_DeletedText
  | Tag_Details
  | Tag_Definition
  | Tag_Dialog
  | Tag_Division
  | Tag_DescriptionList
  | Tag_DescriptionTerm
  | Tag_Emphasis
  | Tag_Embed
  | Tag_Fieldset
  | Tag_FigureCaption
  | Tag_Figure
  | Tag_Footer
  | Tag_Form
  | Tag_H1
  | Tag_H2
  | Tag_H3
  | Tag_H4
  | Tag_H5
  | Tag_H6
  | Tag_Head
  | Tag_Header
  | Tag_HeadingGroup
  | Tag_HorizontalRule
  | Tag_Html
  | Tag_IdiomaticText
  | Tag_IFrame
  | Tag_Image
  | Tag_Input
  | Tag_InsertedText
  | Tag_KeyboardInput
  | Tag_Label
  | Tag_Legend
  | Tag_ListItem
  | Tag_Link
  | Tag_Main
  | Tag_Map
  | Tag_Mark
  | Tag_Menu
  | Tag_Meta
  | Tag_Meter
  | Tag_Nav
  | Tag_NoScript
  | Tag_Object
  | Tag_OrderedList
  | Tag_OptionGroup
  | Tag_Option
  | Tag_Output
  | Tag_Paragraph
  | Tag_Picture
  | Tag_PreformattedText
  | Tag_Progress
  | Tag_Quotation
  | Tag_RubyParenthesis
  | Tag_RubyText
  | Tag_Ruby
  | Tag_Strikethrough
  | Tag_Sample
  | Tag_Script
  | Tag_Search
  | Tag_Section
  | Tag_Select
  | Tag_Slot
  | Tag_SideComment
  | Tag_Source
  | Tag_Span
  | Tag_Strong
  | Tag_Style
  | Tag_Subscript
  | Tag_Summary
  | Tag_Superscript
  | Tag_Table
  | Tag_TableBody
  | Tag_TableDataCell
  | Tag_ContentTemplate
  | Tag_TextArea
  | Tag_TableFoot
  | Tag_TableHeader
  | Tag_TableHead
  | Tag_Time
  | Tag_Title
  | Tag_TableRow
  | Tag_Track
  | Tag_Underline
  | Tag_UnorderedList
  | Tag_Variable
  | Tag_Video
  | Tag_WordBreakOpportunity

elementTypeToBytes :: ElementType -> LBS.ByteString
elementTypeToBytes element =
  case element of
    Tag_CustomElement tagName  -> Render.textToLazyBytes tagName
    Tag_Anchor                 -> "a"
    Tag_Abbreviation           -> "abbr"
    Tag_ContactAddress         -> "address"
    Tag_Area                   -> "area"
    Tag_Article                -> "article"
    Tag_Aside                  -> "aside"
    Tag_Audio                  -> "audio"
    Tag_BringAttentionTo       -> "b"
    Tag_Base                   -> "base"
    Tag_BidirectionalIsolation -> "bdi"
    Tag_BidirectionalOverride  -> "bdo"
    Tag_Blockquote             -> "blockquote"
    Tag_Body                   -> "body"
    Tag_LineBreak              -> "br"
    Tag_Button                 -> "button"
    Tag_Canvas                 -> "canvas"
    Tag_TableCaption           -> "caption"
    Tag_Citation               -> "cite"
    Tag_Code                   -> "code"
    Tag_TableColumn            -> "col"
    Tag_TableColumnGroup       -> "colgroup"
    Tag_Data                   -> "data"
    Tag_DataList               -> "datalist"
    Tag_DescriptionDetails     -> "dd"
    Tag_DeletedText            -> "del"
    Tag_Details                -> "details"
    Tag_Definition             -> "dfn"
    Tag_Dialog                 -> "dialog"
    Tag_Division               -> "div"
    Tag_DescriptionList        -> "dl"
    Tag_DescriptionTerm        -> "dt"
    Tag_Emphasis               -> "em"
    Tag_Embed                  -> "embed"
    Tag_Fieldset               -> "fieldset"
    Tag_FigureCaption          -> "figcaption"
    Tag_Figure                 -> "figure"
    Tag_Footer                 -> "footer"
    Tag_Form                   -> "form"
    Tag_H1                     -> "h1"
    Tag_H2                     -> "h2"
    Tag_H3                     -> "h3"
    Tag_H4                     -> "h4"
    Tag_H5                     -> "h5"
    Tag_H6                     -> "h6"
    Tag_Head                   -> "head"
    Tag_Header                 -> "header"
    Tag_HeadingGroup           -> "hgroup"
    Tag_HorizontalRule         -> "hr"
    Tag_Html                   -> "html"
    Tag_IdiomaticText          -> "i"
    Tag_IFrame                 -> "iframe"
    Tag_Image                  -> "img"
    Tag_Input                  -> "input"
    Tag_InsertedText           -> "ins"
    Tag_KeyboardInput          -> "kbd"
    Tag_Label                  -> "label"
    Tag_Legend                 -> "legend"
    Tag_ListItem               -> "li"
    Tag_Link                   -> "link"
    Tag_Main                   -> "main"
    Tag_Map                    -> "map"
    Tag_Mark                   -> "mark"
    Tag_Menu                   -> "menu"
    Tag_Meta                   -> "meta"
    Tag_Meter                  -> "metere"
    Tag_Nav                    -> "nav"
    Tag_NoScript               -> "noscript"
    Tag_Object                 -> "object"
    Tag_OrderedList            -> "ol"
    Tag_OptionGroup            -> "optgroup"
    Tag_Option                 -> "option"
    Tag_Output                 -> "output"
    Tag_Paragraph              -> "p"
    Tag_Picture                -> "picture"
    Tag_PreformattedText       -> "pre"
    Tag_Progress               -> "progress"
    Tag_Quotation              -> "q"
    Tag_RubyParenthesis        -> "rp"
    Tag_RubyText               -> "rt"
    Tag_Ruby                   -> "ruby"
    Tag_Strikethrough          -> "s"
    Tag_Sample                 -> "samp"
    Tag_Script                 -> "script"
    Tag_Search                 -> "search"
    Tag_Section                -> "section"
    Tag_Select                 -> "select"
    Tag_Slot                   -> "slot"
    Tag_SideComment            -> "small"
    Tag_Source                 -> "source"
    Tag_Span                   -> "span"
    Tag_Strong                 -> "strong"
    Tag_Style                  -> "style"
    Tag_Subscript              -> "sub"
    Tag_Summary                -> "summary"
    Tag_Superscript            -> "sup"
    Tag_Table                  -> "table"
    Tag_TableBody              -> "tbody"
    Tag_TableDataCell          -> "td"
    Tag_ContentTemplate        -> "template"
    Tag_TextArea               -> "textarea"
    Tag_TableFoot              -> "tfoot"
    Tag_TableHeader            -> "th"
    Tag_TableHead              -> "thead"
    Tag_Time                   -> "time"
    Tag_Title                  -> "title"
    Tag_TableRow               -> "tr"
    Tag_Track                  -> "track"
    Tag_Underline              -> "u"
    Tag_UnorderedList          -> "ul"
    Tag_Variable               -> "var"
    Tag_Video                  -> "video"
    Tag_WordBreakOpportunity   -> "wbr"

elementTypeToText :: ElementType -> T.Text
elementTypeToText element =
  case element of
    Tag_CustomElement tagName  -> tagName
    Tag_Anchor                 -> "a"
    Tag_Abbreviation           -> "abbr"
    Tag_ContactAddress         -> "address"
    Tag_Area                   -> "area"
    Tag_Article                -> "article"
    Tag_Aside                  -> "aside"
    Tag_Audio                  -> "audio"
    Tag_BringAttentionTo       -> "b"
    Tag_Base                   -> "base"
    Tag_BidirectionalIsolation -> "bdi"
    Tag_BidirectionalOverride  -> "bdo"
    Tag_Blockquote             -> "blockquote"
    Tag_Body                   -> "body"
    Tag_LineBreak              -> "br"
    Tag_Button                 -> "button"
    Tag_Canvas                 -> "canvas"
    Tag_TableCaption           -> "caption"
    Tag_Citation               -> "cite"
    Tag_Code                   -> "code"
    Tag_TableColumn            -> "col"
    Tag_TableColumnGroup       -> "colgroup"
    Tag_Data                   -> "data"
    Tag_DataList               -> "datalist"
    Tag_DescriptionDetails     -> "dd"
    Tag_DeletedText            -> "del"
    Tag_Details                -> "details"
    Tag_Definition             -> "dfn"
    Tag_Dialog                 -> "dialog"
    Tag_Division               -> "div"
    Tag_DescriptionList        -> "dl"
    Tag_DescriptionTerm        -> "dt"
    Tag_Emphasis               -> "em"
    Tag_Embed                  -> "embed"
    Tag_Fieldset               -> "fieldset"
    Tag_FigureCaption          -> "figcaption"
    Tag_Figure                 -> "figure"
    Tag_Footer                 -> "footer"
    Tag_Form                   -> "form"
    Tag_H1                     -> "h1"
    Tag_H2                     -> "h2"
    Tag_H3                     -> "h3"
    Tag_H4                     -> "h4"
    Tag_H5                     -> "h5"
    Tag_H6                     -> "h6"
    Tag_Head                   -> "head"
    Tag_Header                 -> "header"
    Tag_HeadingGroup           -> "hgroup"
    Tag_HorizontalRule         -> "hr"
    Tag_Html                   -> "html"
    Tag_IdiomaticText          -> "i"
    Tag_IFrame                 -> "iframe"
    Tag_Image                  -> "img"
    Tag_Input                  -> "input"
    Tag_InsertedText           -> "ins"
    Tag_KeyboardInput          -> "kbd"
    Tag_Label                  -> "label"
    Tag_Legend                 -> "legend"
    Tag_ListItem               -> "li"
    Tag_Link                   -> "link"
    Tag_Main                   -> "main"
    Tag_Map                    -> "map"
    Tag_Mark                   -> "mark"
    Tag_Menu                   -> "menu"
    Tag_Meta                   -> "meta"
    Tag_Meter                  -> "metere"
    Tag_Nav                    -> "nav"
    Tag_NoScript               -> "noscript"
    Tag_Object                 -> "object"
    Tag_OrderedList            -> "ol"
    Tag_OptionGroup            -> "optgroup"
    Tag_Option                 -> "option"
    Tag_Output                 -> "output"
    Tag_Paragraph              -> "p"
    Tag_Picture                -> "picture"
    Tag_PreformattedText       -> "pre"
    Tag_Progress               -> "progress"
    Tag_Quotation              -> "q"
    Tag_RubyParenthesis        -> "rp"
    Tag_RubyText               -> "rt"
    Tag_Ruby                   -> "ruby"
    Tag_Strikethrough          -> "s"
    Tag_Sample                 -> "samp"
    Tag_Script                 -> "script"
    Tag_Search                 -> "search"
    Tag_Section                -> "section"
    Tag_Select                 -> "select"
    Tag_Slot                   -> "slot"
    Tag_SideComment            -> "small"
    Tag_Source                 -> "source"
    Tag_Span                   -> "span"
    Tag_Strong                 -> "strong"
    Tag_Style                  -> "style"
    Tag_Subscript              -> "sub"
    Tag_Summary                -> "summary"
    Tag_Superscript            -> "sup"
    Tag_Table                  -> "table"
    Tag_TableBody              -> "tbody"
    Tag_TableDataCell          -> "td"
    Tag_ContentTemplate        -> "template"
    Tag_TextArea               -> "textarea"
    Tag_TableFoot              -> "tfoot"
    Tag_TableHeader            -> "th"
    Tag_TableHead              -> "thead"
    Tag_Time                   -> "time"
    Tag_Title                  -> "title"
    Tag_TableRow               -> "tr"
    Tag_Track                  -> "track"
    Tag_Underline              -> "u"
    Tag_UnorderedList          -> "ul"
    Tag_Variable               -> "var"
    Tag_Video                  -> "video"
    Tag_WordBreakOpportunity   -> "wbr"

type AttributeSelector = (AttributeType, Maybe T.Text)

attributeSelectorToBytes :: AttributeSelector -> LBS.ByteString
attributeSelectorToBytes (attr, mbVal) =
  LBS.concat
    [ "["
    , attributeTypeToBytes attr
    , maybe "" (\v -> "='" <> Render.textToLazyBytes v <> "'") mbVal
    , "]"
    ]

attributeSelectorToText :: AttributeSelector -> T.Text
attributeSelectorToText (attr, mbVal) =
  let attrVal = maybe "" (\v -> "='" <> v <> "'") mbVal
   in "[" <> attributeTypeToText attr <> attrVal <> "]"

data AttributeType
  -- Custom Attribute
  = Attr_CustomAttribute T.Text

  -- Global Attributes
  --
  | Attr_AccessKey
  | Attr_Autocapitalize
  | Attr_Autofocus
  | Attr_Class
  | Attr_ContentEditable
  | Attr_CustomData T.Text
  | Attr_Dir
  | Attr_Draggable
  | Attr_EnterKeyHint
  | Attr_ExportParts
  | Attr_Hidden
  | Attr_Id
  | Attr_Inert
  | Attr_InputMode
  | Attr_Is
  | Attr_ItemId
  | Attr_ItemProp
  | Attr_ItemRef
  | Attr_ItemScope
  | Attr_ItemType
  | Attr_Lang
  | Attr_Nonce
  | Attr_Part
  | Attr_Popover
  | Attr_Role
  | Attr_Slot
  | Attr_Spellcheck
  | Attr_Style
  | Attr_TabIndex
  | Attr_Title
  | Attr_Translate
  | Attr_WritingSuggestions

  -- Scoped Attributes
  --
  | Attr_Abbreviation
  | Attr_Accept
  | Attr_AcceptCharset
  | Attr_Action
  | Attr_Allow
  | Attr_Alt
  | Attr_As
  | Attr_Async
  | Attr_Autocomplete
  | Attr_Autoplay
  | Attr_Background
  | Attr_BackgroundColor
  | Attr_Blocking
  | Attr_Border
  | Attr_Capture
  | Attr_Charset
  | Attr_Checked
  | Attr_Cite
  | Attr_Color
  | Attr_Cols
  | Attr_Colspan
  | Attr_Content
  | Attr_Controls
  | Attr_ControlsList
  | Attr_Coords
  | Attr_CrossOrigin
  | Attr_Data
  | Attr_Datetime
  | Attr_Decoding
  | Attr_Default
  | Attr_Defer
  | Attr_Dirname
  | Attr_Disabled
  | Attr_DisablePictureInPicture
  | Attr_DisableRemotePlayback
  | Attr_Download
  | Attr_Enctype
  | Attr_FetchPriority
  | Attr_For
  | Attr_Form
  | Attr_FormAction
  | Attr_FormEnctype
  | Attr_FormMethod
  | Attr_FormNoValidate
  | Attr_FormTarget
  | Attr_Headers
  | Attr_Height
  | Attr_High
  | Attr_Href
  | Attr_HrefLang
  | Attr_HttpEquiv
  | Attr_ImageSizes
  | Attr_ImageSrcset
  | Attr_Integrity
  | Attr_IsMap
  | Attr_Kind
  | Attr_Label
  | Attr_List
  | Attr_Loading
  | Attr_Loop
  | Attr_Low
  | Attr_Max
  | Attr_MaxLength
  | Attr_MinLength
  | Attr_Media
  | Attr_Method
  | Attr_Min
  | Attr_Multiple
  | Attr_Muted
  | Attr_Name
  | Attr_NoModule
  | Attr_NoValidate
  | Attr_Open
  | Attr_Optimum
  | Attr_Pattern
  | Attr_Ping
  | Attr_Placeholder
  | Attr_PlaysInline
  | Attr_PopoverTarget
  | Attr_PopoverTargetAction
  | Attr_Poster
  | Attr_Preload
  | Attr_ReadOnly
  | Attr_ReferrerPolicy
  | Attr_Rel
  | Attr_Required
  | Attr_Reversed
  | Attr_Rows
  | Attr_Rowspan
  | Attr_Sandbox
  | Attr_Scope
  | Attr_Selected
  | Attr_Shape
  | Attr_Size
  | Attr_Sizes
  | Attr_Span
  | Attr_Src
  | Attr_SrcDoc
  | Attr_SrcLang
  | Attr_SrcSet
  | Attr_Start
  | Attr_Step
  | Attr_Target
  | Attr_Type
  | Attr_UseMap
  | Attr_Value
  | Attr_Width
  | Attr_Wrap
  | Attr_XMLNS

  -- HTMX Attributes
  --
  | Attr_HxGet
  | Attr_HxPost
  | Attr_HxOn T.Text
  | Attr_HxPushURL
  | Attr_HxSelect
  | Attr_HxSelectOOB
  | Attr_HxSwap
  | Attr_HxSwapOOB
  | Attr_HxTarget
  | Attr_HxTrigger
  | Attr_HxVals
  | Attr_HxBoost
  | Attr_HxConfirm
  | Attr_HxDelete
  | Attr_HxDisable
  | Attr_HxDisabledElt
  | Attr_HxDisinherit
  | Attr_HxEncoding
  | Attr_HxExt
  | Attr_HxHeaders
  | Attr_HxHistory
  | Attr_HxHistoryElt
  | Attr_HxInclude
  | Attr_HxIndicator
  | Attr_HxParams
  | Attr_HxPatch
  | Attr_HxPreserve
  | Attr_HxPrompt
  | Attr_HxPut
  | Attr_HxReplaceURL
  | Attr_HxRequest
  | Attr_HxSync
  | Attr_HxValidate
  | Attr_HyperScript

attributeTypeToBytes :: AttributeType -> LBS.ByteString
attributeTypeToBytes attr =
  case attr of
    -- Custom Attribute
    Attr_CustomAttribute attrName -> Render.textToLazyBytes attrName

    -- Global Attributes
    --
    Attr_AccessKey           -> "accesskey"
    Attr_Autocapitalize      -> "autocapitalize"
    Attr_Autofocus           -> "autofocus"
    Attr_Class               -> "class"
    Attr_ContentEditable     -> "contenteditable"
    Attr_CustomData attrName -> "data-" <> Render.textToLazyBytes attrName
    Attr_Dir                 -> "dir"
    Attr_Draggable           -> "draggable"
    Attr_EnterKeyHint        -> "enterkeyhint"
    Attr_ExportParts         -> "exportparts"
    Attr_Hidden              -> "hidden"
    Attr_Id                  -> "id"
    Attr_Inert               -> "inert"
    Attr_InputMode           -> "inputmode"
    Attr_Is                  -> "is"
    Attr_ItemId              -> "itemid"
    Attr_ItemProp            -> "itemprop"
    Attr_ItemRef             -> "itemref"
    Attr_ItemScope           -> "itemscope"
    Attr_ItemType            -> "itemtype"
    Attr_Lang                -> "lang"
    Attr_Nonce               -> "nonce"
    Attr_Part                -> "part"
    Attr_Popover             -> "popover"
    Attr_Role                -> "role"
    Attr_Slot                -> "slot"
    Attr_Spellcheck          -> "spellcheck"
    Attr_Style               -> "style"
    Attr_TabIndex            -> "tabindex"
    Attr_Title               -> "title"
    Attr_Translate           -> "translate"
    Attr_WritingSuggestions  -> "writingsuggestions"

    -- Scoped Attributes
    --
    Attr_Abbreviation            -> "abbr"
    Attr_Accept                  -> "accept"
    Attr_AcceptCharset           -> "accept-charset"
    Attr_Action                  -> "action"
    Attr_Allow                   -> "allow"
    Attr_Alt                     -> "alt"
    Attr_As                      -> "as"
    Attr_Async                   -> "async"
    Attr_Autocomplete            -> "autocomplete"
    Attr_Autoplay                -> "autoplay"
    Attr_Background              -> "background"
    Attr_BackgroundColor         -> "bgcolor"
    Attr_Blocking                -> "blocking"
    Attr_Border                  -> "border"
    Attr_Capture                 -> "capture"
    Attr_Charset                 -> "charset"
    Attr_Checked                 -> "checked"
    Attr_Cite                    -> "cite"
    Attr_Color                   -> "color"
    Attr_Cols                    -> "cols"
    Attr_Colspan                 -> "colspan"
    Attr_Content                 -> "content"
    Attr_Controls                -> "controls"
    Attr_ControlsList            -> "controlslist"
    Attr_Coords                  -> "coords"
    Attr_CrossOrigin             -> "crossorigin"
    Attr_Data                    -> "data"
    Attr_Datetime                -> "datetime"
    Attr_Decoding                -> "decoding"
    Attr_Default                 -> "default"
    Attr_Defer                   -> "defer"
    Attr_Dirname                 -> "dirname"
    Attr_Disabled                -> "disabled"
    Attr_DisablePictureInPicture -> "disablepictureinpicture"
    Attr_DisableRemotePlayback   -> "disabled"
    Attr_Download                -> "download"
    Attr_Enctype                 -> "enctype"
    Attr_FetchPriority           -> "fetchpriority"
    Attr_For                     -> "for"
    Attr_Form                    -> "form"
    Attr_FormAction              -> "formaction"
    Attr_FormEnctype             -> "formenctype"
    Attr_FormMethod              -> "formmethod"
    Attr_FormNoValidate          -> "formnovalidate"
    Attr_FormTarget              -> "formtarget"
    Attr_Headers                 -> "headers"
    Attr_Height                  -> "height"
    Attr_High                    -> "high"
    Attr_Href                    -> "href"
    Attr_HrefLang                -> "hreflang"
    Attr_HttpEquiv               -> "http-equiv"
    Attr_ImageSizes              -> "imagesizes"
    Attr_ImageSrcset             -> "imagesrcset"
    Attr_Integrity               -> "integrity"
    Attr_IsMap                   -> "ismap"
    Attr_Kind                    -> "kind"
    Attr_Label                   -> "label"
    Attr_List                    -> "list"
    Attr_Loading                 -> "loading"
    Attr_Loop                    -> "loop"
    Attr_Low                     -> "low"
    Attr_Max                     -> "max"
    Attr_MaxLength               -> "maxlength"
    Attr_MinLength               -> "minlength"
    Attr_Media                   -> "media"
    Attr_Method                  -> "method"
    Attr_Min                     -> "min"
    Attr_Multiple                -> "multiple"
    Attr_Muted                   -> "muted"
    Attr_Name                    -> "name"
    Attr_NoModule                -> "nomodule"
    Attr_NoValidate              -> "novalidate"
    Attr_Open                    -> "open"
    Attr_Optimum                 -> "optimum"
    Attr_Pattern                 -> "pattern"
    Attr_Ping                    -> "ping"
    Attr_Placeholder             -> "placeholder"
    Attr_PlaysInline             -> "playsinline"
    Attr_PopoverTarget           -> "popovertarget"
    Attr_PopoverTargetAction     -> "popovertargetaction"
    Attr_Poster                  -> "poster"
    Attr_Preload                 -> "preload"
    Attr_ReadOnly                -> "readonly"
    Attr_ReferrerPolicy          -> "referrerpolicy"
    Attr_Rel                     -> "rel"
    Attr_Required                -> "required"
    Attr_Reversed                -> "reversed"
    Attr_Rows                    -> "rows"
    Attr_Rowspan                 -> "rowspan"
    Attr_Sandbox                 -> "sandbox"
    Attr_Scope                   -> "scope"
    Attr_Selected                -> "selected"
    Attr_Shape                   -> "shape"
    Attr_Size                    -> "size"
    Attr_Sizes                   -> "sizes"
    Attr_Span                    -> "span"
    Attr_Src                     -> "src"
    Attr_SrcDoc                  -> "srcdoc"
    Attr_SrcLang                 -> "srclang"
    Attr_SrcSet                  -> "srcset"
    Attr_Start                   -> "start"
    Attr_Step                    -> "step"
    Attr_Target                  -> "target"
    Attr_Type                    -> "type"
    Attr_UseMap                  -> "usemap"
    Attr_Value                   -> "value"
    Attr_Width                   -> "width"
    Attr_Wrap                    -> "wrap"
    Attr_XMLNS                   -> "xmlns"

    -- HTMX Attributes
    --
    Attr_HxGet         -> "hx-get"
    Attr_HxPost        -> "hx-post"
    Attr_HxOn event    -> "hx-on" <> LBS8.pack (T.unpack event)
    Attr_HxPushURL     -> "hx-push-url"
    Attr_HxSelect      -> "hx-select"
    Attr_HxSelectOOB   -> "hx-select-oob"
    Attr_HxSwap        -> "hx-swap"
    Attr_HxSwapOOB     -> "hx-swap-oob"
    Attr_HxTarget      -> "hx-target"
    Attr_HxTrigger     -> "hx-trigger"
    Attr_HxVals        -> "hx-vals"
    Attr_HxBoost       -> "hx-boost"
    Attr_HxConfirm     -> "hx-confirm"
    Attr_HxDelete      -> "hx-delete"
    Attr_HxDisable     -> "hx-disable"
    Attr_HxDisabledElt -> "hx-disabled-elt"
    Attr_HxDisinherit  -> "hx-disinherit"
    Attr_HxEncoding    -> "hx-encoding"
    Attr_HxExt         -> "hx-ext"
    Attr_HxHeaders     -> "hx-headers"
    Attr_HxHistory     -> "hx-history"
    Attr_HxHistoryElt  -> "hx-historyElt"
    Attr_HxInclude     -> "hx-include"
    Attr_HxIndicator   -> "hx-indicator"
    Attr_HxParams      -> "hx-params"
    Attr_HxPatch       -> "hx-patch"
    Attr_HxPreserve    -> "hx-preserve"
    Attr_HxPrompt      -> "hx-prompt"
    Attr_HxPut         -> "hx-put"
    Attr_HxReplaceURL  -> "hx-replace-url"
    Attr_HxRequest     -> "hx-request"
    Attr_HxSync        -> "hx-sync"
    Attr_HxValidate    -> "hx-validate"

    -- Other Attributes
    --
    Attr_HyperScript -> "_"

-- The default case here is to treat it as a `CustomAttribute`.
attributeTypeFromText :: T.Text -> Either String AttributeType
attributeTypeFromText attr =
  case attr of
    -- Global Attributes
    --
    "accesskey"          -> Right Attr_AccessKey
    "autocapitalize"     -> Right Attr_Autocapitalize
    "autofocus"          -> Right Attr_Autofocus
    "class"              -> Right Attr_Class
    "contenteditable"    -> Right Attr_ContentEditable
    "dir"                -> Right Attr_Dir
    "draggable"          -> Right Attr_Draggable
    "enterkeyhint"       -> Right Attr_EnterKeyHint
    "exportparts"        -> Right Attr_ExportParts
    "hidden"             -> Right Attr_Hidden
    "id"                 -> Right Attr_Id
    "inert"              -> Right Attr_Inert
    "inputmode"          -> Right Attr_InputMode
    "is"                 -> Right Attr_Is
    "itemid"             -> Right Attr_ItemId
    "itemprop"           -> Right Attr_ItemProp
    "itemref"            -> Right Attr_ItemRef
    "itemscope"          -> Right Attr_ItemScope
    "itemtype"           -> Right Attr_ItemType
    "lang"               -> Right Attr_Lang
    "nonce"              -> Right Attr_Nonce
    "part"               -> Right Attr_Part
    "popover"            -> Right Attr_Popover
    "role"               -> Right Attr_Role
    "slot"               -> Right Attr_Slot
    "spellcheck"         -> Right Attr_Spellcheck
    "style"              -> Right Attr_Style
    "tabindex"           -> Right Attr_TabIndex
    "title"              -> Right Attr_Title
    "translate"          -> Right Attr_Translate
    "writingsuggestions" -> Right Attr_WritingSuggestions

    -- Scoped Attributes
    --
    "abbr"                    -> Right Attr_Abbreviation
    "accept"                  -> Right Attr_Accept
    "accept-charset"          -> Right Attr_AcceptCharset
    "action"                  -> Right Attr_Action
    "allow"                   -> Right Attr_Allow
    "alt"                     -> Right Attr_Alt
    "as"                      -> Right Attr_As
    "async"                   -> Right Attr_Async
    "autocomplete"            -> Right Attr_Autocomplete
    "autoplay"                -> Right Attr_Autoplay
    "background"              -> Right Attr_Background
    "bgcolor"                 -> Right Attr_BackgroundColor
    "blocking"                -> Right Attr_Blocking
    "border"                  -> Right Attr_Border
    "capture"                 -> Right Attr_Capture
    "charset"                 -> Right Attr_Charset
    "checked"                 -> Right Attr_Checked
    "cite"                    -> Right Attr_Cite
    "color"                   -> Right Attr_Color
    "cols"                    -> Right Attr_Cols
    "colspan"                 -> Right Attr_Colspan
    "content"                 -> Right Attr_Content
    "controls"                -> Right Attr_Controls
    "coords"                  -> Right Attr_Coords
    "crossorigin"             -> Right Attr_CrossOrigin
    "data"                    -> Right Attr_Data
    "datetime"                -> Right Attr_Datetime
    "decoding"                -> Right Attr_Decoding
    "default"                 -> Right Attr_Default
    "defer"                   -> Right Attr_Defer
    "dirname"                 -> Right Attr_Dirname
    "disabled"                -> Right Attr_Disabled
    "disablepictureinpicture" -> Right Attr_DisablePictureInPicture
    "disableremoteplayback"   -> Right Attr_DisableRemotePlayback
    "download"                -> Right Attr_Download
    "enctype"                 -> Right Attr_Enctype
    "fetchpriority"           -> Right Attr_FetchPriority
    "for"                     -> Right Attr_For
    "form"                    -> Right Attr_Form
    "formaction"              -> Right Attr_FormAction
    "formenctype"             -> Right Attr_FormEnctype
    "formmethod"              -> Right Attr_FormMethod
    "formnovalidate"          -> Right Attr_FormNoValidate
    "formtarget"              -> Right Attr_FormTarget
    "headers"                 -> Right Attr_Headers
    "height"                  -> Right Attr_Height
    "high"                    -> Right Attr_High
    "href"                    -> Right Attr_Href
    "hreflang"                -> Right Attr_HrefLang
    "http-equiv"              -> Right Attr_HttpEquiv
    "imagesizes"              -> Right Attr_ImageSizes
    "imagesrcset"             -> Right Attr_ImageSrcset
    "integrity"               -> Right Attr_Integrity
    "ismap"                   -> Right Attr_IsMap
    "kind"                    -> Right Attr_Kind
    "label"                   -> Right Attr_Label
    "list"                    -> Right Attr_List
    "loading"                 -> Right Attr_Loading
    "loop"                    -> Right Attr_Loop
    "low"                     -> Right Attr_Low
    "max"                     -> Right Attr_Max
    "maxlength"               -> Right Attr_MaxLength
    "minlength"               -> Right Attr_MinLength
    "media"                   -> Right Attr_Media
    "method"                  -> Right Attr_Method
    "min"                     -> Right Attr_Min
    "multiple"                -> Right Attr_Multiple
    "muted"                   -> Right Attr_Muted
    "name"                    -> Right Attr_Name
    "nomodule"                -> Right Attr_NoModule
    "novalidate"              -> Right Attr_NoValidate
    "open"                    -> Right Attr_Open
    "optimum"                 -> Right Attr_Optimum
    "pattern"                 -> Right Attr_Pattern
    "ping"                    -> Right Attr_Ping
    "placeholder"             -> Right Attr_Placeholder
    "playsinline"             -> Right Attr_PlaysInline
    "popovertarget"           -> Right Attr_PopoverTarget
    "popovertargetaction"     -> Right Attr_PopoverTargetAction
    "poster"                  -> Right Attr_Poster
    "preload"                 -> Right Attr_Preload
    "readonly"                -> Right Attr_ReadOnly
    "referrerpolicy"          -> Right Attr_ReferrerPolicy
    "rel"                     -> Right Attr_Rel
    "required"                -> Right Attr_Required
    "reversed"                -> Right Attr_Reversed
    "rows"                    -> Right Attr_Rows
    "rowspan"                 -> Right Attr_Rowspan
    "sandbox"                 -> Right Attr_Sandbox
    "scope"                   -> Right Attr_Scope
    "selected"                -> Right Attr_Selected
    "shape"                   -> Right Attr_Shape
    "size"                    -> Right Attr_Size
    "sizes"                   -> Right Attr_Sizes
    "span"                    -> Right Attr_Span
    "src"                     -> Right Attr_Src
    "srcdoc"                  -> Right Attr_SrcDoc
    "srclang"                 -> Right Attr_SrcLang
    "srcset"                  -> Right Attr_SrcSet
    "start"                   -> Right Attr_Start
    "step"                    -> Right Attr_Step
    "target"                  -> Right Attr_Target
    "type"                    -> Right Attr_Type
    "usemap"                  -> Right Attr_UseMap
    "value"                   -> Right Attr_Value
    "width"                   -> Right Attr_Width
    "wrap"                    -> Right Attr_Wrap
    "xmlns"                   -> Right Attr_XMLNS

    -- HTMX Attributes
    --
    "hx-get"          -> Right Attr_HxGet
    "hx-post"         -> Right Attr_HxPost
    "hx-push-url"     -> Right Attr_HxPushURL
    "hx-select"       -> Right Attr_HxSelect
    "hx-select-oob"   -> Right Attr_HxSelectOOB
    "hx-swap"         -> Right Attr_HxSwap
    "hx-swap-oob"     -> Right Attr_HxSwapOOB
    "hx-target"       -> Right Attr_HxTarget
    "hx-trigger"      -> Right Attr_HxTrigger
    "hx-vals"         -> Right Attr_HxVals
    "hx-boost"        -> Right Attr_HxBoost
    "hx-confirm"      -> Right Attr_HxConfirm
    "hx-delete"       -> Right Attr_HxDelete
    "hx-disable"      -> Right Attr_HxDisable
    "hx-disabled-elt" -> Right Attr_HxDisabledElt
    "hx-disinherit"   -> Right Attr_HxDisinherit
    "hx-encoding"     -> Right Attr_HxEncoding
    "hx-ext"          -> Right Attr_HxExt
    "hx-headers"      -> Right Attr_HxHeaders
    "hx-history"      -> Right Attr_HxHistory
    "hx-historyElt"   -> Right Attr_HxHistoryElt
    "hx-include"      -> Right Attr_HxInclude
    "hx-indicator"    -> Right Attr_HxIndicator
    "hx-params"       -> Right Attr_HxParams
    "hx-patch"        -> Right Attr_HxPatch
    "hx-preserve"     -> Right Attr_HxPreserve
    "hx-prompt"       -> Right Attr_HxPrompt
    "hx-put"          -> Right Attr_HxPut
    "hx-replace-url"  -> Right Attr_HxReplaceURL
    "hx-request"      -> Right Attr_HxRequest
    "hx-sync"         -> Right Attr_HxSync
    "hx-validate"     -> Right Attr_HxValidate

    -- Other Attributes
    --
    "_" -> Right Attr_HyperScript

    -- Edge cases
    txt
      | T.isPrefixOf "data-" txt ->
          Right
            . maybe (Attr_CustomAttribute txt) Attr_CustomData
            . tryParseFreeAttribute
            $ T.drop 5 txt

      -- This is stupid, but there's no way around it - some events share a
      -- namespace in the HTML events list and there's no way to differentiate
      -- between the two during parsing. So, our only option here is to always
      -- fail to parse event-based attributes, but since this is just for
      -- encoding the HTMX Config, this is an acceptable loss.
      | T.isPrefixOf "hx-on" txt ->
          Left $ "hx-on attributes cannot be parsed."
      | otherwise ->
          Right $ Attr_CustomAttribute txt

tryParseFreeAttribute :: T.Text -> Maybe T.Text
tryParseFreeAttribute txt =
  if T.all (\c -> c == '-' || c `elem` [ 'a'..'z' ]) txt
     then Just txt
     else Nothing

attributeTypeToText :: AttributeType -> T.Text
attributeTypeToText attr =
  case attr of
    -- Custom Attribute
    Attr_CustomAttribute attrName -> attrName

    -- Global Attributes
    --
    Attr_AccessKey           -> "accesskey"
    Attr_Autocapitalize      -> "autocapitalize"
    Attr_Autofocus           -> "autofocus"
    Attr_Class               -> "class"
    Attr_ContentEditable     -> "contenteditable"
    Attr_CustomData attrName -> "data-" <> attrName
    Attr_Dir                 -> "dir"
    Attr_Draggable           -> "draggable"
    Attr_EnterKeyHint        -> "enterkeyhint"
    Attr_ExportParts         -> "exportparts"
    Attr_Hidden              -> "hidden"
    Attr_Id                  -> "id"
    Attr_Inert               -> "inert"
    Attr_InputMode           -> "inputmode"
    Attr_Is                  -> "is"
    Attr_ItemId              -> "itemid"
    Attr_ItemProp            -> "itemprop"
    Attr_ItemRef             -> "itemref"
    Attr_ItemScope           -> "itemscope"
    Attr_ItemType            -> "itemtype"
    Attr_Lang                -> "lang"
    Attr_Nonce               -> "nonce"
    Attr_Part                -> "part"
    Attr_Popover             -> "popover"
    Attr_Role                -> "role"
    Attr_Slot                -> "slot"
    Attr_Spellcheck          -> "spellcheck"
    Attr_Style               -> "style"
    Attr_TabIndex            -> "tabindex"
    Attr_Title               -> "title"
    Attr_Translate           -> "translate"
    Attr_WritingSuggestions  -> "writingsuggestions"

    -- Scoped Attributes
    --
    Attr_Abbreviation            -> "abbr"
    Attr_Accept                  -> "accept"
    Attr_AcceptCharset           -> "accept-charset"
    Attr_Action                  -> "action"
    Attr_Allow                   -> "allow"
    Attr_Alt                     -> "alt"
    Attr_As                      -> "as"
    Attr_Async                   -> "async"
    Attr_Autocomplete            -> "autocomplete"
    Attr_Autoplay                -> "autoplay"
    Attr_Background              -> "background"
    Attr_BackgroundColor         -> "bgcolor"
    Attr_Blocking                -> "blocking"
    Attr_Border                  -> "border"
    Attr_Capture                 -> "capture"
    Attr_Charset                 -> "charset"
    Attr_Checked                 -> "checked"
    Attr_Cite                    -> "cite"
    Attr_Color                   -> "color"
    Attr_Cols                    -> "cols"
    Attr_Colspan                 -> "colspan"
    Attr_Content                 -> "content"
    Attr_Controls                -> "controls"
    Attr_ControlsList            -> "controlslist"
    Attr_Coords                  -> "coords"
    Attr_CrossOrigin             -> "crossorigin"
    Attr_Data                    -> "data"
    Attr_Datetime                -> "datetime"
    Attr_Decoding                -> "decoding"
    Attr_Default                 -> "default"
    Attr_Defer                   -> "defer"
    Attr_Dirname                 -> "dirname"
    Attr_Disabled                -> "disabled"
    Attr_DisablePictureInPicture -> "disablepictureinpicture"
    Attr_DisableRemotePlayback   -> "disableremoteplayback"
    Attr_Download                -> "download"
    Attr_Enctype                 -> "enctype"
    Attr_FetchPriority           -> "fetchpriority"
    Attr_For                     -> "for"
    Attr_Form                    -> "form"
    Attr_FormAction              -> "formaction"
    Attr_FormEnctype             -> "formenctype"
    Attr_FormMethod              -> "formmethod"
    Attr_FormNoValidate          -> "formnovalidate"
    Attr_FormTarget              -> "formtarget"
    Attr_Headers                 -> "headers"
    Attr_Height                  -> "height"
    Attr_High                    -> "high"
    Attr_Href                    -> "href"
    Attr_HrefLang                -> "hreflang"
    Attr_HttpEquiv               -> "http-equiv"
    Attr_ImageSizes              -> "imagesizes"
    Attr_ImageSrcset             -> "imagesrcset"
    Attr_Integrity               -> "integrity"
    Attr_IsMap                   -> "ismap"
    Attr_Kind                    -> "kind"
    Attr_Label                   -> "label"
    Attr_List                    -> "list"
    Attr_Loading                 -> "loading"
    Attr_Loop                    -> "loop"
    Attr_Low                     -> "low"
    Attr_Max                     -> "max"
    Attr_MaxLength               -> "maxlength"
    Attr_MinLength               -> "minlength"
    Attr_Media                   -> "media"
    Attr_Method                  -> "method"
    Attr_Min                     -> "min"
    Attr_Multiple                -> "multiple"
    Attr_Muted                   -> "muted"
    Attr_Name                    -> "name"
    Attr_NoModule                -> "nomodule"
    Attr_NoValidate              -> "novalidate"
    Attr_Open                    -> "open"
    Attr_Optimum                 -> "optimum"
    Attr_Pattern                 -> "pattern"
    Attr_Ping                    -> "ping"
    Attr_Placeholder             -> "placeholder"
    Attr_PlaysInline             -> "playsinline"
    Attr_PopoverTarget           -> "popovertarget"
    Attr_PopoverTargetAction     -> "popovertargetaction"
    Attr_Poster                  -> "poster"
    Attr_Preload                 -> "preload"
    Attr_ReadOnly                -> "readonly"
    Attr_ReferrerPolicy          -> "referrerpolicy"
    Attr_Rel                     -> "rel"
    Attr_Required                -> "required"
    Attr_Reversed                -> "reversed"
    Attr_Rows                    -> "rows"
    Attr_Rowspan                 -> "rowspan"
    Attr_Sandbox                 -> "sandbox"
    Attr_Scope                   -> "scope"
    Attr_Selected                -> "selected"
    Attr_Shape                   -> "shape"
    Attr_Size                    -> "size"
    Attr_Sizes                   -> "sizes"
    Attr_Span                    -> "span"
    Attr_Src                     -> "src"
    Attr_SrcDoc                  -> "srcdoc"
    Attr_SrcLang                 -> "srclang"
    Attr_SrcSet                  -> "srcset"
    Attr_Start                   -> "start"
    Attr_Step                    -> "step"
    Attr_Target                  -> "target"
    Attr_Type                    -> "type"
    Attr_UseMap                  -> "usemap"
    Attr_Value                   -> "value"
    Attr_Width                   -> "width"
    Attr_Wrap                    -> "wrap"
    Attr_XMLNS                   -> "xmlns"

    -- HTMX Attributes
    --
    Attr_HxGet         -> "hx-get"
    Attr_HxPost        -> "hx-post"
    Attr_HxOn event    -> "hx-on" <> event
    Attr_HxPushURL     -> "hx-push-url"
    Attr_HxSelect      -> "hx-select"
    Attr_HxSelectOOB   -> "hx-select-oob"
    Attr_HxSwap        -> "hx-swap"
    Attr_HxSwapOOB     -> "hx-swap-oob"
    Attr_HxTarget      -> "hx-target"
    Attr_HxTrigger     -> "hx-trigger"
    Attr_HxVals        -> "hx-vals"
    Attr_HxBoost       -> "hx-boost"
    Attr_HxConfirm     -> "hx-confirm"
    Attr_HxDelete      -> "hx-delete"
    Attr_HxDisable     -> "hx-disable"
    Attr_HxDisabledElt -> "hx-disabled-elt"
    Attr_HxDisinherit  -> "hx-disinherit"
    Attr_HxEncoding    -> "hx-encoding"
    Attr_HxExt         -> "hx-ext"
    Attr_HxHeaders     -> "hx-headers"
    Attr_HxHistory     -> "hx-history"
    Attr_HxHistoryElt  -> "hx-historyElt"
    Attr_HxInclude     -> "hx-include"
    Attr_HxIndicator   -> "hx-indicator"
    Attr_HxParams      -> "hx-params"
    Attr_HxPatch       -> "hx-patch"
    Attr_HxPreserve    -> "hx-preserve"
    Attr_HxPrompt      -> "hx-prompt"
    Attr_HxPut         -> "hx-put"
    Attr_HxReplaceURL  -> "hx-replace-url"
    Attr_HxRequest     -> "hx-request"
    Attr_HxSync        -> "hx-sync"
    Attr_HxValidate    -> "hx-validate"

    -- Other Attributes
    --
    Attr_HyperScript -> "_"

attr_custom :: T.Text -> T.Text -> AttributeSelector
attr_custom attrName val =
  (Attr_CustomAttribute attrName, Just val)

-- Global Attributes
--

attr_accesskey :: Char -> AttributeSelector
attr_accesskey = (,) Attr_AccessKey . Just . T.singleton

attr_autocapitalize :: AutocapitalizeOption -> AttributeSelector
attr_autocapitalize =
  (,) Attr_Autocapitalize . Just . autocapitalizeOptionToText

attr_autofocus :: T.Text -> AttributeSelector
attr_autofocus = (,) Attr_Autofocus . Just

attr_blocking :: BlockOption -> AttributeSelector
attr_blocking =
  (,) Attr_Blocking . Just . blockOptionToText

attr_class :: Class.Class -> AttributeSelector
attr_class = (,) Attr_Class . Just . Class.classToText

attr_contenteditable :: ContentEditableOption -> AttributeSelector
attr_contenteditable =
  (,) Attr_ContentEditable . Just . contentEditableOptionToText

attr_customData :: T.Text -> T.Text -> AttributeSelector
attr_customData dataName val = (Attr_CustomData dataName, Just val)

attr_dir :: Directionality -> AttributeSelector
attr_dir = (,) Attr_Dir . Just . directionalityToText

attr_draggable :: Bool -> AttributeSelector
attr_draggable = (,) Attr_Draggable . Just . Render.enumBoolToText

attr_enterkeyhint :: KeyHintOption -> AttributeSelector
attr_enterkeyhint = (,) Attr_EnterKeyHint . Just . keyHintOptionToText

attr_exportparts :: [ExportPart] -> AttributeSelector
attr_exportparts =
  (,) Attr_ExportParts
    . Just
    . Render.foldToTextWithSeparator exportPartToText ", "

attr_hidden :: AttributeSelector
attr_hidden = (Attr_Hidden, Nothing)

attr_id :: Id.Id -> AttributeSelector
attr_id = (,) Attr_Id . Just . Id.idToText

attr_inert :: AttributeSelector
attr_inert = (Attr_Inert, Nothing)

attr_inputmode :: InputMode -> AttributeSelector
attr_inputmode = (,) Attr_InputMode . Just . inputModeToText

attr_is :: T.Text -> AttributeSelector
attr_is = (,) Attr_Is . Just

attr_itemid :: T.Text -> AttributeSelector
attr_itemid = (,) Attr_ItemId . Just

attr_itemprop :: T.Text -> AttributeSelector
attr_itemprop = (,) Attr_ItemProp . Just

attr_itemref :: NEL.NonEmpty Id.Id -> AttributeSelector
attr_itemref =
  (,) Attr_ItemRef
    . Just
    . Render.foldToTextWithSeparator Id.idToText " "
    . NEL.toList

attr_itemscope :: AttributeSelector
attr_itemscope = (Attr_ItemScope, Nothing)

attr_itemtype :: AbsoluteURL -> AttributeSelector
attr_itemtype = (,) Attr_ItemType . Just . absoluteURLToText

attr_lang :: Maybe BCP_47 -> AttributeSelector
attr_lang =
  (,) Attr_Lang . Just . maybe "" bcp_47ToText

attr_nonce :: T.Text -> AttributeSelector
attr_nonce = (,) Attr_Nonce . Just

attr_part :: [Part] -> AttributeSelector
attr_part =
  (,) Attr_Part
    . Just
    . Render.foldToTextWithSeparator partToText " "

attr_popover :: PopoverState -> AttributeSelector
attr_popover = (,) Attr_Popover . Just . popoverStateToText

attr_role :: Role -> AttributeSelector
attr_role = (,) Attr_Role . Just . roleToText

attr_slot :: Name -> AttributeSelector
attr_slot = (,) Attr_Slot . Just . nameToText

attr_spellcheck :: Bool -> AttributeSelector
attr_spellcheck = (,) Attr_Spellcheck . Just . Render.enumBoolToText

attr_style :: T.Text -> AttributeSelector
attr_style = (,) Attr_Style . Just

attr_tabindex :: Integer -> AttributeSelector
attr_tabindex = (,) Attr_TabIndex . Just . Render.showText

attr_title :: T.Text -> AttributeSelector
attr_title = (,) Attr_Title . Just

attr_translate :: Bool -> AttributeSelector
attr_translate = (,) Attr_Translate . Just . Render.enumBoolToText

attr_writingsuggestions :: Bool -> AttributeSelector
attr_writingsuggestions =
  (,) Attr_WritingSuggestions . Just . Render.enumBoolToText

-- Scoped Attributes
--

attr_abbr :: T.Text -> AttributeSelector
attr_abbr = (,) Attr_Abbreviation . Just

attr_accept :: BS.ByteString -> AttributeSelector
attr_accept = (,) Attr_Accept . Just . Render.bytesToText

attr_acceptCharset :: AttributeSelector
attr_acceptCharset = (Attr_AcceptCharset, Just "UTF-8")

attr_action :: ( KnownNat branchIndex
               , branchIndex ~ FirstIndexOf action ActionTypes
               )
            => action -> AttributeSelector
attr_action =
  (,) Attr_Action . Just . actionToText . mkAction

attr_allow :: [FeaturePolicyDirective] -> AttributeSelector
attr_allow =
  (,) Attr_Allow
    . Just
    . Render.foldToTextWithSeparator id "; "
    . nubOrd
    . fmap featurePolicyDirectiveToText

attr_alt :: T.Text -> AttributeSelector
attr_alt = (,) Attr_Alt . Just

attr_as :: As -> AttributeSelector
attr_as = (,) Attr_As . Just . asToText

attr_async :: AttributeSelector
attr_async = (Attr_Async, Nothing)

attr_autocomplete :: ( KnownNat branchIndex
                     , branchIndex ~ FirstIndexOf token AutocompleteTokenTypes
                     )
                  => token -> AttributeSelector
attr_autocomplete =
  (,) Attr_Autocomplete . Just . autocompleteTokenToText . mkAutocompleteToken

attr_autoplay :: AttributeSelector
attr_autoplay = (Attr_Autoplay, Nothing)

attr_capture :: Maybe CaptureMethod -> AttributeSelector
attr_capture =
  maybe
    (Attr_Capture, Nothing)
    ((,) Attr_Capture . Just . captureMethodToText)

attr_charset :: AttributeSelector
attr_charset = (Attr_Charset, Just "utf-8")

attr_checked :: AttributeSelector
attr_checked = (Attr_Checked, Nothing)

attr_cite :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf cite URLTypes
             )
          => cite -> AttributeSelector
attr_cite = (,) Attr_Cite . Just . urlToText . mkURL

attr_cols :: Natural -> AttributeSelector
attr_cols = (,) Attr_Cols . Just . Render.showText

attr_colspan :: Positive -> AttributeSelector
attr_colspan = (,) Attr_Colspan . Just . Render.showText

attr_content :: T.Text -> AttributeSelector
attr_content = (,) Attr_Content . Just

attr_controls :: AttributeSelector
attr_controls = (Attr_Controls, Nothing)

attr_controlslist :: ControlsList -> AttributeSelector
attr_controlslist = (,) Attr_ControlsList . Just . controlsListToText

attr_coords :: NEL.NonEmpty Integer -> AttributeSelector
attr_coords =
  (,) Attr_Coords
    . Just
    . Render.foldToTextWithSeparator Render.showText ","
    . NEL.toList

attr_crossorigin :: CrossOriginFetch -> AttributeSelector
attr_crossorigin = (,) Attr_CrossOrigin . Just . crossOriginFetchToText

attr_data :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf _data URLTypes
             )
          => _data -> AttributeSelector
attr_data =
  (,) Attr_Data . Just . urlToText . mkURL

attr_datetime :: ISO8601 t => t -> AttributeSelector
attr_datetime = (,) Attr_Datetime . Just . T.pack . iso8601Show

attr_datetimeWithFormat :: Time.FormatTime t
                        => String -> t -> AttributeSelector
attr_datetimeWithFormat format =
  (,) Attr_Datetime
    . Just
    . T.pack
    . Time.formatTime Time.defaultTimeLocale format

attr_decoding :: Decoding -> AttributeSelector
attr_decoding = (,) Attr_Decoding . Just . decodingToText

attr_default :: AttributeSelector
attr_default = (Attr_Default, Nothing)

attr_defer :: AttributeSelector
attr_defer = (Attr_Defer, Nothing)

attr_dirname :: T.Text -> AttributeSelector
attr_dirname = (,) Attr_Dirname . Just

attr_disabled :: AttributeSelector
attr_disabled = (Attr_Disabled, Nothing)

attr_disablepictureinpicture :: AttributeSelector
attr_disablepictureinpicture = (Attr_DisablePictureInPicture, Nothing)

attr_disableremoteplayback :: AttributeSelector
attr_disableremoteplayback = (Attr_DisableRemotePlayback, Nothing)

attr_download :: Maybe NET.NonEmptyText -> AttributeSelector
attr_download = (,) Attr_Download . fmap NET.toText

attr_enctype :: BS.ByteString -> AttributeSelector
attr_enctype = (,) Attr_Enctype . Just . Render.bytesToText

attr_fetchpriority :: FetchPriority -> AttributeSelector
attr_fetchpriority = (,) Attr_FetchPriority . Just . fetchPriorityToText

attr_for :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf for ForOptionTypes
            )
         => for -> AttributeSelector
attr_for =
  (,) Attr_For . Just . forOptionToText . mkForOption

attr_form :: Id.Id -> AttributeSelector
attr_form = (,) Attr_Form . Just . Id.idToText

attr_formaction :: ( KnownNat branchIndex
                   , branchIndex ~ FirstIndexOf action ActionTypes
                   )
                => action -> AttributeSelector
attr_formaction =
  (,) Attr_FormAction . Just . actionToText . mkAction

attr_formenctype :: BS.ByteString -> AttributeSelector
attr_formenctype = (,) Attr_FormEnctype . Just . Render.bytesToText

attr_formmethod :: FormMethod -> AttributeSelector
attr_formmethod = (,) Attr_FormMethod . Just . formMethodToText

attr_formnovalidate :: AttributeSelector
attr_formnovalidate = (Attr_FormNoValidate, Nothing)

attr_formtarget :: Target -> AttributeSelector
attr_formtarget = (,) Attr_FormTarget . Just . targetToText

attr_headers :: [Id.Id] -> AttributeSelector
attr_headers =
  (,) Attr_Headers
    . Just
    . Render.foldToTextWithSeparator Id.idToText " "

attr_height :: Positive -> AttributeSelector
attr_height = (,) Attr_Height . Just . Render.showText

attr_high :: Number -> AttributeSelector
attr_high = (,) Attr_High . Just . numberToText

attr_href :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf href HrefSelectorTypes
             )
          => href -> AttributeSelector
attr_href = (,) Attr_Href . Just . hrefSelectorToText . mkHrefSelector

attr_hreflang :: Ogma.BCP_47 -> AttributeSelector
attr_hreflang = (,) Attr_HrefLang . Just . Ogma.bcp_47ToText

attr_httpEquiv :: HttpEquivToken -> AttributeSelector
attr_httpEquiv = (,) Attr_HttpEquiv . Just . httpEquivTokenToText

attr_imagesizes :: NEL.NonEmpty Size -> AttributeSelector
attr_imagesizes =
  (,) Attr_ImageSizes
    . Just
    . Render.foldToTextWithSeparator sizeToText ", "
    . NEL.toList

attr_imagesrcset :: NEL.NonEmpty SrcsetCandidate -> AttributeSelector
attr_imagesrcset =
  (,) Attr_ImageSrcset
    . Just
    . Render.foldToTextWithSeparator srcsetCandidateToText ", "
    . NEL.toList

attr_integrity :: IntegrityEncoding -> BS.ByteString -> AttributeSelector
attr_integrity sha = (,) Attr_Integrity . Just . integrityToText sha

attr_ismap :: AttributeSelector
attr_ismap = (Attr_IsMap, Nothing)

attr_kind :: TrackKind -> AttributeSelector
attr_kind = (,) Attr_Kind . Just . trackKindToText

attr_label :: T.Text -> AttributeSelector
attr_label = (,) Attr_Label . Just

attr_list :: Id.Id -> AttributeSelector
attr_list = (,) Attr_List . Just . Id.idToText

attr_loading :: LoadOption -> AttributeSelector
attr_loading = (,) Attr_Loading . Just . loadOptionToText

attr_loop :: AttributeSelector
attr_loop = (Attr_Loop, Nothing)

attr_low :: Number -> AttributeSelector
attr_low = (,) Attr_Low . Just . numberToText

attr_max :: RangeBound -> AttributeSelector
attr_max = (,) Attr_Max . Just . rangeBoundToText

attr_maxlength :: Natural -> AttributeSelector
attr_maxlength = (,) Attr_MaxLength . Just . Render.showText

attr_media :: NEL.NonEmpty MediaQuery -> AttributeSelector
attr_media =
  (,) Attr_Media
    . Just
    . Render.foldToTextWithSeparator mediaQueryToText ", "
    . NEL.toList

attr_method :: FormMethod -> AttributeSelector
attr_method = (,) Attr_Method . Just . formMethodToText

attr_min :: RangeBound -> AttributeSelector
attr_min = (,) Attr_Min . Just . rangeBoundToText

attr_minlength :: Natural -> AttributeSelector
attr_minlength = (,) Attr_MinLength . Just . Render.showText

attr_multiple :: AttributeSelector
attr_multiple = (Attr_Multiple, Nothing)

attr_muted :: AttributeSelector
attr_muted = (Attr_Muted, Nothing)

attr_name :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf name NameOptionTypes
             )
           => name -> AttributeSelector
attr_name =
  (,) Attr_Name . Just . nameOptionToText . mkNameOption

attr_nomodule :: Bool -> AttributeSelector
attr_nomodule = (,) Attr_NoModule . Just . Render.enumBoolToText

attr_novalidate :: AttributeSelector
attr_novalidate = (Attr_NoValidate, Nothing)

attr_open :: AttributeSelector
attr_open = (Attr_Open, Nothing)

attr_optimum :: Number -> AttributeSelector
attr_optimum = (,) Attr_Optimum . Just . numberToText

attr_pattern :: T.Text -> AttributeSelector
attr_pattern = (,) Attr_Pattern . Just

attr_ping :: NEL.NonEmpty Ping -> AttributeSelector
attr_ping =
  (,) Attr_Ping
    . Just
    . Render.foldToTextWithSeparator pingToText " "
    . NEL.toList

attr_placeholder :: T.Text -> AttributeSelector
attr_placeholder = (,) Attr_Placeholder . Just

attr_playsinline :: AttributeSelector
attr_playsinline = (Attr_PlaysInline, Nothing)

attr_popovertarget :: Id.Id -> AttributeSelector
attr_popovertarget = (,) Attr_PopoverTarget . Just . Id.idToText

attr_popovertargetaction :: PopoverTargetAction -> AttributeSelector
attr_popovertargetaction =
  (,) Attr_PopoverTarget . Just . popoverTargetActionToText

attr_poster :: ( KnownNat branchIndex
               , branchIndex ~ FirstIndexOf poster URLTypes
               )
            => poster -> AttributeSelector
attr_poster = (,) Attr_Poster . Just . urlToText . mkURL

attr_preload :: Preload -> AttributeSelector
attr_preload = (,) Attr_Preload . Just . preloadToText

attr_readonly :: AttributeSelector
attr_readonly = (Attr_ReadOnly, Nothing)

attr_referrerpolicy :: ReferrerPolicy -> AttributeSelector
attr_referrerpolicy = (,) Attr_ReferrerPolicy . Just . referrerPolicyToText

attr_rel :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf rel RelationshipTypes
            )
         => rel -> AttributeSelector
attr_rel =
  (,) Attr_Rel . Just . relationshipToText . mkRelationship

attr_required :: AttributeSelector
attr_required = (Attr_Required, Nothing)

attr_reversed :: AttributeSelector
attr_reversed = (Attr_Reversed, Nothing)

attr_rows :: Natural -> AttributeSelector
attr_rows = (,) Attr_Rows . Just . Render.showText

attr_rowspan :: Positive -> AttributeSelector
attr_rowspan = (,) Attr_Rowspan . Just . Render.showText

attr_sandbox :: [SandboxToken] -> AttributeSelector
attr_sandbox tokens =
  ( Attr_Sandbox
  , if null tokens
      then Nothing
      else Just $ Render.foldToTextWithSeparator sandboxTokenToText " " tokens
  )

attr_scope :: Scope -> AttributeSelector
attr_scope = (,) Attr_Scope . Just . scopeToText

attr_selected :: AttributeSelector
attr_selected = (Attr_Selected, Nothing)

attr_shape :: Shape -> AttributeSelector
attr_shape = (,) Attr_Shape . Just . shapeToText

attr_size :: Positive -> AttributeSelector
attr_size = (,) Attr_Size . Just . Render.showText

attr_sizes :: NEL.NonEmpty Size -> AttributeSelector
attr_sizes =
  (,) Attr_Sizes
    . Just
    . Render.foldToTextWithSeparator sizeToText ", "
    . NEL.toList

attr_span :: Positive -> AttributeSelector
attr_span = (,) Attr_Span . Just . Render.showText

attr_src :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf src URLTypes
            )
         => src -> AttributeSelector
attr_src =
  (,) Attr_Src . Just . urlToText . mkURL

-- | This is using a 'Text' representation of the HTML document contained in
-- the 'srcdoc' attribute. It is provided for completion's sake - it is highly
-- recommended that you not try to perform a query select based on this
-- attribute. If you absolutely must, use at your own risk.
--
attr_srcdoc :: T.Text -> AttributeSelector
attr_srcdoc = (,) Attr_SrcDoc . Just

attr_srclang :: BCP_47 -> AttributeSelector
attr_srclang = (,) Attr_SrcLang . Just . bcp_47ToText

attr_srcset :: NEL.NonEmpty SrcsetCandidate -> AttributeSelector
attr_srcset =
  (,) Attr_SrcSet
    . Just
    . Render.foldToTextWithSeparator srcsetCandidateToText ", "
    . NEL.toList

attr_start :: Integer -> AttributeSelector
attr_start = (,) Attr_Start . Just . Render.showText

attr_step :: Step -> AttributeSelector
attr_step = (,) Attr_Step . Just . stepToText

attr_target :: Target -> AttributeSelector
attr_target = (,) Attr_Target . Just . targetToText

attr_type :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf type_ TypeOptionTypes
             )
          => type_ -> AttributeSelector
attr_type =
  (,) Attr_Type . Just . typeOptionToText . mkTypeOption

attr_usemap :: Name -> AttributeSelector
attr_usemap = (,) Attr_UseMap . Just . nameToText

attr_value :: ( KnownNat branchIndex
              , branchIndex ~ FirstIndexOf value ValueTypes
              )
           => value -> AttributeSelector
attr_value =
  (,) Attr_Value . Just . valueToText . mkValue

attr_width :: Positive -> AttributeSelector
attr_width = (,) Attr_Width . Just . Render.showText

attr_wrap :: Wrap -> AttributeSelector
attr_wrap = (,) Attr_Wrap . Just . wrapToText

attr_xmlns :: ( KnownNat branchIndex
              , branchIndex ~ FirstIndexOf url URLTypes
              )
           => url -> AttributeSelector
attr_xmlns =
  (,) Attr_XMLNS . Just . urlToText . mkURL

-- HTMX Attributes
--

attr_hxGet :: RelativeURL Get -> AttributeSelector
attr_hxGet = (,) Attr_HxGet . Just . relativeURLToText

attr_hxPost :: RelativeURL Post -> AttributeSelector
attr_hxPost = (,) Attr_HxPost . Just . relativeURLToText

attr_hxOn :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf eventType Event.EventTypes
             )
          => eventType -> T.Text -> AttributeSelector
attr_hxOn eventType eventAction =
  ( Attr_HxOn . Event.hxOnEventText $ Event.mkEvent eventType
  , Just eventAction
  )

attr_hxPushURL :: ( KnownNat branchIndex
                  , branchIndex ~ FirstIndexOf url PushURLTypes
                  )
               => url -> AttributeSelector
attr_hxPushURL = (,) Attr_HxPushURL . Just . pushURLToText . mkPushURL

attr_hxSelect :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                 )
              => querySelector -> AttributeSelector
attr_hxSelect =
  (,) Attr_HxSelect . Just . querySelectorToText . mkQuerySelector

attr_hxSelectOOB :: NEL.NonEmpty OutOfBandSelect -> AttributeSelector
attr_hxSelectOOB =
  (,) Attr_HxSelectOOB
    . Just
    . Render.foldToTextWithSeparator outOfBandSelectToText ", "
    . NEL.toList

attr_hxSwap :: (KnownNat branchIndex, branchIndex ~ FirstIndexOf swap SwapTypes)
            => swap -> AttributeSelector
attr_hxSwap = (,) Attr_HxSwap . Just . swapToText . mkSwap

attr_hxSwapOOB :: ( KnownNat branchIndex
                  , branchIndex ~ FirstIndexOf swap OutOfBandSwapTypes
                  )
               => Maybe swap -> AttributeSelector
attr_hxSwapOOB =
  (,) Attr_HxSwapOOB
    . Just
    . maybe "true" (outOfBandSwapToText . mkOutOfBandSwap)

attr_hxTarget :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf target HxTargetTypes
                 )
              => target -> AttributeSelector
attr_hxTarget = (,) Attr_HxTarget . Just . hxTargetToText . mkHxTarget

attr_hxTrigger :: NEL.NonEmpty Trigger -> AttributeSelector
attr_hxTrigger =
  (,) Attr_HxTrigger
    . Just
    . Render.foldToTextWithSeparator triggerToText ", "
    . NEL.toList

attr_hxVals :: ( KnownNat branchIndex
               , branchIndex ~ FirstIndexOf vals HtmxValsTypes
               )
            => vals -> AttributeSelector
attr_hxVals = (,) Attr_HxVals . Just . htmxValsToText . mkHtmxVals

attr_hxBoost :: Bool -> AttributeSelector
attr_hxBoost = (,) Attr_HxBoost . Just . Render.enumBoolToText

attr_hxConfirm :: T.Text -> AttributeSelector
attr_hxConfirm = (,) Attr_HxConfirm . Just

attr_hxDelete :: RelativeURL Delete -> AttributeSelector
attr_hxDelete = (,) Attr_HxDelete . Just . relativeURLToText

attr_hxDisable :: AttributeSelector
attr_hxDisable = (Attr_HxDisable, Nothing)

attr_hxDisabledElt :: NEL.NonEmpty DisabledSelector -> AttributeSelector
attr_hxDisabledElt =
  (,) Attr_HxDisabledElt
    . Just
    . Render.foldToTextWithSeparator disabledSelectorToText ", "
    . NEL.toList

attr_hxDisinherit :: ( KnownNat branchIndex
                     , branchIndex ~ FirstIndexOf disinherit DisinheritTypes
                     )
                  => disinherit -> AttributeSelector
attr_hxDisinherit =
  (,) Attr_HxDisinherit . Just . disinheritToText . mkDisinherit

attr_hxEncoding :: AttributeSelector
attr_hxEncoding = (Attr_HxEncoding, Just "multipart/form-data")

attr_hxExt :: NEL.NonEmpty Extension -> AttributeSelector
attr_hxExt =
  (,) Attr_HxExt
    . Just
    . Render.foldToTextWithSeparator extensionToText ","
    . NEL.toList

attr_hxHeaders :: ( KnownNat branchIndex
                  , branchIndex ~ FirstIndexOf headers HtmxHeadersTypes
                  )
               => headers -> AttributeSelector
attr_hxHeaders = (,) Attr_HxHeaders . Just . htmxHeadersToText . mkHtmxHeaders

attr_hxHistory :: AttributeSelector
attr_hxHistory = (Attr_HxHistory, Just "false")

attr_hxHistoryElt :: AttributeSelector
attr_hxHistoryElt = (Attr_HxHistoryElt, Nothing)

attr_hxInclude :: IncludeSelector -> AttributeSelector
attr_hxInclude = (,) Attr_HxInclude . Just . includeSelectorToText

attr_hxIndicator :: Indicator -> AttributeSelector
attr_hxIndicator = (,) Attr_HxIndicator . Just . indicatorToText

attr_hxParams :: RequestParams -> AttributeSelector
attr_hxParams = (,) Attr_HxParams . Just . requestParamsToText

attr_hxPatch :: RelativeURL Patch -> AttributeSelector
attr_hxPatch = (,) Attr_HxPatch . Just . relativeURLToText

attr_hxPreserve :: AttributeSelector
attr_hxPreserve = (Attr_HxPreserve, Nothing)

attr_hxPrompt :: T.Text -> AttributeSelector
attr_hxPrompt = (,) Attr_HxPrompt . Just

attr_hxPut :: RelativeURL Put -> AttributeSelector
attr_hxPut = (,) Attr_HxPut . Just . relativeURLToText

attr_hxReplaceURL :: ( KnownNat branchIndex
                     , branchIndex ~ FirstIndexOf url PushURLTypes
                     )
                  => url -> AttributeSelector
attr_hxReplaceURL = (,) Attr_HxReplaceURL . Just . pushURLToText . mkPushURL

-- TODO
attr_hxRequest :: T.Text -> AttributeSelector
attr_hxRequest = (,) Attr_HxRequest . Just

-- TODO
attr_hxSync :: T.Text -> AttributeSelector
attr_hxSync = (,) Attr_HxSync . Just

attr_hxValidate :: AttributeSelector
attr_hxValidate = (Attr_HxValidate, Nothing)

-- Swap
--
newtype Swap =
  Swap
    { unSwap :: Shrubbery.Union SwapTypes
    }

type SwapTypes =
  [ SwapModifier
  , None
  , RawSwap
  ]

mkSwap :: (KnownNat branchIndex, branchIndex ~ FirstIndexOf swap SwapTypes)
       => swap -> Swap
mkSwap = Swap . Shrubbery.unify

swapToBytes :: Swap -> LBS.ByteString
swapToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @SwapModifier swapModifierToBytes
      . Shrubbery.branch @None (("show:" <>) . noneToBytes)
      . Shrubbery.branch @RawSwap rawSwapToBytes
      $ Shrubbery.branchEnd
  ) . unSwap

swapToText :: Swap -> T.Text
swapToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @SwapModifier swapModifierToText
      . Shrubbery.branch @None (("show:" <>) . noneToText)
      . Shrubbery.branch @RawSwap rawSwapToText
      $ Shrubbery.branchEnd
  ) . unSwap

data SwapModifier =
  SwapModifier
    { swapModifierStrategy :: SwapStyle
    , swapModifierModifier :: Maybe (Shrubbery.Union SwapModifierTypes)
    }

type SwapModifierTypes =
  [ SwapTransition
  , SwapTiming
  , IgnoreTitle
  , SwapDisplay
  , FocusScroll
  ]

swapModifierToBytes :: SwapModifier -> LBS.ByteString
swapModifierToBytes swapModifier =
  LBS8.unwords
    . catMaybes
    $ [ Just . swapStyleToBytes $ swapModifierStrategy swapModifier
      , ( Shrubbery.dissect
            . Shrubbery.branchBuild
            . Shrubbery.branch @SwapTransition swapTransitionToBytes
            . Shrubbery.branch @SwapTiming swapTimingToBytes
            . Shrubbery.branch @IgnoreTitle ignoreTitleToBytes
            . Shrubbery.branch @SwapDisplay swapDisplayToBytes
            . Shrubbery.branch @FocusScroll focusScrollToBytes
            $ Shrubbery.branchEnd
        ) <$> swapModifierModifier swapModifier
      ]

swapModifierToText :: SwapModifier -> T.Text
swapModifierToText swapModifier =
  T.unwords
    . catMaybes
    $ [ Just . swapStyleToText $ swapModifierStrategy swapModifier
      , ( Shrubbery.dissect
            . Shrubbery.branchBuild
            . Shrubbery.branch @SwapTransition swapTransitionToText
            . Shrubbery.branch @SwapTiming swapTimingToText
            . Shrubbery.branch @IgnoreTitle ignoreTitleToText
            . Shrubbery.branch @SwapDisplay swapDisplayToText
            . Shrubbery.branch @FocusScroll focusScrollToText
            $ Shrubbery.branchEnd
        ) <$> swapModifierModifier swapModifier
      ]


swapInnerHTML :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf swap SwapModifierTypes
                 )
              => Maybe swap -> SwapModifier
swapInnerHTML swap =
  SwapModifier
    { swapModifierStrategy = InnerHTML
    , swapModifierModifier = Shrubbery.unify <$> swap
    }

swapOuterHTML :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf swap SwapModifierTypes
                 )
              => Maybe swap -> SwapModifier
swapOuterHTML swap =
  SwapModifier
    { swapModifierStrategy = OuterHTML
    , swapModifierModifier = Shrubbery.unify <$> swap
    }

swapBeforebegin :: ( KnownNat branchIndex
                   , branchIndex ~ FirstIndexOf swap SwapModifierTypes
                   )
                => Maybe swap -> SwapModifier
swapBeforebegin swap =
  SwapModifier
    { swapModifierStrategy = BeforeBegin
    , swapModifierModifier = Shrubbery.unify <$> swap
    }

swapAfterbegin :: ( KnownNat branchIndex
                  , branchIndex ~ FirstIndexOf swap SwapModifierTypes
                  )
               => Maybe swap -> SwapModifier
swapAfterbegin swap =
  SwapModifier
    { swapModifierStrategy = AfterBegin
    , swapModifierModifier = Shrubbery.unify <$> swap
    }

swapBeforeend :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf swap SwapModifierTypes
                 )
              => Maybe swap -> SwapModifier
swapBeforeend swap =
  SwapModifier
    { swapModifierStrategy = BeforeEnd
    , swapModifierModifier = Shrubbery.unify <$> swap
    }

swapAfterend :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf swap SwapModifierTypes
                )
             => Maybe swap -> SwapModifier
swapAfterend swap =
  SwapModifier
    { swapModifierStrategy = AfterEnd
    , swapModifierModifier = Shrubbery.unify <$> swap
    }

swapDelete :: ( KnownNat branchIndex
              , branchIndex ~ FirstIndexOf swap SwapModifierTypes
              )
           => Maybe swap -> SwapModifier
swapDelete swap =
  SwapModifier
    { swapModifierStrategy = SwapDelete
    , swapModifierModifier = Shrubbery.unify <$> swap
    }

-- Swap Display
--
data SwapDisplay =
  SwapDisplay
    { swapDisplayType   :: SwapDisplayType
    , swapDisplayView   :: SwapDisplayView
    , swapDisplayTarget :: Maybe (Shrubbery.Union SwapDisplayTargetTypes)
    }

type SwapDisplayTargetTypes =
  [ QuerySelector
  , Window
  ]

swapDisplayToBytes :: SwapDisplay -> LBS.ByteString
swapDisplayToBytes display =
  LBS.intercalate (LBS8.pack ":")
    . catMaybes
    $ [ Just . swapDisplayTypeToBytes $ swapDisplayType display
      , ( Shrubbery.dissect
            . Shrubbery.branchBuild
            . Shrubbery.branch @QuerySelector querySelectorToBytes
            . Shrubbery.branch @Window windowToBytes
            $ Shrubbery.branchEnd
        ) <$> swapDisplayTarget display
      , Just . swapDisplayViewToBytes $ swapDisplayView display
      ]

swapDisplayToText :: SwapDisplay -> T.Text
swapDisplayToText display =
  T.intercalate ":"
    . catMaybes
    $ [ Just . swapDisplayTypeToText $ swapDisplayType display
      , ( Shrubbery.dissect
            . Shrubbery.branchBuild
            . Shrubbery.branch @QuerySelector querySelectorToText
            . Shrubbery.branch @Window windowToText
            $ Shrubbery.branchEnd
        ) <$> swapDisplayTarget display
      , Just . swapDisplayViewToText $ swapDisplayView display
      ]

scroll :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf display SwapDisplayTargetTypes
          )
       => SwapDisplayView -> Maybe display -> SwapDisplay
scroll displayTo mbDisplayTarget =
  SwapDisplay
    { swapDisplayType   = ScrollTo
    , swapDisplayView   = displayTo
    , swapDisplayTarget = Shrubbery.unify <$> mbDisplayTarget
    }

show :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf display SwapDisplayTargetTypes
        )
     => SwapDisplayView -> Maybe display -> SwapDisplay
show displayTo mbDisplayTarget =
  SwapDisplay
    { swapDisplayType   = Show
    , swapDisplayView   = displayTo
    , swapDisplayTarget = Shrubbery.unify <$> mbDisplayTarget
    }

data SwapDisplayType
  = ScrollTo
  | Show

swapDisplayTypeToBytes :: SwapDisplayType -> LBS.ByteString
swapDisplayTypeToBytes displayType =
  case displayType of
    ScrollTo -> "scroll"
    Show     -> "show"

swapDisplayTypeToText :: SwapDisplayType -> T.Text
swapDisplayTypeToText displayType =
  case displayType of
    ScrollTo -> "scroll"
    Show     -> "show"

data SwapDisplayView
  = SwapTop
  | SwapBottom

swapDisplayViewToBytes :: SwapDisplayView -> LBS.ByteString
swapDisplayViewToBytes view =
  case view of
    SwapTop    -> "top"
    SwapBottom -> "bottom"

swapDisplayViewToText :: SwapDisplayView -> T.Text
swapDisplayViewToText view =
  case view of
    SwapTop    -> "top"
    SwapBottom -> "bottom"

newtype RawSwap =
  RawSwap
    { rawSwapToText :: T.Text
    } deriving (Eq)

instance Show.Show RawSwap where
  show = mappend "RawSwap " . Show.show

rawSwapToBytes :: RawSwap -> LBS.ByteString
rawSwapToBytes = Render.textToLazyBytes . rawSwapToText

-- Swap Selector
--
data SwapSelector =
  SwapSelector
    { swapSelectorStrategy :: SwapStyle
    , swapSelectorQuery    :: QuerySelector
    }

-- <swap strategy>:<query selector>
swapSelectorToBytes :: SwapSelector -> LBS.ByteString
swapSelectorToBytes swap =
  LBS.concat
    [ swapStyleToBytes $ swapSelectorStrategy swap
    , LBS8.singleton ':'
    , querySelectorToBytes $ swapSelectorQuery swap
    ]

swapSelectorToText :: SwapSelector -> T.Text
swapSelectorToText swap =
  T.concat
    [ swapStyleToText $ swapSelectorStrategy swap
    , T.singleton ':'
    , querySelectorToText $ swapSelectorQuery swap
    ]

-- <query selector>:<swap style>
selectSwapToBytes :: SwapSelector -> LBS.ByteString
selectSwapToBytes swap =
  LBS.concat
    [ querySelectorToBytes $ swapSelectorQuery swap
    , LBS8.singleton ':'
    , swapStyleToBytes $ swapSelectorStrategy swap
    ]

selectSwapToText :: SwapSelector -> T.Text
selectSwapToText swap =
  T.concat
    [ querySelectorToText $ swapSelectorQuery swap
    , T.singleton ':'
    , swapStyleToText $ swapSelectorStrategy swap
    ]

swapSelectInnerHTML :: QuerySelector -> SwapSelector
swapSelectInnerHTML = SwapSelector InnerHTML

swapSelectOuterHTML :: QuerySelector -> SwapSelector
swapSelectOuterHTML = SwapSelector OuterHTML

swapSelectBeforebegin :: QuerySelector -> SwapSelector
swapSelectBeforebegin = SwapSelector BeforeBegin

swapSelectAfterbegin :: QuerySelector -> SwapSelector
swapSelectAfterbegin = SwapSelector AfterBegin

swapSelectBeforeend :: QuerySelector -> SwapSelector
swapSelectBeforeend = SwapSelector BeforeEnd

swapSelectAfterend :: QuerySelector -> SwapSelector
swapSelectAfterend = SwapSelector AfterEnd

swapSelectDelete :: QuerySelector -> SwapSelector
swapSelectDelete = SwapSelector SwapDelete

swapSelectNone :: QuerySelector -> SwapSelector
swapSelectNone = SwapSelector SwapNone

-- Out of band Select
--
newtype OutOfBandSelect =
  OutOfBandSelect
    { unOutOfBandSelect :: Shrubbery.Union OutOfBandSelectTypes
    }

type OutOfBandSelectTypes =
  [ QuerySelector
  , SwapSelector
  , RawSelector
  ]

mkOutOfBandSelect :: ( KnownNat branchIndex
                     , branchIndex ~ FirstIndexOf select OutOfBandSelectTypes
                     )
                => select -> OutOfBandSelect
mkOutOfBandSelect =
  OutOfBandSelect . Shrubbery.unify

outOfBandSelectToBytes :: OutOfBandSelect -> LBS.ByteString
outOfBandSelectToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @QuerySelector querySelectorToBytes
      . Shrubbery.branch @SwapSelector swapSelectorToBytes
      . Shrubbery.branch @RawSelector rawSelectorToBytes
      $ Shrubbery.branchEnd
  ) . unOutOfBandSelect

outOfBandSelectToText :: OutOfBandSelect -> T.Text
outOfBandSelectToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @QuerySelector querySelectorToText
      . Shrubbery.branch @SwapSelector swapSelectorToText
      . Shrubbery.branch @RawSelector rawSelectorToText
      $ Shrubbery.branchEnd
  ) . unOutOfBandSelect

-- Out of band Swap
--
newtype OutOfBandSwap =
  OutOfBandSwap
    { unOutOfBandSwap :: Shrubbery.Union OutOfBandSwapTypes
    }

type OutOfBandSwapTypes =
  [ SwapStyle
  , SwapSelector
  , RawSelector
  ]

mkOutOfBandSwap :: ( KnownNat branchIndex
                   , branchIndex ~ FirstIndexOf swap OutOfBandSwapTypes
                   )
                => swap -> OutOfBandSwap
mkOutOfBandSwap =
  OutOfBandSwap . Shrubbery.unify

outOfBandSwapToBytes :: OutOfBandSwap -> LBS.ByteString
outOfBandSwapToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @SwapStyle swapStyleToBytes
      . Shrubbery.branch @SwapSelector swapSelectorToBytes
      . Shrubbery.branch @RawSelector rawSelectorToBytes
      $ Shrubbery.branchEnd
  ) . unOutOfBandSwap

outOfBandSwapToText :: OutOfBandSwap -> T.Text
outOfBandSwapToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @SwapStyle swapStyleToText
      . Shrubbery.branch @SwapSelector swapSelectorToText
      . Shrubbery.branch @RawSelector rawSelectorToText
      $ Shrubbery.branchEnd
  ) . unOutOfBandSwap

-- Disabled Selector
--
newtype DisabledSelector =
  DisabledSelector
    { unDisabledSelector :: Shrubbery.Union DisabledSelectorTypes
    }

type DisabledSelectorTypes =
  [ This
  , QuerySelector
  , TargetSelector
  ]

disableThis :: DisabledSelector
disableThis = DisabledSelector $ Shrubbery.unify This

disableClosest :: ( KnownNat branchIndex
                  , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                  )
               => querySelector -> DisabledSelector
disableClosest = DisabledSelector . Shrubbery.unify . htmx_closest

disabledSelector :: ( KnownNat branchIndex
                    , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                    )
                 => querySelector -> DisabledSelector
disabledSelector = DisabledSelector . Shrubbery.unify . mkQuerySelector

disabledSelectorToBytes :: DisabledSelector -> LBS.ByteString
disabledSelectorToBytes =
  ( Shrubbery.dissect
     . Shrubbery.branchBuild
     . Shrubbery.branch @This thisToBytes
     . Shrubbery.branch @QuerySelector querySelectorToBytes
     . Shrubbery.branch @TargetSelector targetSelectorToBytes
     $ Shrubbery.branchEnd
  ) . unDisabledSelector

disabledSelectorToText :: DisabledSelector -> T.Text
disabledSelectorToText =
  ( Shrubbery.dissect
     . Shrubbery.branchBuild
     . Shrubbery.branch @This thisToText
     . Shrubbery.branch @QuerySelector querySelectorToText
     . Shrubbery.branch @TargetSelector targetSelectorToText
     $ Shrubbery.branchEnd
  ) . unDisabledSelector

-- Include Selector
--
newtype IncludeSelector =
  IncludeSelector
    { unIncludeSelector :: Shrubbery.Union IncludeSelectorTypes
    }

type IncludeSelectorTypes =
  [ This
  , QuerySelector
  , TargetSelector
  ]

includeThis :: IncludeSelector
includeThis = IncludeSelector $ Shrubbery.unify This

includeSelector :: ( KnownNat branchIndex
                   , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                   )
                => querySelector -> IncludeSelector
includeSelector = IncludeSelector . Shrubbery.unify . mkQuerySelector

includeTarget :: TargetSelector -> IncludeSelector
includeTarget = IncludeSelector . Shrubbery.unify

includeSelectorToBytes :: IncludeSelector -> LBS.ByteString
includeSelectorToBytes =
  ( Shrubbery.dissect
     . Shrubbery.branchBuild
     . Shrubbery.branch @This thisToBytes
     . Shrubbery.branch @QuerySelector querySelectorToBytes
     . Shrubbery.branch @TargetSelector targetSelectorToBytes
     $ Shrubbery.branchEnd
  ) . unIncludeSelector

includeSelectorToText :: IncludeSelector -> T.Text
includeSelectorToText =
  ( Shrubbery.dissect
     . Shrubbery.branchBuild
     . Shrubbery.branch @This thisToText
     . Shrubbery.branch @QuerySelector querySelectorToText
     . Shrubbery.branch @TargetSelector targetSelectorToText
     $ Shrubbery.branchEnd
  ) . unIncludeSelector

-- Indicator
--
newtype Indicator =
  Indicator
    { unIndicator :: Shrubbery.Union IndicatorTypes
    }

type IndicatorTypes =
  [ QuerySelector
  , TargetSelector
  ]

indicateClosest :: ( KnownNat branchIndex
                   , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                   )
                => querySelector -> Indicator
indicateClosest = Indicator . Shrubbery.unify . htmx_closest

indicateSelector :: ( KnownNat branchIndex
                   , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                   )
                => querySelector -> Indicator
indicateSelector = Indicator . Shrubbery.unify . mkQuerySelector

indicatorToBytes :: Indicator -> LBS.ByteString
indicatorToBytes =
  ( Shrubbery.dissect
     . Shrubbery.branchBuild
     . Shrubbery.branch @QuerySelector querySelectorToBytes
     . Shrubbery.branch @TargetSelector targetSelectorToBytes
     $ Shrubbery.branchEnd
  ) . unIndicator

indicatorToText :: Indicator -> T.Text
indicatorToText =
  ( Shrubbery.dissect
     . Shrubbery.branchBuild
     . Shrubbery.branch @QuerySelector querySelectorToText
     . Shrubbery.branch @TargetSelector targetSelectorToText
     $ Shrubbery.branchEnd
  ) . unIndicator

-- HxTarget and TargetSelector
--
newtype HxTarget =
  HxTarget
    { unHxTarget :: Shrubbery.Union HxTargetTypes
    }

type HxTargetTypes =
  [ QuerySelector
  , TargetSelector
  , TargetType
  , This
  , RawSelector
  ]

mkHxTarget :: ( KnownNat branchIndex
              , branchIndex ~ FirstIndexOf target HxTargetTypes
              )
           => target -> HxTarget
mkHxTarget =
  HxTarget . Shrubbery.unify

hxTargetToBytes :: HxTarget -> LBS.ByteString
hxTargetToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @QuerySelector querySelectorToBytes
      . Shrubbery.branch @TargetSelector targetSelectorToBytes
      . Shrubbery.branch @TargetType targetTypeToBytes
      . Shrubbery.branch @This thisToBytes
      . Shrubbery.branch @RawSelector rawSelectorToBytes
      $ Shrubbery.branchEnd
  ) . unHxTarget

hxTargetToText :: HxTarget -> T.Text
hxTargetToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @QuerySelector querySelectorToText
      . Shrubbery.branch @TargetSelector targetSelectorToText
      . Shrubbery.branch @TargetType targetTypeToText
      . Shrubbery.branch @This thisToText
      . Shrubbery.branch @RawSelector rawSelectorToText
      $ Shrubbery.branchEnd
  ) . unHxTarget

data TargetSelectorType
  = TargetSelector_Closest
  | TargetSelector_Find
  | TargetSelector_Next
  | TargetSelector_Previous

targetSelectorTypeToBytes :: TargetSelectorType -> LBS.ByteString
targetSelectorTypeToBytes selectorType =
  case selectorType of
    TargetSelector_Closest  -> "closest"
    TargetSelector_Find     -> "find"
    TargetSelector_Next     -> "next"
    TargetSelector_Previous -> "previous"

targetSelectorTypeToText :: TargetSelectorType -> T.Text
targetSelectorTypeToText selectorType =
  case selectorType of
    TargetSelector_Closest  -> "closest"
    TargetSelector_Find     -> "find"
    TargetSelector_Next     -> "next"
    TargetSelector_Previous -> "previous"

data TargetSelector =
  TargetSelector
    { targetSelectorType  :: TargetSelectorType
    , targetSelectorQuery :: QuerySelector
    }

htmx_closest :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                )
             => querySelector -> TargetSelector
htmx_closest =
  TargetSelector TargetSelector_Closest . mkQuerySelector

htmx_find :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
             )
          => querySelector -> TargetSelector
htmx_find =
  TargetSelector TargetSelector_Find . mkQuerySelector

htmx_next :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
             )
          => querySelector -> TargetSelector
htmx_next =
  TargetSelector TargetSelector_Next . mkQuerySelector

htmx_previous :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                 )
              => querySelector -> TargetSelector
htmx_previous =
  TargetSelector TargetSelector_Previous . mkQuerySelector

targetSelectorToBytes :: TargetSelector -> LBS.ByteString
targetSelectorToBytes selector =
  LBS.concat
    [ targetSelectorTypeToBytes $ targetSelectorType selector
    , " "
    , querySelectorToBytes $ targetSelectorQuery selector
    ]

targetSelectorToWrappedBytes :: TargetSelector -> LBS.ByteString
targetSelectorToWrappedBytes selector =
  LBS.concat
    [ targetSelectorTypeToBytes $ targetSelectorType selector
    , " ("
    , querySelectorToBytes $ targetSelectorQuery selector
    , ")"
    ]

targetSelectorToText :: TargetSelector -> T.Text
targetSelectorToText selector =
  T.unwords
    [ targetSelectorTypeToText $ targetSelectorType selector
    , querySelectorToText $ targetSelectorQuery selector
    ]

targetSelectorToWrappedText :: TargetSelector -> T.Text
targetSelectorToWrappedText selector =
  T.concat
    [ targetSelectorTypeToText $ targetSelectorType selector
    , " ("
    , querySelectorToText $ targetSelectorQuery selector
    , ")"
    ]

-- Trigger
--
newtype Trigger =
  Trigger
    { unTrigger :: Shrubbery.Union TriggerTypes
    }

type TriggerTypes =
  [ Every
  , TriggerEvent
  , CustomTrigger
  , RawTrigger
  ]

mkTrigger :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf trigger TriggerTypes
             )
          => trigger -> Trigger
mkTrigger = Trigger . Shrubbery.unify

triggerToBytes :: Trigger -> LBS.ByteString
triggerToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Every everyToBytes
      . Shrubbery.branch @TriggerEvent triggerEventToBytes
      . Shrubbery.branch @CustomTrigger customTriggerToBytes
      . Shrubbery.branch @RawTrigger rawTriggerToBytes
      $ Shrubbery.branchEnd
  ) . unTrigger

triggerToText :: Trigger -> T.Text
triggerToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Every everyToText
      . Shrubbery.branch @TriggerEvent triggerEventToText
      . Shrubbery.branch @CustomTrigger customTriggerToText
      . Shrubbery.branch @RawTrigger rawTriggerToText
      $ Shrubbery.branchEnd
  ) . unTrigger

-- TriggerEvent
--
data TriggerEvent =
  TriggerEvent
    { triggerEventType      :: Shrubbery.Union TriggerEventTypes
    , triggerEventFilter    :: Maybe TriggerFilter
    , triggerEventModifiers :: [TriggerModifier]
    }

type TriggerEventTypes =
  [ Event.Event
  , Event.TriggerLoad
  , Event.TriggerRevealed
  , Intersect
  ]

mkTriggerEvent :: ( KnownNat branchIndex
                  , branchIndex ~ FirstIndexOf triggerEvent TriggerEventTypes
                  )
               => triggerEvent
               -> Maybe TriggerFilter
               -> [TriggerModifier]
               -> TriggerEvent
mkTriggerEvent eventType mbFilter modifiers =
  TriggerEvent
    { triggerEventType      = Shrubbery.unify eventType
    , triggerEventFilter    = mbFilter
    , triggerEventModifiers = modifiers
    }

triggerEventToBytes :: TriggerEvent -> LBS.ByteString
triggerEventToBytes event =
  lbsUnwords
    . catMaybes
    $ [ ( Shrubbery.dissect
            . Shrubbery.branchBuild
            . Shrubbery.branch @Event.Event Event.eventToBytes
            . Shrubbery.branch @Event.TriggerLoad Event.triggerLoadToBytes
            . Shrubbery.branch @Event.TriggerRevealed Event.triggerRevealedToBytes
            . Shrubbery.branch @Intersect intersectToBytes
            $ Shrubbery.branchEnd
        ) <$> Just (triggerEventType event)
      , triggerFilterToBytes <$> triggerEventFilter event
      , fmap (lbsUnwords . fmap triggerModifierToBytes . NEL.toList)
          . NEL.nonEmpty
          $ triggerEventModifiers event
      ]

triggerEventToText :: TriggerEvent -> T.Text
triggerEventToText event =
  T.unwords
    . catMaybes
    $ [ ( Shrubbery.dissect
            . Shrubbery.branchBuild
            . Shrubbery.branch @Event.Event Event.eventToText
            . Shrubbery.branch @Event.TriggerLoad Event.triggerLoadToText
            . Shrubbery.branch @Event.TriggerRevealed Event.triggerRevealedToText
            . Shrubbery.branch @Intersect intersectToText
            $ Shrubbery.branchEnd
        ) <$> Just (triggerEventType event)
      , triggerFilterToText <$> triggerEventFilter event
      , fmap (T.unwords . fmap triggerModifierToText . NEL.toList)
          . NEL.nonEmpty
          $ triggerEventModifiers event
      ]

newtype Intersect =
  Intersect
    { unIntersect :: Shrubbery.Union IntersectTypes
    }

type IntersectTypes =
  [ Root
  , Threshold
  ]

mkIntersect :: ( KnownNat branchIndex
               , branchIndex ~ FirstIndexOf intersect IntersectTypes
               )
            => intersect -> Intersect
mkIntersect = Intersect . Shrubbery.unify

intersectRoot :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                 )
              => querySelector -> Intersect
intersectRoot = mkIntersect . Root . mkQuerySelector

intersectThreshold :: Threshold -> Intersect
intersectThreshold = mkIntersect

intersectToBytes :: Intersect -> LBS.ByteString
intersectToBytes =
  ("intersect " <>)
    . ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @Root rootToBytes
          . Shrubbery.branch @Threshold thresholdToBytes
          $ Shrubbery.branchEnd
      )
    . unIntersect

intersectToText :: Intersect -> T.Text
intersectToText =
  ("intersect " <>)
    . ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @Root rootToText
          . Shrubbery.branch @Threshold thresholdToText
          $ Shrubbery.branchEnd
      )
    . unIntersect

newtype Root =
  Root
    { unRoot :: QuerySelector
    }

rootToBytes :: Root -> LBS.ByteString
rootToBytes =
  ("root:" <>) . querySelectorToWrappedBytes . unRoot

rootToText :: Root -> T.Text
rootToText =
  ("root:" <>) . querySelectorToWrappedText . unRoot

newtype TriggerModifier =
  TriggerModifier
    { unTriggerModifier :: Shrubbery.Union TriggerModifierTypes
    }

type TriggerModifierTypes =
  [ Once
  , Changed
  , Delay
  , Throttle
  , TriggerFrom
  , TriggerTarget
  , Consume
  , QueueOption
  ]

triggerModifierToBytes :: TriggerModifier -> LBS.ByteString
triggerModifierToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Once onceToBytes
      . Shrubbery.branch @Changed changedToBytes
      . Shrubbery.branch @Delay delayToBytes
      . Shrubbery.branch @Throttle throttleToBytes
      . Shrubbery.branch @TriggerFrom triggerFromToBytes
      . Shrubbery.branch @TriggerTarget triggerTargetToBytes
      . Shrubbery.branch @Consume consumeToBytes
      . Shrubbery.branch @QueueOption queueOptionToBytes
      $ Shrubbery.branchEnd
  ) . unTriggerModifier

triggerModifierToText :: TriggerModifier -> T.Text
triggerModifierToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Once onceToText
      . Shrubbery.branch @Changed changedToText
      . Shrubbery.branch @Delay delayToText
      . Shrubbery.branch @Throttle throttleToText
      . Shrubbery.branch @TriggerFrom triggerFromToText
      . Shrubbery.branch @TriggerTarget triggerTargetToText
      . Shrubbery.branch @Consume consumeToText
      . Shrubbery.branch @QueueOption queueOptionToText
      $ Shrubbery.branchEnd
  ) . unTriggerModifier

triggerOnce :: TriggerModifier
triggerOnce = TriggerModifier $ Shrubbery.unify Once

triggerChanged :: TriggerModifier
triggerChanged = TriggerModifier $ Shrubbery.unify Changed

triggerDelay :: Natural -> TriggerModifier
triggerDelay = TriggerModifier . Shrubbery.unify . delay

triggerThrottle :: Natural -> TriggerModifier
triggerThrottle = TriggerModifier . Shrubbery.unify . throttle

triggerFrom :: ( KnownNat branchIndex
               , branchIndex ~ FirstIndexOf triggerFrom TriggerFromTypes
               )
            => triggerFrom -> TriggerModifier
triggerFrom = TriggerModifier . Shrubbery.unify . mkTriggerFrom

triggerTarget :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                 )
              => querySelector -> TriggerModifier
triggerTarget =
  TriggerModifier . Shrubbery.unify . TriggerTarget . mkQuerySelector

triggerConsume :: TriggerModifier
triggerConsume = TriggerModifier $ Shrubbery.unify Consume

triggerQueue :: QueueOption -> TriggerModifier
triggerQueue = TriggerModifier . Shrubbery.unify

-- Trigger From
--
newtype TriggerFrom =
  TriggerFrom
    { unTriggerFrom :: Shrubbery.Union TriggerFromTypes
    }

type TriggerFromTypes =
  [ QuerySelector
  , Document
  , Window
  , TargetSelector
  ]

mkTriggerFrom :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf triggerFrom TriggerFromTypes
                 )
              => triggerFrom -> TriggerFrom
mkTriggerFrom = TriggerFrom . Shrubbery.unify

triggerFromToBytes :: TriggerFrom -> LBS.ByteString
triggerFromToBytes =
  ("from:" <>)
    . ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @QuerySelector querySelectorToWrappedBytes
          . Shrubbery.branch @Document documentToBytes
          . Shrubbery.branch @Window windowToBytes
          . Shrubbery.branch @TargetSelector targetSelectorToWrappedBytes
          $ Shrubbery.branchEnd
      )
    . unTriggerFrom

triggerFromToText :: TriggerFrom -> T.Text
triggerFromToText =
  ("from:" <>)
    . ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @QuerySelector querySelectorToWrappedText
          . Shrubbery.branch @Document documentToText
          . Shrubbery.branch @Window windowToText
          . Shrubbery.branch @TargetSelector targetSelectorToWrappedText
          $ Shrubbery.branchEnd
      )
    . unTriggerFrom

-- Trigger Target
--
newtype TriggerTarget =
  TriggerTarget
    { unTriggerTarget :: QuerySelector
    }

triggerTargetToBytes :: TriggerTarget -> LBS.ByteString
triggerTargetToBytes =
  ("target:" <>) .  querySelectorToWrappedBytes . unTriggerTarget

triggerTargetToText :: TriggerTarget -> T.Text
triggerTargetToText =
  ("target:" <>) . querySelectorToWrappedText . unTriggerTarget

-- CustomTrigger
--
newtype CustomTrigger = CustomTrigger T.Text

customTrigger :: T.Text -> Trigger
customTrigger = mkTrigger . CustomTrigger

customTriggerToBytes :: CustomTrigger -> LBS.ByteString
customTriggerToBytes (CustomTrigger trigger) =
  Render.textToLazyBytes trigger <> " from:body"

customTriggerToText :: CustomTrigger -> T.Text
customTriggerToText (CustomTrigger trigger) =
  trigger <> " from:body"

-- RawTrigger
--
newtype RawTrigger =
  RawTrigger
    { rawTriggerToText :: T.Text
    } deriving (Eq)

instance Show.Show RawTrigger where
  show = mappend "RawTrigger " . Show.show

rawTriggerToBytes :: RawTrigger -> LBS.ByteString
rawTriggerToBytes =
  Render.textToLazyBytes . rawTriggerToText

-- Helpers
--
lbsUnwords :: [LBS.ByteString] -> LBS.ByteString
lbsUnwords = LBS.intercalate (LBS8.pack " ")
