{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Types.QuerySelector
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
  , customTag
  , a
  , abbr
  , address
  , area
  , article
  , aside
  , audio
  , b
  , base
  , bdi
  , bdo
  , blockquote
  , body
  , br
  , button
  , canvas
  , caption
  , citeTag
  , code
  , col
  , colgroup
  , dataTag
  , datalist
  , dd
  , del
  , details
  , dfn
  , dialog
  , div
  , dl
  , dt
  , em
  , embed
  , fieldset
  , figcaption
  , figure
  , footer
  , formTag
  , h1
  , h2
  , h3
  , h4
  , h5
  , h6
  , head
  , header
  , hgroup
  , hr
  , html
  , i
  , iframe
  , img
  , input
  , ins
  , kbd
  , labelTag
  , legend
  , li
  , link
  , main
  , map
  , mark
  , menu
  , meta
  , meter
  , nav
  , noscript
  , object
  , ol
  , optgroup
  , option
  , output
  , p
  , picture
  , pre
  , progress
  , q
  , rp
  , rt
  , ruby
  , s
  , sample
  , script
  , search
  , section
  , select
  , slotTag
  , small
  , source
  , spanTag
  , strong
  , styleTag
  , sub
  , summary
  , sup
  , table
  , tbody
  , td
  , template
  , textarea
  , tfoot
  , th
  , thead
  , time
  , titleTag
  , tr
  , track
  , u
  , ul
  , var
  , video
  , wbr
  , AttributeSelector
  , attributeSelectorToBytes
  , attributeSelectorToText
  , customAttribute
  , accesskey
  , autocapitalize
  , autofocus
  , class_
  , contenteditable
  , customData
  , dir
  , draggable
  , enterkeyhint
  , exportparts
  , hidden
  , id
  , inert
  , inputmode
  , is
  , itemid
  , itemprop
  , itemref
  , itemscope
  , itemtype
  , lang
  , nonce
  , part
  , popover
  , role
  , slot
  , spellcheck
  , style
  , tabindex
  , title
  , translate
  , accept
  , acceptCharset
  , action
  , allow
  , alt
  , async
  , autocomplete
  , autoplay
  , background
  , bgcolor
  , border
  , capture
  , charset
  , checked
  , cite
  , color
  , cols
  , colspan
  , content
  , controls
  , coords
  , crossorigin
  , data_
  , datetime
  , decoding
  , default_
  , defer
  , dirname
  , disabled
  , download
  , enctype
  , for
  , form
  , formaction
  , formenctype
  , formmethod
  , formnovalidate
  , formtarget
  , headers
  , height
  , high
  , href
  , hreflang
  , httpEquiv
  , integrity
  , ismap
  , kind
  , label
  , list
  , loop
  , low
  , max
  , maxlength
  , minlength
  , media
  , method
  , min
  , multiple
  , muted
  , name
  , novalidate
  , open
  , optimum
  , pattern
  , ping
  , placeholder
  , playsinline
  , poster
  , preload
  , readonly
  , referrerpolicy
  , rel
  , required
  , reversed
  , rows
  , rowspan
  , sandbox
  , scope
  , selected
  , shape
  , size
  , sizes
  , span
  , src
  , srcdoc
  , srclang
  , srcset
  , start
  , step
  , target
  , type_
  , usemap
  , value
  , width
  , wrap
  , hxGet
  , hxPost
  , hxOn
  , hxPushURL
  , hxSelect
  , hxSelectOOB
  , hxSwap
  , hxSwapOOB
  , hxTarget
  , hxTrigger
  , hxVals
  , hxBoost
  , hxConfirm
  , hxDelete
  , hxDisable
  , hxDisabledElt
  , hxDisinherit
  , hxEncoding
  , hxExt
  , hxHeaders
  , hxHistory
  , hxHistoryElt
  , hxInclude
  , hxIndicator
  , hxParams
  , hxPatch
  , hxPreserve
  , hxPrompt
  , hxPut
  , hxReplaceURL
  , hxRequest
  , hxSync
  , hxValidate
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
      , Attr_Accept
      , Attr_AcceptCharset
      , Attr_Action
      , Attr_Allow
      , Attr_Alt
      , Attr_Async
      , Attr_Autocomplete
      , Attr_Autoplay
      , Attr_Background
      , Attr_BackgroundColor
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
      , Attr_Coords
      , Attr_CrossOrigin
      , Attr_Data
      , Attr_Datetime
      , Attr_Decoding
      , Attr_Default
      , Attr_Defer
      , Attr_Dirname
      , Attr_Disabled
      , Attr_Download
      , Attr_Enctype
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
      , Attr_Integrity
      , Attr_IsMap
      , Attr_Kind
      , Attr_Label
      , Attr_List
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
      )
  , attributeTypeFromText
  , attributeTypeToText
  , Target
  , TargetTypes
  , mkTarget
  , unTarget
  , targetToBytes
  , targetToText
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
      ( Top
      , Bottom
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
  , closest
  , find
  , next
  , previous
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

import Prelude hiding (Show, div, head, id, map, max, min, show, span)
import Data.Bool qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List.NonEmpty qualified as NEL
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (KnownNat)
import Numeric.Natural (Natural)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)
import Text.Show qualified as Show

import HTML.Types.Autocapitalize (AutocapitalizeOption, autocapitalizeOptionToText)
import HTML.Types.Changed (Changed (Changed), changedToBytes, changedToText)
import HTML.Types.Class qualified as Class
import HTML.Types.ClassSelector qualified as CS
import HTML.Types.Consume (Consume (Consume), consumeToBytes, consumeToText)
import HTML.Types.ContentEditable (ContentEditableOption, contentEditableOptionToText)
import HTML.Types.CrossOrigin (CrossOriginFetch, crossoriginFetchToText)
import HTML.Types.Delay (Delay, delay, delayToBytes, delayToText)
import HTML.Types.Directionality (Directionality, directionalityToText)
import HTML.Types.Disinherit (DisinheritTypes, disinheritToText, mkDisinherit)
import HTML.Types.Document (Document, documentToBytes, documentToText)
import HTML.Types.Event qualified as Event
import HTML.Types.Every (Every, everyToBytes, everyToText)
import HTML.Types.Extension (Extension, extensionToText)
import HTML.Types.FocusScroll (FocusScroll, focusScrollToBytes, focusScrollToText)
import HTML.Types.Href (HrefSelectorTypes, hrefSelectorToText, mkHrefSelector)
import HTML.Types.Id qualified as Id
import HTML.Types.IgnoreTitle (IgnoreTitle, ignoreTitleToBytes, ignoreTitleToText)
import HTML.Types.KeyHint (KeyHintOption, keyHintOptionToText)
import HTML.Types.Method (Get, Post, Delete, Put, Patch)
import HTML.Types.NoContent (NoContent)
import HTML.Types.None (None, noneToBytes, noneToText)
import HTML.Types.Once (Once (Once), onceToBytes, onceToText)
import HTML.Types.Part (ExportPart, Part, exportPartToText, partToText)
import HTML.Types.PopoverState (PopoverState, popoverStateToText)
import HTML.Types.PushURL (PushURLTypes, mkPushURL, pushURLToText)
import HTML.Types.QueueOption (QueueOption, queueOptionToBytes, queueOptionToText)
import HTML.Types.RequestParams (RequestParams, requestParamsToText)
import HTML.Types.Swap (SwapStyle (..), swapStyleToBytes, swapStyleToText)
import HTML.Types.SwapTiming (SwapTiming, swapTimingToBytes, swapTimingToText)
import HTML.Types.SwapTransition (SwapTransition, swapTransitionToBytes, swapTransitionToText)
import HTML.Types.Target (TargetType, targetTypeToBytes, targetTypeToText)
import HTML.Types.This (This, thisToBytes, thisToText)
import HTML.Types.Threshold (Threshold, thresholdToBytes, thresholdToText)
import HTML.Types.Throttle (Throttle, throttle, throttleToBytes, throttleToText)
import HTML.Types.TriggerFilter (TriggerFilter, triggerFilterToBytes, triggerFilterToText)
import HTML.Types.URL (RelativeURL, relativeURLToText)
import HTML.Types.Vals (HtmxValsTypes, htmxValsToText, mkHtmxVals)
import HTML.Types.Window (Window, windowToBytes, windowToText)

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
    }

rawSelectorToBytes :: RawSelector -> LBS.ByteString
rawSelectorToBytes =
  LBS.fromStrict . TE.encodeUtf8 . rawSelectorToText

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

customTag :: T.Text
          -> Maybe AttributeSelector
          -> [CS.ClassSelector]
          -> Either NoContent ElementSelector
          -> ElementSelector
customTag elementName mbAttr classes eiNoContentOrElement =
  ElementSelector
    (Tag_CustomElement elementName)
    mbAttr
    classes
    (either (const Nothing) Just eiNoContentOrElement)

a :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
a = ElementSelector Tag_Anchor

abbr :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
abbr = ElementSelector Tag_Abbreviation

address :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
address = ElementSelector Tag_ContactAddress

area :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
area mbAttr classes = ElementSelector Tag_Area mbAttr classes Nothing

article :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
article = ElementSelector Tag_Article

aside :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
aside = ElementSelector Tag_Aside

audio :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
audio = ElementSelector Tag_Audio

b :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
b = ElementSelector Tag_BringAttentionTo

base :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
base mbAttr classes = ElementSelector Tag_Base mbAttr classes Nothing

bdi :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
bdi = ElementSelector Tag_BidirectionalIsolation

bdo :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
bdo = ElementSelector Tag_BidirectionalOverride

blockquote :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
blockquote = ElementSelector Tag_Blockquote

body :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
body = ElementSelector Tag_Body

br :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
br mbAttr classes = ElementSelector Tag_LineBreak mbAttr classes Nothing

button :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
button = ElementSelector Tag_Button

canvas :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
canvas = ElementSelector Tag_Canvas

caption :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
caption = ElementSelector Tag_TableCaption

citeTag :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
citeTag = ElementSelector Tag_Citation

code :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
code = ElementSelector Tag_Code

col :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
col mbAttr classes = ElementSelector Tag_TableColumn mbAttr classes Nothing

colgroup :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
colgroup = ElementSelector Tag_TableColumnGroup

dataTag :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
dataTag = ElementSelector Tag_Data

datalist :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
datalist = ElementSelector Tag_DataList

dd :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
dd = ElementSelector Tag_DescriptionDetails

del :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
del = ElementSelector Tag_DeletedText

details :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
details = ElementSelector Tag_Details

dfn :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
dfn = ElementSelector Tag_Definition

dialog :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
dialog = ElementSelector Tag_Dialog

div :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
div = ElementSelector Tag_Division

dl :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
dl = ElementSelector Tag_DescriptionList

dt :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
dt = ElementSelector Tag_DescriptionTerm

em :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
em = ElementSelector Tag_Emphasis

embed :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
embed mbAttr classes = ElementSelector Tag_Embed mbAttr classes Nothing

fieldset :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
fieldset = ElementSelector Tag_Fieldset

figcaption :: Maybe AttributeSelector
           -> [CS.ClassSelector]
           -> Maybe ElementSelector
           -> ElementSelector
figcaption = ElementSelector Tag_FigureCaption

figure :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
figure = ElementSelector Tag_Figure

footer :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
footer = ElementSelector Tag_Footer

formTag :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
formTag = ElementSelector Tag_Form

h1 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h1 = ElementSelector Tag_H1

h2 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h2 = ElementSelector Tag_H1

h3 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h3 = ElementSelector Tag_H3

h4 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h4 = ElementSelector Tag_H4

h5 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h5 = ElementSelector Tag_H5

h6 :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
h6 = ElementSelector Tag_H6

head :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
head = ElementSelector Tag_Head

header :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
header = ElementSelector Tag_Header

hgroup :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
hgroup = ElementSelector Tag_HeadingGroup

hr :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
hr mbAttr classes = ElementSelector Tag_HorizontalRule mbAttr classes Nothing

html :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
html = ElementSelector Tag_Html

i :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
i = ElementSelector Tag_IdiomaticText

iframe :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
iframe mbAttr classes = ElementSelector Tag_IFrame mbAttr classes Nothing

img :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
img mbAttr classes = ElementSelector Tag_Image mbAttr classes Nothing

input :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
input mbAttr classes = ElementSelector Tag_Input mbAttr classes Nothing

ins :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
ins = ElementSelector Tag_InsertedText

kbd :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
kbd = ElementSelector Tag_KeyboardInput

labelTag :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
labelTag = ElementSelector Tag_Label

legend :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
legend = ElementSelector Tag_Legend

li :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
li = ElementSelector Tag_ListItem

link :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
link mbAttr classes = ElementSelector Tag_Link mbAttr classes Nothing

main :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
main = ElementSelector Tag_Main

map :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
map = ElementSelector Tag_Map

mark :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
mark = ElementSelector Tag_Mark

menu :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
menu = ElementSelector Tag_Menu

meta :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
meta mbAttr classes = ElementSelector Tag_Meta mbAttr classes Nothing

meter :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
meter = ElementSelector Tag_Meter

nav :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
nav = ElementSelector Tag_Nav

noscript :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
noscript = ElementSelector Tag_NoScript

object :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
object = ElementSelector Tag_Object

ol :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
ol = ElementSelector Tag_OrderedList

optgroup :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
optgroup = ElementSelector Tag_OptionGroup

-- option does not take a child argument, since it only holds text values,
-- which cannot be targeted as part of a CSS query.
option :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
option mbAttr classes = ElementSelector Tag_Option mbAttr classes Nothing

output :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
output = ElementSelector Tag_Output

p :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
p = ElementSelector Tag_Paragraph

picture :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
picture = ElementSelector Tag_Picture

pre :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
pre = ElementSelector Tag_PreformattedText

progress :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
progress = ElementSelector Tag_Progress

q :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
q = ElementSelector Tag_Quotation

-- rp does not take a child argument, since it only holds text values, which
-- cannot be targeted as part of a CSS query.
rp :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
rp mbAttr classes = ElementSelector Tag_RubyParenthesis mbAttr classes Nothing

rt :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
rt = ElementSelector Tag_RubyText

ruby :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
ruby = ElementSelector Tag_Ruby

s :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
s = ElementSelector Tag_Strikethrough

sample :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
sample = ElementSelector Tag_Sample

script :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
script mbAttr classes = ElementSelector Tag_Script mbAttr classes Nothing

search :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
search = ElementSelector Tag_Search

section :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
section = ElementSelector Tag_Section

select :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
select = ElementSelector Tag_Select

slotTag :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
slotTag = ElementSelector Tag_Slot

small :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
small = ElementSelector Tag_SideComment

source :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
source mbAttr classes = ElementSelector Tag_Source mbAttr classes Nothing

spanTag :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
spanTag = ElementSelector Tag_Span

strong :: Maybe AttributeSelector
       -> [CS.ClassSelector]
       -> Maybe ElementSelector
       -> ElementSelector
strong = ElementSelector Tag_Strong

styleTag :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
styleTag mbAttr classes = ElementSelector Tag_Style mbAttr classes Nothing

sub :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
sub = ElementSelector Tag_Subscript

summary :: Maybe AttributeSelector
        -> [CS.ClassSelector]
        -> Maybe ElementSelector
        -> ElementSelector
summary = ElementSelector Tag_Summary

sup :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
sup = ElementSelector Tag_Superscript

table :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
table = ElementSelector Tag_Table

tbody :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tbody = ElementSelector Tag_TableBody

td :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
td = ElementSelector Tag_TableDataCell

template :: Maybe AttributeSelector
         -> [CS.ClassSelector]
         -> Maybe ElementSelector
         -> ElementSelector
template = ElementSelector Tag_ContentTemplate

-- textarea does not take a child argument, since it only holds text values,
-- which cannot be targeted as part of a CSS query.
textarea :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
textarea mbAttr classes = ElementSelector Tag_TextArea mbAttr classes Nothing

tfoot :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
tfoot = ElementSelector Tag_TableFoot

th :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
th = ElementSelector Tag_TableHeader

thead :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
thead = ElementSelector Tag_TableHead

time :: Maybe AttributeSelector
     -> [CS.ClassSelector]
     -> Maybe ElementSelector
     -> ElementSelector
time = ElementSelector Tag_Time

-- title does not take a child argument, since it only holds text values, which
-- cannot be targeted as part of a CSS query.
titleTag :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
titleTag mbAttr classes = ElementSelector Tag_Title mbAttr classes Nothing

tr :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
tr = ElementSelector Tag_TableRow

track :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
track mbAttr classes = ElementSelector Tag_Track mbAttr classes Nothing

u :: Maybe AttributeSelector
  -> [CS.ClassSelector]
  -> Maybe ElementSelector
  -> ElementSelector
u = ElementSelector Tag_Underline

ul :: Maybe AttributeSelector
   -> [CS.ClassSelector]
   -> Maybe ElementSelector
   -> ElementSelector
ul = ElementSelector Tag_UnorderedList

var :: Maybe AttributeSelector
    -> [CS.ClassSelector]
    -> Maybe ElementSelector
    -> ElementSelector
var = ElementSelector Tag_Variable

video :: Maybe AttributeSelector
      -> [CS.ClassSelector]
      -> Maybe ElementSelector
      -> ElementSelector
video = ElementSelector Tag_Video

wbr :: Maybe AttributeSelector -> [CS.ClassSelector] -> ElementSelector
wbr mbAttr classes =
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
    Tag_CustomElement tagName  -> LBS.fromStrict $ TE.encodeUtf8 tagName
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
    Tag_Sample                 -> "sample"
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
    Tag_Sample                 -> "sample"
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
    , maybe "" (\v -> "='" <> LBS.fromStrict (TE.encodeUtf8 v) <> "'") mbVal
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

  -- Scoped Attributes
  --
  | Attr_Accept
  | Attr_AcceptCharset
  | Attr_Action
  | Attr_Allow
  | Attr_Alt
  | Attr_Async
  | Attr_Autocomplete
  | Attr_Autoplay
  | Attr_Background
  | Attr_BackgroundColor
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
  | Attr_Coords
  | Attr_CrossOrigin
  | Attr_Data
  | Attr_Datetime
  | Attr_Decoding
  | Attr_Default
  | Attr_Defer
  | Attr_Dirname
  | Attr_Disabled
  | Attr_Download
  | Attr_Enctype
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
  | Attr_Integrity
  | Attr_IsMap
  | Attr_Kind
  | Attr_Label
  | Attr_List
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
  | Attr_NoValidate
  | Attr_Open
  | Attr_Optimum
  | Attr_Pattern
  | Attr_Ping
  | Attr_Placeholder
  | Attr_PlaysInline
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

attributeTypeToBytes :: AttributeType -> LBS.ByteString
attributeTypeToBytes attr =
  case attr of
    -- Custom Attribute
    Attr_CustomAttribute attrName -> LBS.fromStrict $ TE.encodeUtf8 attrName

    -- Global Attributes
    --
    Attr_AccessKey           -> "accesskey"
    Attr_Autocapitalize      -> "autocapitalize"
    Attr_Autofocus           -> "autofocus"
    Attr_Class               -> "class"
    Attr_ContentEditable     -> "contenteditable"
    Attr_CustomData attrName -> "data-" <> LBS.fromStrict (TE.encodeUtf8 attrName)
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

    -- Scoped Attributes
    --
    Attr_Accept          -> "accept"
    Attr_AcceptCharset   -> "accept-charset"
    Attr_Action          -> "action"
    Attr_Allow           -> "allow"
    Attr_Alt             -> "alt"
    Attr_Async           -> "async"
    Attr_Autocomplete    -> "autocomplete"
    Attr_Autoplay        -> "autoplay"
    Attr_Background      -> "background"
    Attr_BackgroundColor -> "bgcolor"
    Attr_Border          -> "border"
    Attr_Capture         -> "capture"
    Attr_Charset         -> "charset"
    Attr_Checked         -> "checked"
    Attr_Cite            -> "cite"
    Attr_Color           -> "color"
    Attr_Cols            -> "cols"
    Attr_Colspan         -> "colspan"
    Attr_Content         -> "content"
    Attr_Controls        -> "controls"
    Attr_Coords          -> "coords"
    Attr_CrossOrigin     -> "crossorigin"
    Attr_Data            -> "data"
    Attr_Datetime        -> "datetime"
    Attr_Decoding        -> "decoding"
    Attr_Default         -> "default"
    Attr_Defer           -> "defer"
    Attr_Dirname         -> "dirname"
    Attr_Disabled        -> "disabled"
    Attr_Download        -> "download"
    Attr_Enctype         -> "enctype"
    Attr_For             -> "for"
    Attr_Form            -> "form"
    Attr_FormAction      -> "formaction"
    Attr_FormEnctype     -> "formenctype"
    Attr_FormMethod      -> "formmethod"
    Attr_FormNoValidate  -> "formnovalidate"
    Attr_FormTarget      -> "formtarget"
    Attr_Headers         -> "headers"
    Attr_Height          -> "height"
    Attr_High            -> "high"
    Attr_Href            -> "href"
    Attr_HrefLang        -> "hreflang"
    Attr_HttpEquiv       -> "http-equiv"
    Attr_Integrity       -> "integrity"
    Attr_IsMap           -> "ismap"
    Attr_Kind            -> "kind"
    Attr_Label           -> "label"
    Attr_List            -> "list"
    Attr_Loop            -> "loop"
    Attr_Low             -> "low"
    Attr_Max             -> "max"
    Attr_MaxLength       -> "maxlength"
    Attr_MinLength       -> "minlength"
    Attr_Media           -> "media"
    Attr_Method          -> "method"
    Attr_Min             -> "min"
    Attr_Multiple        -> "multiple"
    Attr_Muted           -> "muted"
    Attr_Name            -> "name"
    Attr_NoValidate      -> "novalidate"
    Attr_Open            -> "open"
    Attr_Optimum         -> "optimum"
    Attr_Pattern         -> "pattern"
    Attr_Ping            -> "ping"
    Attr_Placeholder     -> "placeholder"
    Attr_PlaysInline     -> "playsinline"
    Attr_Poster          -> "poster"
    Attr_Preload         -> "preload"
    Attr_ReadOnly        -> "readonly"
    Attr_ReferrerPolicy  -> "referrerpolicy"
    Attr_Rel             -> "rel"
    Attr_Required        -> "required"
    Attr_Reversed        -> "reversed"
    Attr_Rows            -> "rows"
    Attr_Rowspan         -> "rowspan"
    Attr_Sandbox         -> "sandbox"
    Attr_Scope           -> "scope"
    Attr_Selected        -> "selected"
    Attr_Shape           -> "shape"
    Attr_Size            -> "size"
    Attr_Sizes           -> "sizes"
    Attr_Span            -> "span"
    Attr_Src             -> "src"
    Attr_SrcDoc          -> "srcdoc"
    Attr_SrcLang         -> "srclang"
    Attr_SrcSet          -> "srcset"
    Attr_Start           -> "start"
    Attr_Step            -> "step"
    Attr_Target          -> "target"
    Attr_Type            -> "type"
    Attr_UseMap          -> "usemap"
    Attr_Value           -> "value"
    Attr_Width           -> "width"
    Attr_Wrap            -> "wrap"

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

-- The default case here is to treat it as a `CustomAttribute`.
attributeTypeFromText :: T.Text -> Either String AttributeType
attributeTypeFromText attr =
  case attr of
    -- Global Attributes
    --
    "accesskey"       -> Right Attr_AccessKey
    "autocapitalize"  -> Right Attr_Autocapitalize
    "autofocus"       -> Right Attr_Autofocus
    "class"           -> Right Attr_Class
    "contenteditable" -> Right Attr_ContentEditable
    "dir"             -> Right Attr_Dir
    "draggable"       -> Right Attr_Draggable
    "enterkeyhint"    -> Right Attr_EnterKeyHint
    "exportparts"     -> Right Attr_ExportParts
    "hidden"          -> Right Attr_Hidden
    "id"              -> Right Attr_Id
    "inert"           -> Right Attr_Inert
    "inputmode"       -> Right Attr_InputMode
    "is"              -> Right Attr_Is
    "itemid"          -> Right Attr_ItemId
    "itemprop"        -> Right Attr_ItemProp
    "itemref"         -> Right Attr_ItemRef
    "itemscope"       -> Right Attr_ItemScope
    "itemtype"        -> Right Attr_ItemType
    "lang"            -> Right Attr_Lang
    "nonce"           -> Right Attr_Nonce
    "part"            -> Right Attr_Part
    "popover"         -> Right Attr_Popover
    "role"            -> Right Attr_Role
    "slot"            -> Right Attr_Slot
    "spellcheck"      -> Right Attr_Spellcheck
    "style"           -> Right Attr_Style
    "tabindex"        -> Right Attr_TabIndex
    "title"           -> Right Attr_Title
    "translate"       -> Right Attr_Translate

    -- Scoped Attributes
    --
    "accept"         -> Right Attr_Accept
    "accept-charset" -> Right Attr_AcceptCharset
    "action"         -> Right Attr_Action
    "allow"          -> Right Attr_Allow
    "alt"            -> Right Attr_Alt
    "async"          -> Right Attr_Async
    "autocomplete"   -> Right Attr_Autocomplete
    "autoplay"       -> Right Attr_Autoplay
    "background"     -> Right Attr_Background
    "bgcolor"        -> Right Attr_BackgroundColor
    "border"         -> Right Attr_Border
    "capture"        -> Right Attr_Capture
    "charset"        -> Right Attr_Charset
    "checked"        -> Right Attr_Checked
    "cite"           -> Right Attr_Cite
    "color"          -> Right Attr_Color
    "cols"           -> Right Attr_Cols
    "colspan"        -> Right Attr_Colspan
    "content"        -> Right Attr_Content
    "controls"       -> Right Attr_Controls
    "coords"         -> Right Attr_Coords
    "crossorigin"    -> Right Attr_CrossOrigin
    "data"           -> Right Attr_Data
    "datetime"       -> Right Attr_Datetime
    "decoding"       -> Right Attr_Decoding
    "default"        -> Right Attr_Default
    "defer"          -> Right Attr_Defer
    "dirname"        -> Right Attr_Dirname
    "disabled"       -> Right Attr_Disabled
    "download"       -> Right Attr_Download
    "enctype"        -> Right Attr_Enctype
    "for"            -> Right Attr_For
    "form"           -> Right Attr_Form
    "formaction"     -> Right Attr_FormAction
    "formenctype"    -> Right Attr_FormEnctype
    "formmethod"     -> Right Attr_FormMethod
    "formnovalidate" -> Right Attr_FormNoValidate
    "formtarget"     -> Right Attr_FormTarget
    "headers"        -> Right Attr_Headers
    "height"         -> Right Attr_Height
    "high"           -> Right Attr_High
    "href"           -> Right Attr_Href
    "hreflang"       -> Right Attr_HrefLang
    "http-equiv"     -> Right Attr_HttpEquiv
    "integrity"      -> Right Attr_Integrity
    "ismap"          -> Right Attr_IsMap
    "kind"           -> Right Attr_Kind
    "label"          -> Right Attr_Label
    "list"           -> Right Attr_List
    "loop"           -> Right Attr_Loop
    "low"            -> Right Attr_Low
    "max"            -> Right Attr_Max
    "maxlength"      -> Right Attr_MaxLength
    "minlength"      -> Right Attr_MinLength
    "media"          -> Right Attr_Media
    "method"         -> Right Attr_Method
    "min"            -> Right Attr_Min
    "multiple"       -> Right Attr_Multiple
    "muted"          -> Right Attr_Muted
    "name"           -> Right Attr_Name
    "novalidate"     -> Right Attr_NoValidate
    "open"           -> Right Attr_Open
    "optimum"        -> Right Attr_Optimum
    "pattern"        -> Right Attr_Pattern
    "ping"           -> Right Attr_Ping
    "placeholder"    -> Right Attr_Placeholder
    "playsinline"    -> Right Attr_PlaysInline
    "poster"         -> Right Attr_Poster
    "preload"        -> Right Attr_Preload
    "readonly"       -> Right Attr_ReadOnly
    "referrerpolicy" -> Right Attr_ReferrerPolicy
    "rel"            -> Right Attr_Rel
    "required"       -> Right Attr_Required
    "reversed"       -> Right Attr_Reversed
    "rows"           -> Right Attr_Rows
    "rowspan"        -> Right Attr_Rowspan
    "sandbox"        -> Right Attr_Sandbox
    "scope"          -> Right Attr_Scope
    "selected"       -> Right Attr_Selected
    "shape"          -> Right Attr_Shape
    "size"           -> Right Attr_Size
    "sizes"          -> Right Attr_Sizes
    "span"           -> Right Attr_Span
    "src"            -> Right Attr_Src
    "srcdoc"         -> Right Attr_SrcDoc
    "srclang"        -> Right Attr_SrcLang
    "srcset"         -> Right Attr_SrcSet
    "start"          -> Right Attr_Start
    "step"           -> Right Attr_Step
    "target"         -> Right Attr_Target
    "type"           -> Right Attr_Type
    "usemap"         -> Right Attr_UseMap
    "value"          -> Right Attr_Value
    "width"          -> Right Attr_Width
    "wrap"           -> Right Attr_Wrap

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

    -- Scoped Attributes
    --
    Attr_Accept          -> "accept"
    Attr_AcceptCharset   -> "accept-charset"
    Attr_Action          -> "action"
    Attr_Allow           -> "allow"
    Attr_Alt             -> "alt"
    Attr_Async           -> "async"
    Attr_Autocomplete    -> "autocomplete"
    Attr_Autoplay        -> "autoplay"
    Attr_Background      -> "background"
    Attr_BackgroundColor -> "bgcolor"
    Attr_Border          -> "border"
    Attr_Capture         -> "capture"
    Attr_Charset         -> "charset"
    Attr_Checked         -> "checked"
    Attr_Cite            -> "cite"
    Attr_Color           -> "color"
    Attr_Cols            -> "cols"
    Attr_Colspan         -> "colspan"
    Attr_Content         -> "content"
    Attr_Controls        -> "controls"
    Attr_Coords          -> "coords"
    Attr_CrossOrigin     -> "crossorigin"
    Attr_Data            -> "data"
    Attr_Datetime        -> "datetime"
    Attr_Decoding        -> "decoding"
    Attr_Default         -> "default"
    Attr_Defer           -> "defer"
    Attr_Dirname         -> "dirname"
    Attr_Disabled        -> "disabled"
    Attr_Download        -> "download"
    Attr_Enctype         -> "enctype"
    Attr_For             -> "for"
    Attr_Form            -> "form"
    Attr_FormAction      -> "formaction"
    Attr_FormEnctype     -> "formenctype"
    Attr_FormMethod      -> "formmethod"
    Attr_FormNoValidate  -> "formnovalidate"
    Attr_FormTarget      -> "formtarget"
    Attr_Headers         -> "headers"
    Attr_Height          -> "height"
    Attr_High            -> "high"
    Attr_Href            -> "href"
    Attr_HrefLang        -> "hreflang"
    Attr_HttpEquiv       -> "http-equiv"
    Attr_Integrity       -> "integrity"
    Attr_IsMap           -> "ismap"
    Attr_Kind            -> "kind"
    Attr_Label           -> "label"
    Attr_List            -> "list"
    Attr_Loop            -> "loop"
    Attr_Low             -> "low"
    Attr_Max             -> "max"
    Attr_MaxLength       -> "maxlength"
    Attr_MinLength       -> "minlength"
    Attr_Media           -> "media"
    Attr_Method          -> "method"
    Attr_Min             -> "min"
    Attr_Multiple        -> "multiple"
    Attr_Muted           -> "muted"
    Attr_Name            -> "name"
    Attr_NoValidate      -> "novalidate"
    Attr_Open            -> "open"
    Attr_Optimum         -> "optimum"
    Attr_Pattern         -> "pattern"
    Attr_Ping            -> "ping"
    Attr_Placeholder     -> "placeholder"
    Attr_PlaysInline     -> "playsinline"
    Attr_Poster          -> "poster"
    Attr_Preload         -> "preload"
    Attr_ReadOnly        -> "readonly"
    Attr_ReferrerPolicy  -> "referrerpolicy"
    Attr_Rel             -> "rel"
    Attr_Required        -> "required"
    Attr_Reversed        -> "reversed"
    Attr_Rows            -> "rows"
    Attr_Rowspan         -> "rowspan"
    Attr_Sandbox         -> "sandbox"
    Attr_Scope           -> "scope"
    Attr_Selected        -> "selected"
    Attr_Shape           -> "shape"
    Attr_Size            -> "size"
    Attr_Sizes           -> "sizes"
    Attr_Span            -> "span"
    Attr_Src             -> "src"
    Attr_SrcDoc          -> "srcdoc"
    Attr_SrcLang         -> "srclang"
    Attr_SrcSet          -> "srcset"
    Attr_Start           -> "start"
    Attr_Step            -> "step"
    Attr_Target          -> "target"
    Attr_Type            -> "type"
    Attr_UseMap          -> "usemap"
    Attr_Value           -> "value"
    Attr_Width           -> "width"
    Attr_Wrap            -> "wrap"

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

customAttribute :: T.Text -> T.Text -> AttributeSelector
customAttribute attrName val =
  (Attr_CustomAttribute attrName, Just val)

-- Global Attributes
--

accesskey :: Char -> AttributeSelector
accesskey = (,) Attr_AccessKey . Just . T.singleton

autocapitalize :: AutocapitalizeOption -> AttributeSelector
autocapitalize = (,) Attr_Autocapitalize . Just . autocapitalizeOptionToText

autofocus :: T.Text -> AttributeSelector
autofocus = (,) Attr_Autofocus . Just

class_ :: Class.Class -> AttributeSelector
class_ = (,) Attr_Class . Just . Class.classToText

contenteditable :: ContentEditableOption -> AttributeSelector
contenteditable = (,) Attr_ContentEditable . Just . contentEditableOptionToText

customData :: T.Text -> T.Text -> AttributeSelector
customData dataName val = (Attr_CustomData dataName, Just val)

dir :: Directionality -> AttributeSelector
dir = (,) Attr_Dir . Just . directionalityToText

draggable :: Bool -> AttributeSelector
draggable = (,) Attr_Draggable . Just . enumBoolToText

enterkeyhint :: KeyHintOption -> AttributeSelector
enterkeyhint = (,) Attr_EnterKeyHint . Just . keyHintOptionToText

exportparts :: NEL.NonEmpty ExportPart -> AttributeSelector
exportparts =
  (,) Attr_ExportParts
    . Just
    . T.intercalate ", "
    . fmap exportPartToText
    . NEL.toList

hidden :: AttributeSelector
hidden = (Attr_Hidden, Nothing)

id :: Id.Id -> AttributeSelector
id = (,) Attr_Id . Just . Id.idToText

inert :: AttributeSelector
inert = (Attr_Inert, Nothing)

-- TODO
inputmode :: T.Text -> AttributeSelector
inputmode = (,) Attr_InputMode . Just

is :: T.Text -> AttributeSelector
is = (,) Attr_Is . Just

-- TODO
itemid :: T.Text -> AttributeSelector
itemid = (,) Attr_ItemId . Just

-- TODO
itemprop :: T.Text -> AttributeSelector
itemprop = (,) Attr_ItemProp . Just

-- TODO
itemref :: T.Text -> AttributeSelector
itemref = (,) Attr_ItemRef . Just

-- TODO
itemscope :: T.Text -> AttributeSelector
itemscope = (,) Attr_ItemScope . Just

-- TODO
itemtype :: T.Text -> AttributeSelector
itemtype = (,) Attr_ItemType . Just

-- TODO
lang :: T.Text -> AttributeSelector
lang = (,) Attr_Lang . Just

-- TODO
nonce :: T.Text -> AttributeSelector
nonce = (,) Attr_Nonce . Just

part :: NEL.NonEmpty Part -> AttributeSelector
part = (,) Attr_Part . Just . T.unwords . fmap partToText . NEL.toList

popover :: PopoverState -> AttributeSelector
popover = (,) Attr_Popover . Just . popoverStateToText

-- TODO
role :: T.Text -> AttributeSelector
role = (,) Attr_Role . Just

-- TODO
slot :: T.Text -> AttributeSelector
slot = (,) Attr_Slot . Just

spellcheck :: Bool -> AttributeSelector
spellcheck = (,) Attr_Spellcheck . Just . enumBoolToText

style :: T.Text -> AttributeSelector
style = (,) Attr_Style . Just

tabindex :: Int -> AttributeSelector
tabindex = (,) Attr_TabIndex . Just . showText

title :: T.Text -> AttributeSelector
title = (,) Attr_Title . Just

translate :: Bool -> AttributeSelector
translate = (,) Attr_Translate . Just . enumBoolToText

-- Scoped Attributes
--

-- TODO
accept :: T.Text -> AttributeSelector
accept = (,) Attr_Accept . Just

-- TODO
acceptCharset :: T.Text -> AttributeSelector
acceptCharset = (,) Attr_AcceptCharset . Just

-- TODO
action :: T.Text -> AttributeSelector
action = (,) Attr_Action . Just

-- TODO
allow :: T.Text -> AttributeSelector
allow = (,) Attr_Allow . Just

-- TODO
alt :: T.Text -> AttributeSelector
alt = (,) Attr_Alt . Just

-- TODO
async :: T.Text -> AttributeSelector
async = (,) Attr_Async . Just

-- TODO
autocomplete :: T.Text -> AttributeSelector
autocomplete = (,) Attr_Autocomplete . Just

-- TODO
autoplay :: T.Text -> AttributeSelector
autoplay = (,) Attr_Autoplay . Just

-- TODO
background :: T.Text -> AttributeSelector
background = (,) Attr_Background . Just

-- TODO
bgcolor :: T.Text -> AttributeSelector
bgcolor = (,) Attr_BackgroundColor . Just

-- TODO
border :: T.Text -> AttributeSelector
border = (,) Attr_Border . Just

-- TODO
capture :: T.Text -> AttributeSelector
capture = (,) Attr_Capture . Just

-- TODO
charset :: T.Text -> AttributeSelector
charset = (,) Attr_Charset . Just

-- TODO
checked :: T.Text -> AttributeSelector
checked = (,) Attr_Checked . Just

-- TODO
cite :: T.Text -> AttributeSelector
cite = (,) Attr_Cite . Just

-- TODO
color :: T.Text -> AttributeSelector
color = (,) Attr_Color . Just

-- TODO
cols :: T.Text -> AttributeSelector
cols = (,) Attr_Cols . Just

-- TODO
colspan :: T.Text -> AttributeSelector
colspan = (,) Attr_Colspan . Just

-- TODO
content :: T.Text -> AttributeSelector
content = (,) Attr_Content . Just

-- TODO
controls :: T.Text -> AttributeSelector
controls = (,) Attr_Controls . Just

-- TODO
coords :: T.Text -> AttributeSelector
coords = (,) Attr_Coords . Just

crossorigin :: CrossOriginFetch -> AttributeSelector
crossorigin = (,) Attr_CrossOrigin . Just . crossoriginFetchToText

-- TODO
data_ :: T.Text -> AttributeSelector
data_ = (,) Attr_Data . Just

-- TODO
datetime :: T.Text -> AttributeSelector
datetime = (,) Attr_Datetime . Just

-- TODO
decoding :: T.Text -> AttributeSelector
decoding = (,) Attr_Decoding . Just

-- TODO
default_ :: T.Text -> AttributeSelector
default_ = (,) Attr_Default . Just

-- TODO
defer :: T.Text -> AttributeSelector
defer = (,) Attr_Defer . Just

-- TODO
dirname :: T.Text -> AttributeSelector
dirname = (,) Attr_Dirname . Just

disabled :: AttributeSelector
disabled = (Attr_Disabled, Nothing)

-- TODO
download :: T.Text -> AttributeSelector
download = (,) Attr_Download . Just

-- TODO
enctype :: T.Text -> AttributeSelector
enctype = (,) Attr_Enctype . Just

-- TODO
for :: T.Text -> AttributeSelector
for = (,) Attr_For . Just

-- TODO
form :: T.Text -> AttributeSelector
form = (,) Attr_Form . Just

-- TODO
formaction :: T.Text -> AttributeSelector
formaction = (,) Attr_FormAction . Just

-- TODO
formenctype :: T.Text -> AttributeSelector
formenctype = (,) Attr_FormEnctype . Just

-- TODO
formmethod :: T.Text -> AttributeSelector
formmethod = (,) Attr_FormMethod . Just

-- TODO
formnovalidate :: T.Text -> AttributeSelector
formnovalidate = (,) Attr_FormNoValidate . Just

-- TODO
formtarget :: T.Text -> AttributeSelector
formtarget = (,) Attr_FormTarget . Just

-- TODO
headers :: T.Text -> AttributeSelector
headers = (,) Attr_Headers . Just

-- TODO
height :: T.Text -> AttributeSelector
height = (,) Attr_Height . Just

-- TODO
high :: T.Text -> AttributeSelector
high = (,) Attr_High . Just

href :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf href HrefSelectorTypes
        )
     => href -> AttributeSelector
href = (,) Attr_Href . Just . hrefSelectorToText . mkHrefSelector

-- TODO
hreflang :: T.Text -> AttributeSelector
hreflang = (,) Attr_HrefLang . Just

-- TODO
httpEquiv :: T.Text -> AttributeSelector
httpEquiv = (,) Attr_HttpEquiv . Just

-- TODO
integrity :: T.Text -> AttributeSelector
integrity = (,) Attr_Integrity . Just

-- TODO
ismap :: T.Text -> AttributeSelector
ismap = (,) Attr_IsMap . Just

-- TODO
kind :: T.Text -> AttributeSelector
kind = (,) Attr_Kind . Just

-- TODO
label :: T.Text -> AttributeSelector
label = (,) Attr_Label . Just

-- TODO
list :: T.Text -> AttributeSelector
list = (,) Attr_List . Just

-- TODO
loop :: T.Text -> AttributeSelector
loop = (,) Attr_Loop . Just

-- TODO
low :: T.Text -> AttributeSelector
low = (,) Attr_Low . Just

-- TODO
max :: T.Text -> AttributeSelector
max = (,) Attr_Max . Just

-- TODO
maxlength :: T.Text -> AttributeSelector
maxlength = (,) Attr_MaxLength . Just

-- TODO
minlength :: T.Text -> AttributeSelector
minlength = (,) Attr_MinLength . Just

-- TODO
media :: T.Text -> AttributeSelector
media = (,) Attr_Media . Just

-- TODO
method :: T.Text -> AttributeSelector
method = (,) Attr_Method . Just

-- TODO
min :: T.Text -> AttributeSelector
min = (,) Attr_Min . Just

-- TODO
multiple :: T.Text -> AttributeSelector
multiple = (,) Attr_Multiple . Just

-- TODO
muted :: T.Text -> AttributeSelector
muted = (,) Attr_Muted . Just

-- TODO
name :: T.Text -> AttributeSelector
name = (,) Attr_Name . Just

-- TODO
novalidate :: T.Text -> AttributeSelector
novalidate = (,) Attr_NoValidate . Just

-- TODO
open :: T.Text -> AttributeSelector
open = (,) Attr_Open . Just

-- TODO
optimum :: T.Text -> AttributeSelector
optimum = (,) Attr_Optimum . Just

-- TODO
pattern :: T.Text -> AttributeSelector
pattern = (,) Attr_Pattern . Just

-- TODO
ping :: T.Text -> AttributeSelector
ping = (,) Attr_Ping . Just

-- TODO
placeholder :: T.Text -> AttributeSelector
placeholder = (,) Attr_Placeholder . Just

-- TODO
playsinline :: T.Text -> AttributeSelector
playsinline = (,) Attr_PlaysInline . Just

-- TODO
poster :: T.Text -> AttributeSelector
poster = (,) Attr_Poster . Just

-- TODO
preload :: T.Text -> AttributeSelector
preload = (,) Attr_Preload . Just

-- TODO
readonly :: T.Text -> AttributeSelector
readonly = (,) Attr_ReadOnly . Just

-- TODO
referrerpolicy :: T.Text -> AttributeSelector
referrerpolicy = (,) Attr_ReferrerPolicy . Just

-- TODO
rel :: T.Text -> AttributeSelector
rel = (,) Attr_Rel . Just

-- TODO
required :: T.Text -> AttributeSelector
required = (,) Attr_Required . Just

-- TODO
reversed :: T.Text -> AttributeSelector
reversed = (,) Attr_Reversed . Just

-- TODO
rows :: T.Text -> AttributeSelector
rows = (,) Attr_Rows . Just

-- TODO
rowspan :: T.Text -> AttributeSelector
rowspan = (,) Attr_Rowspan . Just

-- TODO
sandbox :: T.Text -> AttributeSelector
sandbox = (,) Attr_Sandbox . Just

-- TODO
scope :: T.Text -> AttributeSelector
scope = (,) Attr_Scope . Just

-- TODO
selected :: T.Text -> AttributeSelector
selected = (,) Attr_Selected . Just

-- TODO
shape :: T.Text -> AttributeSelector
shape = (,) Attr_Shape . Just

-- TODO
size :: T.Text -> AttributeSelector
size = (,) Attr_Size . Just

-- TODO
sizes :: T.Text -> AttributeSelector
sizes = (,) Attr_Sizes . Just

-- TODO
span :: T.Text -> AttributeSelector
span = (,) Attr_Span . Just

-- TODO
src :: T.Text -> AttributeSelector
src = (,) Attr_Src . Just

-- TODO
srcdoc :: T.Text -> AttributeSelector
srcdoc = (,) Attr_SrcDoc . Just

-- TODO
srclang :: T.Text -> AttributeSelector
srclang = (,) Attr_SrcLang . Just

-- TODO
srcset :: T.Text -> AttributeSelector
srcset = (,) Attr_SrcSet . Just

-- TODO
start :: T.Text -> AttributeSelector
start = (,) Attr_Start . Just

-- TODO
step :: T.Text -> AttributeSelector
step = (,) Attr_Step . Just

-- TODO
target :: T.Text -> AttributeSelector
target = (,) Attr_Target . Just

-- TODO
type_ :: T.Text -> AttributeSelector
type_ = (,) Attr_Type . Just

-- TODO
usemap :: T.Text -> AttributeSelector
usemap = (,) Attr_UseMap . Just

-- TODO
value :: T.Text -> AttributeSelector
value = (,) Attr_Value . Just

-- TODO
width :: T.Text -> AttributeSelector
width = (,) Attr_Width . Just

-- TODO
wrap :: T.Text -> AttributeSelector
wrap = (,) Attr_Wrap . Just

-- HTMX Attributes
--

hxGet :: RelativeURL Get -> AttributeSelector
hxGet = (,) Attr_HxGet . Just . relativeURLToText

hxPost :: RelativeURL Post -> AttributeSelector
hxPost = (,) Attr_HxPost . Just . relativeURLToText

hxOn :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf eventType Event.EventTypes
        )
     => eventType -> T.Text -> AttributeSelector
hxOn eventType eventAction =
  ( Attr_HxOn . Event.hxOnEventText $ Event.mkEvent eventType
  , Just eventAction
  )

hxPushURL :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf url PushURLTypes
             )
          => url -> AttributeSelector
hxPushURL = (,) Attr_HxPushURL . Just . pushURLToText . mkPushURL

hxSelect :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
            )
         => querySelector -> AttributeSelector
hxSelect = (,) Attr_HxSelect . Just . querySelectorToText . mkQuerySelector

hxSelectOOB :: NEL.NonEmpty OutOfBandSelect -> AttributeSelector
hxSelectOOB =
  (,) Attr_HxSelectOOB
    . Just
    . T.intercalate ", "
    . fmap outOfBandSelectToText
    . NEL.toList

hxSwap :: (KnownNat branchIndex, branchIndex ~ FirstIndexOf swap SwapTypes)
       => swap -> AttributeSelector
hxSwap = (,) Attr_HxSwap . Just . swapToText . mkSwap

hxSwapOOB :: ( KnownNat branchIndex
             , branchIndex ~ FirstIndexOf swap OutOfBandSwapTypes
             )
          => Maybe swap -> AttributeSelector
hxSwapOOB =
  (,) Attr_HxSwapOOB
    . Just
    . maybe "true" (outOfBandSwapToText . mkOutOfBandSwap)

hxTarget :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf target TargetTypes
            )
         => target -> AttributeSelector
hxTarget = (,) Attr_HxTarget . Just . targetToText . mkTarget

hxTrigger :: NEL.NonEmpty Trigger -> AttributeSelector
hxTrigger =
  (,) Attr_HxTrigger
    . Just
    . T.intercalate ", "
    . fmap triggerToText
    . NEL.toList

hxVals :: (KnownNat branchIndex, branchIndex ~ FirstIndexOf vals HtmxValsTypes)
       => vals -> AttributeSelector
hxVals = (,) Attr_HxVals . Just . htmxValsToText . mkHtmxVals

hxBoost :: Bool -> AttributeSelector
hxBoost = (,) Attr_HxBoost . Just . enumBoolToText

hxConfirm :: T.Text -> AttributeSelector
hxConfirm = (,) Attr_HxConfirm . Just

hxDelete :: RelativeURL Delete -> AttributeSelector
hxDelete = (,) Attr_HxDelete . Just . relativeURLToText

hxDisable :: AttributeSelector
hxDisable = (Attr_HxDisable, Nothing)

-- TODO
hxDisabledElt :: T.Text -> AttributeSelector
hxDisabledElt = (,) Attr_HxDisabledElt . Just

hxDisinherit :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf disinherit DisinheritTypes
                )
             => disinherit -> AttributeSelector
hxDisinherit = (,) Attr_HxDisinherit . Just . disinheritToText . mkDisinherit

hxEncoding :: AttributeSelector
hxEncoding = (Attr_HxEncoding, Just "multipart/form-data")

hxExt :: NEL.NonEmpty Extension -> AttributeSelector
hxExt =
  (,) Attr_HxExt
    . Just
    . T.intercalate ","
    . fmap extensionToText
    . NEL.toList

-- TODO
hxHeaders :: T.Text -> AttributeSelector
hxHeaders = (,) Attr_HxHeaders . Just

hxHistory :: AttributeSelector
hxHistory = (Attr_HxHistory, Just "false")

hxHistoryElt :: AttributeSelector
hxHistoryElt = (Attr_HxHistoryElt, Nothing)

-- TODO
hxInclude :: T.Text -> AttributeSelector
hxInclude = (,) Attr_HxInclude . Just

-- TODO
hxIndicator :: T.Text -> AttributeSelector
hxIndicator = (,) Attr_HxIndicator . Just

hxParams :: RequestParams -> AttributeSelector
hxParams = (,) Attr_HxParams . Just . requestParamsToText

hxPatch :: RelativeURL Patch -> AttributeSelector
hxPatch = (,) Attr_HxPatch . Just . relativeURLToText

hxPreserve :: AttributeSelector
hxPreserve = (Attr_HxPreserve, Nothing)

hxPrompt :: T.Text -> AttributeSelector
hxPrompt = (,) Attr_HxPrompt . Just

hxPut :: RelativeURL Put -> AttributeSelector
hxPut = (,) Attr_HxPut . Just . relativeURLToText

hxReplaceURL :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf url PushURLTypes
                )
             => url -> AttributeSelector
hxReplaceURL = (,) Attr_HxReplaceURL . Just . pushURLToText . mkPushURL

-- TODO
hxRequest :: T.Text -> AttributeSelector
hxRequest = (,) Attr_HxRequest . Just

-- TODO
hxSync :: T.Text -> AttributeSelector
hxSync = (,) Attr_HxSync . Just

hxValidate :: AttributeSelector
hxValidate = (Attr_HxValidate, Nothing)

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
  = Top
  | Bottom

swapDisplayViewToBytes :: SwapDisplayView -> LBS.ByteString
swapDisplayViewToBytes view =
  case view of
    Top    -> "top"
    Bottom -> "bottom"

swapDisplayViewToText :: SwapDisplayView -> T.Text
swapDisplayViewToText view =
  case view of
    Top    -> "top"
    Bottom -> "bottom"

newtype RawSwap =
  RawSwap
    { rawSwapToText :: T.Text
    }

rawSwapToBytes :: RawSwap -> LBS.ByteString
rawSwapToBytes = LBS.fromStrict . TE.encodeUtf8 . rawSwapToText

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

-- Target and TargetSelector
--
newtype Target =
  Target
    { unTarget :: Shrubbery.Union TargetTypes
    }

type TargetTypes =
  [ QuerySelector
  , TargetSelector
  , TargetType
  , This
  , RawSelector
  ]

mkTarget :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf target TargetTypes
            )
         => target -> Target
mkTarget =
  Target . Shrubbery.unify

targetToBytes :: Target -> LBS.ByteString
targetToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @QuerySelector querySelectorToBytes
      . Shrubbery.branch @TargetSelector targetSelectorToBytes
      . Shrubbery.branch @TargetType targetTypeToBytes
      . Shrubbery.branch @This thisToBytes
      . Shrubbery.branch @RawSelector rawSelectorToBytes
      $ Shrubbery.branchEnd
  ) . unTarget

targetToText :: Target -> T.Text
targetToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @QuerySelector querySelectorToText
      . Shrubbery.branch @TargetSelector targetSelectorToText
      . Shrubbery.branch @TargetType targetTypeToText
      . Shrubbery.branch @This thisToText
      . Shrubbery.branch @RawSelector rawSelectorToText
      $ Shrubbery.branchEnd
  ) . unTarget

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

closest :: ( KnownNat branchIndex
           , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
           )
        => querySelector -> TargetSelector
closest =
  TargetSelector TargetSelector_Closest . mkQuerySelector

find :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
        )
     => querySelector -> TargetSelector
find =
  TargetSelector TargetSelector_Find . mkQuerySelector

next :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
        )
     => querySelector -> TargetSelector
next =
  TargetSelector TargetSelector_Next . mkQuerySelector

previous :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
            )
         => querySelector -> TargetSelector
previous =
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
  LBS.fromStrict (TE.encodeUtf8 trigger) <> " from:body"

customTriggerToText :: CustomTrigger -> T.Text
customTriggerToText (CustomTrigger trigger) =
  trigger <> " from:body"

-- RawTrigger
--
newtype RawTrigger =
  RawTrigger
    { rawTriggerToText :: T.Text
    }

rawTriggerToBytes :: RawTrigger -> LBS.ByteString
rawTriggerToBytes =
  LBS.fromStrict . TE.encodeUtf8 . rawTriggerToText

-- Helpers
--
enumBoolToText :: Bool -> T.Text
enumBoolToText = B.bool "false" "true"

lbsUnwords :: [LBS.ByteString] -> LBS.ByteString
lbsUnwords = LBS.intercalate (LBS8.pack " ")

showText :: Show.Show s => s -> T.Text
showText = T.pack . Show.show
