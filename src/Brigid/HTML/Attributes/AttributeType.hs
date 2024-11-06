{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HTML.Attributes.AttributeType
  ( AttributeErrorMessage
  , AttributeType
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

      , Accept
      , AcceptCharset
      , Action
      , Allow
      , Alt
      , Async
      , Autocomplete
      , Autoplay
      , Background
      , BackgroundColor
      , Border
      , Capture
      , Charset
      , Checked
      , Cite
      , Color
      , Cols
      , Colspan
      , Content
      , Controls
      , Coords
      , CrossOrigin
      , Data
      , Datetime
      , Decoding
      , Default
      , Defer
      , Dirname
      , Disabled
      , Download
      , Enctype
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
      , Integrity
      , IsMap
      , Kind
      , Label
      , List
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

      , HxValidate
      )
  ) where

import GHC.TypeLits (ErrorMessage(..))

data AttributeType
  -- Global Attributes
  --
  = AccessKey
  | Autocapitalize
  | Autofocus
  | Class
  | ContentEditable
  | CustomData
  | Dir
  | Draggable
  | EnterKeyHint
  | ExportParts
  | Hidden
  | Id
  | Inert
  | InputMode
  | Is
  | ItemId
  | ItemProp
  | ItemRef
  | ItemScope
  | ItemType
  | Lang
  | Nonce
  | Part
  | Popover
  | Role
  | Slot
  | Spellcheck
  | Style
  | TabIndex
  | Title
  | Translate

  -- Scoped Attributes
  --
  | Accept
  | AcceptCharset
  | Action
  | Allow
  | Alt
  | Async
  | Autocomplete
  | Autoplay
  | Background
  | BackgroundColor
  | Border
  | Capture
  | Charset
  | Checked
  | Cite
  | Color
  | Cols
  | Colspan
  | Content
  | Controls
  | Coords
  | CrossOrigin
  | Data
  | Datetime
  | Decoding
  | Default
  | Defer
  | Dirname
  | Disabled
  | Download
  | Enctype
  | For
  | Form
  | FormAction
  | FormEnctype
  | FormMethod
  | FormNoValidate
  | FormTarget
  | Headers
  | Height
  | High
  | Href
  | HrefLang
  | HttpEquiv
  | Integrity
  | IsMap
  | Kind
  | Label
  | List
  | Loop
  | Low
  | Max
  | MaxLength
  | MinLength
  | Media
  | Method
  | Min
  | Multiple
  | Muted
  | Name
  | NoModule
  | NoValidate
  | Open
  | Optimum
  | Pattern
  | Ping
  | Placeholder
  | PlaysInline
  | Poster
  | Preload
  | ReadOnly
  | ReferrerPolicy
  | Rel
  | Required
  | Reversed
  | Rows
  | Rowspan
  | Sandbox
  | Scope
  | Selected
  | Shape
  | Size
  | Sizes
  | Span
  | Src
  | SrcDoc
  | SrcLang
  | SrcSet
  | Start
  | Step
  | Target
  | Type
  | UseMap
  | Value
  | Width
  | Wrap

  -- HTMX Attributes
  --
  | HxValidate

type family AttributeErrorMessage (attr :: AttributeType) :: ErrorMessage where
  AttributeErrorMessage AccessKey       = 'Text "AccessKey (accesskey)"
  AttributeErrorMessage Autocapitalize  = 'Text "Autocapitalize (autocapitalize)"
  AttributeErrorMessage Autofocus       = 'Text "Autofocus (autofocus)"
  AttributeErrorMessage Class           = 'Text "Class (class)"
  AttributeErrorMessage ContentEditable = 'Text "ContentEditable (contenteditable)"
  AttributeErrorMessage CustomData      = 'Text "CustomData (data-*)"
  AttributeErrorMessage Dir             = 'Text "Dir (dir)"
  AttributeErrorMessage Draggable       = 'Text "Draggable (draggable)"
  AttributeErrorMessage EnterKeyHint    = 'Text "EnterKeyHint (enterkeyhint)"
  AttributeErrorMessage ExportParts     = 'Text "ExportParts (exportparts)"
  AttributeErrorMessage Hidden          = 'Text "Hidden (hidden)"
  AttributeErrorMessage Id              = 'Text "Id (id)"
  AttributeErrorMessage Inert           = 'Text "Inert (inert)"
  AttributeErrorMessage InputMode       = 'Text "InputMode (inputmode)"
  AttributeErrorMessage Is              = 'Text "Is (is)"
  AttributeErrorMessage ItemId          = 'Text "ItemId (itemid)"
  AttributeErrorMessage ItemProp        = 'Text "ItemProp (itemprop)"
  AttributeErrorMessage ItemRef         = 'Text "ItemRef (itemref)"
  AttributeErrorMessage ItemScope       = 'Text "ItemScope (itemscope)"
  AttributeErrorMessage ItemType        = 'Text "ItemType (itemtype)"
  AttributeErrorMessage Lang            = 'Text "Lang (lang)"
  AttributeErrorMessage Nonce           = 'Text "Nonce (nonce)"
  AttributeErrorMessage Part            = 'Text "Part (part)"
  AttributeErrorMessage Popover         = 'Text "Popover (popover)"
  AttributeErrorMessage Role            = 'Text "Role (role)"
  AttributeErrorMessage Slot            = 'Text "Slot (slot)"
  AttributeErrorMessage Spellcheck      = 'Text "Spellcheck (spellcheck)"
  AttributeErrorMessage Style           = 'Text "Style (style)"
  AttributeErrorMessage TabIndex        = 'Text "TabIndex (tabindex)"
  AttributeErrorMessage Title           = 'Text "Title (title)"
  AttributeErrorMessage Translate       = 'Text "Translate (translate)"

  AttributeErrorMessage Accept          = 'Text "Accept (accept)"
  AttributeErrorMessage AcceptCharset   = 'Text "AcceptCharset (accept-charset)"
  AttributeErrorMessage Action          = 'Text "Action (action)"
  AttributeErrorMessage Allow           = 'Text "Allow (allow)"
  AttributeErrorMessage Alt             = 'Text "Alt (alt)"
  AttributeErrorMessage Async           = 'Text "Async (async)"
  AttributeErrorMessage Autocomplete    = 'Text "Autocomplete (autocomplete)"
  AttributeErrorMessage Autoplay        = 'Text "Autoplay (autoplay)"
  AttributeErrorMessage Background      = 'Text "Background (background)"
  AttributeErrorMessage BackgroundColor = 'Text "BackgroundColor (bgcolor)"
  AttributeErrorMessage Border          = 'Text "Border (border)"
  AttributeErrorMessage Capture         = 'Text "Capture (capture)"
  AttributeErrorMessage Charset         = 'Text "Charset (charset)"
  AttributeErrorMessage Checked         = 'Text "Checked (checked)"
  AttributeErrorMessage Cite            = 'Text "Cite (cite)"
  AttributeErrorMessage Color           = 'Text "Color (color)"
  AttributeErrorMessage Cols            = 'Text "Cols (cols)"
  AttributeErrorMessage Colspan         = 'Text "Colspan (colspan)"
  AttributeErrorMessage Content         = 'Text "Content (content)"
  AttributeErrorMessage Controls        = 'Text "Controls (controls)"
  AttributeErrorMessage Coords          = 'Text "Coords (coords)"
  AttributeErrorMessage CrossOrigin     = 'Text "CrossOrigin (crossorigin)"
  AttributeErrorMessage Data            = 'Text "Data (data)"
  AttributeErrorMessage Datetime        = 'Text "Datetime (datetime)"
  AttributeErrorMessage Decoding        = 'Text "Decoding (decoding)"
  AttributeErrorMessage Default         = 'Text "Default (default)"
  AttributeErrorMessage Defer           = 'Text "Defer (defer)"
  AttributeErrorMessage Dirname         = 'Text "Dirname (dir)"
  AttributeErrorMessage Disabled        = 'Text "Disabled (disabled)"
  AttributeErrorMessage Download        = 'Text "Download (download)"
  AttributeErrorMessage Enctype         = 'Text "Enctype (enctype)"
  AttributeErrorMessage For             = 'Text "For (for)"
  AttributeErrorMessage Form            = 'Text "Form (form)"
  AttributeErrorMessage FormAction      = 'Text "FormAction (formaction)"
  AttributeErrorMessage FormEnctype     = 'Text "FormEnctype (formenctype)"
  AttributeErrorMessage FormMethod      = 'Text "FormMethod (formmethod)"
  AttributeErrorMessage FormNoValidate  = 'Text "FormNoValidate (formnovalidate)"
  AttributeErrorMessage FormTarget      = 'Text "FormTarget (formtarget)"
  AttributeErrorMessage Headers         = 'Text "Headers (headers)"
  AttributeErrorMessage Height          = 'Text "Height (height)"
  AttributeErrorMessage High            = 'Text "High (high)"
  AttributeErrorMessage Href            = 'Text "Href (href)"
  AttributeErrorMessage HrefLang        = 'Text "HrefLang (hreflang)"
  AttributeErrorMessage HttpEquiv       = 'Text "HttpEquiv (http-equiv)"
  AttributeErrorMessage Integrity       = 'Text "Integrity (integrity)"
  AttributeErrorMessage IsMap           = 'Text "IsMap (ismap)"
  AttributeErrorMessage Kind            = 'Text "Kind (kind)"
  AttributeErrorMessage Label           = 'Text "Label (label)"
  AttributeErrorMessage List            = 'Text "List (list)"
  AttributeErrorMessage Loop            = 'Text "Loop (loop)"
  AttributeErrorMessage Low             = 'Text "Low (low)"
  AttributeErrorMessage Max             = 'Text "Max (max)"
  AttributeErrorMessage MaxLength       = 'Text "MaxLength (maxlength)"
  AttributeErrorMessage MinLength       = 'Text "MinLength (minlength)"
  AttributeErrorMessage Media           = 'Text "Media (media)"
  AttributeErrorMessage Method          = 'Text "Method (method)"
  AttributeErrorMessage Min             = 'Text "Min (min)"
  AttributeErrorMessage Multiple        = 'Text "Multiple (multiple)"
  AttributeErrorMessage Muted           = 'Text "Muted (muted)"
  AttributeErrorMessage Name            = 'Text "Name (name)"
  AttributeErrorMessage NoModule        = 'Text "NoModule (nomodule)"
  AttributeErrorMessage NoValidate      = 'Text "NoValidate (novalidate)"
  AttributeErrorMessage Open            = 'Text "Open (open)"
  AttributeErrorMessage Optimum         = 'Text "Optimum (optimum)"
  AttributeErrorMessage Pattern         = 'Text "Pattern (pattern)"
  AttributeErrorMessage Ping            = 'Text "Ping (ping)"
  AttributeErrorMessage Placeholder     = 'Text "Placeholder (placeholder)"
  AttributeErrorMessage PlaysInline     = 'Text "PlaysInline (playsinline)"
  AttributeErrorMessage Poster          = 'Text "Poster (poster)"
  AttributeErrorMessage Preload         = 'Text "Preload (preload)"
  AttributeErrorMessage ReadOnly        = 'Text "ReadOnly (readonly)"
  AttributeErrorMessage ReferrerPolicy  = 'Text "ReferrerPolicy (referrerpolicy)"
  AttributeErrorMessage Rel             = 'Text "Rel (rel)"
  AttributeErrorMessage Required        = 'Text "Required (required)"
  AttributeErrorMessage Reversed        = 'Text "Reversed (reversed)"
  AttributeErrorMessage Rows            = 'Text "Rows (rows)"
  AttributeErrorMessage Rowspan         = 'Text "Rowspan (rowspan)"
  AttributeErrorMessage Sandbox         = 'Text "Sandbox (sandbox)"
  AttributeErrorMessage Scope           = 'Text "Scope (scope)"
  AttributeErrorMessage Selected        = 'Text "Selected (selected)"
  AttributeErrorMessage Shape           = 'Text "Shape (shape)"
  AttributeErrorMessage Size            = 'Text "Size (size)"
  AttributeErrorMessage Sizes           = 'Text "Sizes (sizes)"
  AttributeErrorMessage Span            = 'Text "Span (span)"
  AttributeErrorMessage Src             = 'Text "Src (src)"
  AttributeErrorMessage SrcDoc          = 'Text "SrcDoc (srcdoc)"
  AttributeErrorMessage SrcLang         = 'Text "SrcLang (srclang)"
  AttributeErrorMessage SrcSet          = 'Text "SrcSet (srcset)"
  AttributeErrorMessage Start           = 'Text "Start (start)"
  AttributeErrorMessage Step            = 'Text "Step (step)"
  AttributeErrorMessage Target          = 'Text "Target (target)"
  AttributeErrorMessage Type            = 'Text "Type (type)"
  AttributeErrorMessage UseMap          = 'Text "UseMap (usemap)"
  AttributeErrorMessage Value           = 'Text "Value (value)"
  AttributeErrorMessage Width           = 'Text "Width (width)"
  AttributeErrorMessage Wrap            = 'Text "Wrap (wrap)"

  AttributeErrorMessage HxValidate      = 'Text "HTMX Validate (hx-validate)"
