{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Attributes
  ( customAttribute
  , accesskey
  , autocapitalize
  , autofocus
  , class_
  , classes
  , contenteditable
  , customData
  , dir
  , draggable
  , enterkeyhint
  , exportparts
  , hide
  , hidden
  , id
  , inert
  -- , inputmode
  , is
  -- , item
  -- , itemid
  -- , itemprop
  -- , itemscope
  -- , itemtype
  -- , lang
  -- , nonce
  , part
  , popover
  -- , role
  -- , slot
  , spellcheck
  , style
  , styles
  , tabindex
  , unsafeTabIndex
  , title
  , translate

  , crossorigin
  , disable
  , disabled
  , href
  ) where

import Prelude hiding (id)
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery.TypeList (FirstIndexOf)

import HTML.Attributes.AttributeType (AttributeType(..))
import HTML.Attributes.Elements (ValidAttribute)
import HTML.Attributes.Href (ValidHref)
import HTML.Attributes.Internal (Attribute(..))
import HTML.Types qualified as Types


customAttribute :: T.Text -> T.Text -> Attribute tag
customAttribute = Attr_Custom

-- Global Attributes
--

accesskey :: Char -> Attribute tag
accesskey = Attr_AccessKey

autocapitalize :: Types.AutocapitalizeOption -> Attribute tag
autocapitalize = Attr_Autocapitalize

autofocus :: Bool -> Attribute tag
autofocus = Attr_Autofocus

class_ :: T.Text -> Attribute tag
class_ = Attr_Class

classes :: [T.Text] -> Attribute tag
classes = class_ . T.unwords

contenteditable :: Types.ContentEditableOption -> Attribute tag
contenteditable = Attr_ContentEditable

customData :: T.Text -> T.Text -> Attribute tag
customData = Attr_CustomData

dir :: Types.Directionality -> Attribute tag
dir = Attr_Dir

draggable :: Bool -> Attribute tag
draggable = Attr_Draggable

enterkeyhint :: Types.KeyHintOption -> Attribute tag
enterkeyhint = Attr_EnterKeyHint

exportparts :: NEL.NonEmpty Types.ExportPart -> Attribute tag
exportparts = Attr_ExportParts

hide :: Bool -> Attribute tag
hide = Attr_Hidden

hidden :: Attribute tag
hidden = hide True

id :: Types.Id -> Attribute tag
id = Attr_Id

inert :: Bool -> Attribute tag
inert = Attr_Inert

-- inputmode :: Types.InputMode -> Attribute tag
-- inputmode = Attr_InputMode

is :: T.Text -> Attribute tag
is = Attr_Is

{-
-- Helper to ensure proper construction of item attributes
item

itemid

itemprop

itemref

itemscope

itemtype
-}

-- lang

-- nonce

part :: NEL.NonEmpty Types.Part -> Attribute tag
part = Attr_Part

popover :: Types.PopoverState -> Attribute tag
popover = Attr_Popover

-- role

-- slot

spellcheck :: Bool -> Attribute tag
spellcheck = Attr_Spellcheck

style :: T.Text -> Attribute tag
style = Attr_Style

styles :: [T.Text] -> Attribute tag
styles = style . T.intercalate ";"

tabindex :: Types.Reachability -> Attribute tag
tabindex = unsafeTabIndex . Types.reachabilityToInt

-- This is "unsafe" because the HTML documentation recommends against using any
-- values other than 0 or -1 because it can be confusing for users with screen
-- readers or who use keyboard navigation.
unsafeTabIndex :: Int -> Attribute tag
unsafeTabIndex = Attr_TabIndex

title :: T.Text -> Attribute tag
title = Attr_Title

translate :: Bool -> Attribute tag
translate = Attr_Translate

-- Scoped Attributes
--

{-|
   This enumerated attribute indicates whether CORS must be used when fetching
   the resource. CORS-enabled images can be reused in the @<canvas>@ element
   without being tainted. If the attribute is not present, the resource is
   fetched without a CORS request (i.e. without sending the @Origin@ HTTP
   header), preventing its non-tainted usage. If invalid, it is handled as if
   the enumerated keyword @anonymous@ was used.
-}
crossorigin :: ValidAttribute 'CrossOrigin tag
            => Types.CrossOriginFetch
            -> Attribute tag
crossorigin = Attr_CrossOrigin

disable :: ValidAttribute 'Disabled tag => Bool -> Attribute tag
disable = Attr_Disabled

disabled :: ValidAttribute 'Disabled tag => Attribute tag
disabled = disable True

href :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf a (Types.HrefTypes Types.Get)
        , ValidHref a tag
        , ValidAttribute 'Href tag
        )
     => a
     -> Attribute tag
href =
  Attr_Href . Types.mkHref
