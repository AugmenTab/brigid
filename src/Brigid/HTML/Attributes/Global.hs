module Brigid.HTML.Attributes.Global
  ( AttributeTags.AccessKey, accesskey
  , AttributeTags.Autocapitalize, autocapitalize
  , AttributeTags.Autofocus, autofocus
  , AttributeTags.Class, class_
  , classes
  , AttributeTags.ContentEditable, contenteditable
  , AttributeTags.CustomData, customData
  , AttributeTags.Dir, dir
  , AttributeTags.Draggable, draggable
  , AttributeTags.EnterKeyHint, enterkeyhint
  , AttributeTags.ExportParts, exportparts
  , AttributeTags.Hidden, hide
  , hidden
  , AttributeTags.Id, id
  , AttributeTags.Inert, inert
  , AttributeTags.InputMode, inputmode
  , AttributeTags.Is, is
  , AttributeTags.ItemId, itemid
  , AttributeTags.ItemProp, itemprop
  , AttributeTags.ItemRef, itemref
  , AttributeTags.ItemScope, itemscope
  , AttributeTags.ItemType, itemtype
  , AttributeTags.Lang, lang
  -- , AttributeTags.Nonce, nonce
  , AttributeTags.Part, part
  , AttributeTags.Popover, popover
  , AttributeTags.Role, role
  , AttributeTags.Slot, slot
  , AttributeTags.Spellcheck, spellcheck
  , AttributeTags.Style, style
  , styles
  , AttributeTags.TabIndex, tabindex
  , unsafeTabIndex
  , AttributeTags.Title, title
  , AttributeTags.Translate, translate
  , AttributeTags.WritingSuggestions, writingsuggestions
  ) where

-- Global Attributes
--

import Prelude hiding (id)
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Ogma qualified

import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Attributes.Tags qualified as AttributeTags
import Brigid.HTML.Types qualified as Types
import Brigid.Internal.Render qualified as Render
import Brigid.Types qualified as Types

accesskey :: Char -> Attribute tag
accesskey = Attr_AccessKey

autocapitalize :: Types.AutocapitalizeOption -> Attribute tag
autocapitalize = Attr_Autocapitalize

autofocus :: Bool -> Attribute tag
autofocus = Attr_Autofocus

class_ :: Types.Class -> Attribute tag
class_ = Attr_Class

classes :: [T.Text] -> Attribute tag
classes = class_ . Types.Class . T.unwords . nubOrd

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

inputmode :: Types.InputMode -> Attribute tag
inputmode = Attr_InputMode

is :: T.Text -> Attribute tag
is = Attr_Is

itemid :: T.Text -> Attribute tag
itemid = Attr_ItemId

itemprop :: T.Text -> Attribute tag
itemprop = Attr_ItemProp

itemref :: NEL.NonEmpty Types.Id -> Attribute tag
itemref = Attr_ItemRef

itemscope :: Attribute tag
itemscope = Attr_ItemScope

itemtype :: Types.AbsoluteURL -> Attribute tag
itemtype = Attr_ItemType

-- | Use `Nothing` to represent an unknown language.
--
lang :: Maybe Ogma.BCP_47 -> Attribute tag
lang = Attr_Lang

-- nonce

part :: NEL.NonEmpty Types.Part -> Attribute tag
part = Attr_Part

popover :: Types.PopoverState -> Attribute tag
popover = Attr_Popover

role :: Types.Role -> Attribute tag
role = Attr_Role

slot :: Types.Name -> Attribute tag
slot = Attr_Slot

spellcheck :: Bool -> Attribute tag
spellcheck = Attr_Spellcheck

style :: T.Text -> Attribute tag
style = Attr_Style

styles :: [T.Text] -> Attribute tag
styles = style . Render.foldToTextWithSeparator (\x -> x) ";"

tabindex :: Types.Reachability -> Attribute tag
tabindex = unsafeTabIndex . Types.reachabilityToInteger

-- This is "unsafe" because the HTML documentation recommends against using any
-- values other than 0 or -1 because it can be confusing for users with screen
-- readers or who use keyboard navigation.
--
unsafeTabIndex :: Integer -> Attribute tag
unsafeTabIndex = Attr_TabIndex

title :: T.Text -> Attribute tag
title = Attr_Title

translate :: Bool -> Attribute tag
translate = Attr_Translate

-- | At the time of writing, this attribute is not available on Firefox - see:
-- https://bugzilla.mozilla.org/show_bug.cgi?id=1871621
--
writingsuggestions :: Bool -> Attribute tag
writingsuggestions = Attr_WritingSuggestions
