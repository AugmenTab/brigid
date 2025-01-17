module Brigid.HTML.Attributes.Global
  ( accesskey
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
  , lang
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
  , writingsuggestions
  ) where

-- Global Attributes
--

import Prelude hiding (id)
import Data.Containers.ListUtils (nubOrd)
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Ogma qualified

import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Types qualified as Types

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

-- | Use `Nothing` to represent an unknown language.
--
lang :: Maybe Ogma.BCP_47 -> Attribute tag
lang = Attr_Lang

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

-- | At the time of writing, this attribute is not available on Firefox - see:
-- https://bugzilla.mozilla.org/show_bug.cgi?id=1871621
--
writingsuggestions :: Bool -> Attribute tag
writingsuggestions = Attr_WritingSuggestions
