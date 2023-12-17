{-# LANGUAGE DataKinds #-}

module Data.HTML4.Attributes
  -- ( accesskey
  ( autocapitalize
  , autofocus
  , class_
  , classes
  , contenteditable
  , data_
  , dir
  , draggable
  , enterkeyhint
  -- , exportparts
  , hide
  , hidden
  , id
  , inert
  , inputmode
  , is
  -- , item
  -- , itemid
  -- , itemprop
  -- , itemscope
  -- , itemtype
  -- , lang
  -- , nonce
  -- , part
  -- , popover
  -- , role
  -- , slot
  , spellcheck
  , style
  , styles
  , tabindex
  , unsafeTabIndex
  , title
  , translate
  , width
  , disable
  , disabled
  ) where

import Prelude hiding (id)
import Data.Text qualified as T

import Data.HTML4.Attributes.AttributeType (AttributeType(..))
import Data.HTML4.Attributes.Elements (ValidAttribute)
import Data.HTML4.Attributes.Internal (Attribute(..))
import Data.HTML4.Types qualified as Types

-- Global Attributes

-- accesskey

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

data_ :: T.Text -> T.Text -> Attribute tag
data_ = Attr_Data

dir :: Types.Directionality -> Attribute tag
dir = Attr_Dir

draggable :: Bool -> Attribute tag
draggable = Attr_Draggable

enterkeyhint :: Types.KeyHintOption -> Attribute tag
enterkeyhint = Attr_EnterKeyHint

-- exportparts

hide :: Bool -> Attribute tag
hide = Attr_Hidden

hidden :: Attribute tag
hidden = hide True

id :: T.Text -> Attribute tag
id = Attr_Id

inert :: Bool -> Attribute tag
inert = Attr_Inert

inputmode :: Types.InputMode -> Attribute tag
inputmode = Attr_InputMode

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

lang

nonce

-- TODO: Can this be determined programmatically?
--
-- https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/part
part

popover

role

slot
-}

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

--

width :: ValidAttribute 'Width tag => Int -> Attribute tag
width = Attr_Width

disable :: ValidAttribute 'Disabled tag => Bool -> Attribute tag
disable = Attr_Disabled

disabled :: ValidAttribute 'Disabled tag => Attribute tag
disabled = disable True
