{-# LANGUAGE DataKinds #-}

module Data.HTML4.Elements
  ( HTML
  , a
  , div
  , span
  , p
  , h1
  , ul
  , li
  , img
  , iframe
  ) where

import Prelude hiding (div, span)
import Data.HTML4.Elements.Tags qualified as Tags
import Data.HTML4.Elements.TagType qualified as TagType
import Data.HTML4.Internal (Attribute, Category(..), HTML, Node(..))

a :: [Attribute 'TagType.Anchor] -> [Node 'Flow] -> HTML Tags.Anchor
a = A

div :: [Attribute 'TagType.Division] -> [Node 'Flow] -> HTML Tags.Division
div = Div

span :: [Attribute 'TagType.Span] -> [Node 'Phrasing] -> HTML Tags.Span
span = Span

p :: [Attribute 'TagType.Paragraph] -> [Node 'Phrasing] -> HTML Tags.Paragraph
p = P

h1 :: [Attribute 'TagType.H1] -> [Node 'Phrasing] -> HTML Tags.H1
h1 = H1

ul :: [Attribute 'TagType.UnorderedList]
   -> [Node 'ListItem]
   -> HTML Tags.UnorderedList
ul = Ul

li :: [Attribute 'TagType.ListItem] -> [Node 'Flow] -> HTML Tags.ListItem
li = Li

img :: [Attribute 'TagType.Image] -> HTML Tags.Image
img = Img

iframe :: [Attribute 'TagType.IFrame] -> HTML Tags.IFrame
iframe = Iframe
