{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML4.Elements.Internal
  ( HTML
  , Document
  , ValidChild
  , ChildHTML
      ( A
      , Div
      , Span
      , P
      , H1
      , Ul
      , Li
      , Img
      , Iframe
      , Html
      )
  ) where

import Data.HTML4.Attributes.Internal (Attribute)
import Data.HTML4.Elements.TagType qualified as TagType
import Data.HTML4.Types.Contains (Contains)

type HTML tag parent =
  ValidChild tag parent => ChildHTML parent

type ValidChild tag parent =
  Contains (ValidChildrenFor parent) tag

type Document =
  ChildHTML 'TagType.Document

data ChildHTML (parent :: TagType.TagType) where
  A      :: ValidChild 'TagType.Anchor        parent => [Attribute 'TagType.Anchor]        -> [ChildHTML 'TagType.Anchor]        -> ChildHTML parent
  Div    :: ValidChild 'TagType.Division      parent => [Attribute 'TagType.Division]      -> [ChildHTML 'TagType.Division]      -> ChildHTML parent
  Span   :: ValidChild 'TagType.Span          parent => [Attribute 'TagType.Span]          -> [ChildHTML 'TagType.Span]          -> ChildHTML parent
  P      :: ValidChild 'TagType.Paragraph     parent => [Attribute 'TagType.Paragraph]     -> [ChildHTML 'TagType.Paragraph]     -> ChildHTML parent
  H1     :: ValidChild 'TagType.H1            parent => [Attribute 'TagType.H1]            -> [ChildHTML 'TagType.H1]            -> ChildHTML parent
  Ul     :: ValidChild 'TagType.UnorderedList parent => [Attribute 'TagType.UnorderedList] -> [ChildHTML 'TagType.UnorderedList] -> ChildHTML parent
  Li     :: ValidChild 'TagType.ListItem      parent => [Attribute 'TagType.ListItem]      -> [ChildHTML 'TagType.ListItem]      -> ChildHTML parent
  Img    :: ValidChild 'TagType.Image         parent => [Attribute 'TagType.Image]                                               -> ChildHTML parent
  Iframe :: ValidChild 'TagType.IFrame        parent => [Attribute 'TagType.IFrame]                                              -> ChildHTML parent
  Html   :: ValidChild 'TagType.Html          parent => [Attribute 'TagType.Html]          -> [ChildHTML 'TagType.Html]          -> ChildHTML parent

type family ValidChildrenFor (parent :: TagType.TagType) :: [TagType.TagType]

type instance ValidChildrenFor 'TagType.Anchor        = FlowWithoutAnchor
type instance ValidChildrenFor 'TagType.Division      = Flow
type instance ValidChildrenFor 'TagType.Span          = [ 'TagType.Anchor, 'TagType.Span, 'TagType.Image, 'TagType.IFrame ]
type instance ValidChildrenFor 'TagType.Paragraph     = [ 'TagType.Anchor, 'TagType.Span, 'TagType.Image, 'TagType.IFrame ]
type instance ValidChildrenFor 'TagType.H1            = [ 'TagType.Anchor, 'TagType.Span, 'TagType.Image, 'TagType.IFrame ]
type instance ValidChildrenFor 'TagType.UnorderedList = '[ TagType.ListItem ]
type instance ValidChildrenFor 'TagType.ListItem      = [ 'TagType.Division, 'TagType.Paragraph, 'TagType.UnorderedList ]
type instance ValidChildrenFor 'TagType.Image         = '[]
type instance ValidChildrenFor 'TagType.IFrame        = '[]
type instance ValidChildrenFor 'TagType.Html          = '[ TagType.Division ]
type instance ValidChildrenFor 'TagType.Document      = '[ TagType.Html ]

type FlowWithoutAnchor = [ 'TagType.Division, 'TagType.Image, 'TagType.Paragraph, 'TagType.UnorderedList ]

type Flow = 'TagType.Anchor ': FlowWithoutAnchor
