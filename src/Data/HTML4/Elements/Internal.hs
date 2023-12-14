{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML4.Elements.Internal
  ( HTML
  , Document
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
import Data.HTML4.Elements.Children (ValidChild)
import Data.HTML4.Elements.TagType qualified as TagType -- import explicit with open constructors after ChildHTML constructor name change

type HTML tag parent =
  ValidChild tag parent => ChildHTML parent

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
