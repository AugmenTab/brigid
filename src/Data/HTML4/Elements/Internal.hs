{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML4.Elements.Internal
  ( HTML
  , Node
      ( A
      , Div
      , Span
      , P
      , H1
      , Ul
      , Li
      , Img
      , Iframe
      )
  ) where

import Data.HTML4.Attributes.Internal (Attribute)
import Data.HTML4.Elements.Category qualified as Cat
import Data.HTML4.Elements.TagType qualified as TagType

type HTML eType =
  Node (ElementCategory eType)

{-
-- For declaring lists of content to a particular Node.
type ChildHTML eType =
  Node (ChildCategory eType)
-}

-- element :: [Attribute for Node] -> [child category Node]-> parent category Node
data Node (cat :: Cat.Category) where
  A      :: [Attribute 'TagType.Anchor]        -> [Node 'Cat.Flow]     -> Node 'Cat.Phrasing
  Div    :: [Attribute 'TagType.Division]      -> [Node 'Cat.Flow]     -> Node 'Cat.Flow
  Span   :: [Attribute 'TagType.Span]          -> [Node 'Cat.Phrasing] -> Node 'Cat.Phrasing
  P      :: [Attribute 'TagType.Paragraph]     -> [Node 'Cat.Phrasing] -> Node 'Cat.Flow
  H1     :: [Attribute 'TagType.H1]            -> [Node 'Cat.Phrasing] -> Node 'Cat.Heading
  Ul     :: [Attribute 'TagType.UnorderedList] -> [Node 'Cat.ListItem] -> Node 'Cat.Flow
  Li     :: [Attribute 'TagType.ListItem]      -> [Node 'Cat.Flow]     -> Node 'Cat.ListItem
  Img    :: [Attribute 'TagType.Image]                                 -> Node 'Cat.Phrasing
  Iframe :: [Attribute 'TagType.IFrame]                                -> Node 'Cat.Phrasing

type family ElementCategory (eType :: TagType.TagType) :: Cat.Category

type instance ElementCategory 'TagType.Anchor        = 'Cat.Phrasing
type instance ElementCategory 'TagType.Division      = 'Cat.Flow
type instance ElementCategory 'TagType.Span          = 'Cat.Phrasing
type instance ElementCategory 'TagType.Paragraph     = 'Cat.Flow
type instance ElementCategory 'TagType.H1            = 'Cat.Heading
type instance ElementCategory 'TagType.UnorderedList = 'Cat.Flow
type instance ElementCategory 'TagType.ListItem      = 'Cat.ListItem
type instance ElementCategory 'TagType.Image         = 'Cat.Phrasing
type instance ElementCategory 'TagType.IFrame        = 'Cat.Phrasing
-- TODO: Add remaining HTML elements.
