{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- This is to prevent warnings for the non-matching case in the third `Elem`
-- instance. GHC claims that this is a redundant constraint, but attributes
-- will fail to compile without it. I believe this is related to a known GHC
-- bug.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Data.HTML4.Internal
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
  , Attribute
      ( Id
      , Class
      , Width
      , Disabled
      )
  , AttributeType(..)
  , Category(..)
  , Contains
  , ValidElementsFor
  ) where

import Data.Text qualified as T

import Data.HTML4.Elements.TagType qualified as TagType

data Category = Flow | Phrasing | Heading | ListItem -- etc.
data AttributeType = IdType | ClassType | WidthType | DisabledType -- etc.

class Contains (list :: [TagType.TagType]) (eType :: TagType.TagType)
instance Contains (eType ': es) eType
instance {-# OVERLAPPABLE #-} Contains es eType => Contains (e ': es) eType

type IsValidAttribute attributeType tag =
  Contains (ValidElementsFor attributeType) tag

data Attribute (tag :: TagType.TagType) where
  Id       ::                                       T.Text -> Attribute tag
  Class    ::                                       T.Text -> Attribute tag
  Width    :: IsValidAttribute 'WidthType    tag => Int    -> Attribute tag
  Disabled :: IsValidAttribute 'DisabledType tag => Bool   -> Attribute tag

type family ValidElementsFor (attribute :: AttributeType) :: [TagType.TagType] where
  ValidElementsFor IdType    = [ 'TagType.Division, 'TagType.Span ]
  ValidElementsFor ClassType = '[ TagType.Division ]
  ValidElementsFor WidthType = '[]

-- Type family for mapping elements to categories
type family ElementCategory (eType :: TagType.TagType) :: Category

type instance ElementCategory 'TagType.Anchor = 'Phrasing
type instance ElementCategory 'TagType.Division = 'Flow
type instance ElementCategory 'TagType.Span = 'Phrasing
type instance ElementCategory 'TagType.Paragraph = 'Flow
type instance ElementCategory 'TagType.H1 = 'Heading
type instance ElementCategory 'TagType.UnorderedList = 'Flow
type instance ElementCategory 'TagType.ListItem = 'ListItem
type instance ElementCategory 'TagType.Image = 'Phrasing
type instance ElementCategory 'TagType.IFrame = 'Phrasing
-- Add mappings for other elements

type HTML eType =
  Node (ElementCategory eType)

{-
-- For declaring lists of content to a particular Node.
type ChildHTML eType =
  Node (ChildCategory eType)
-}

-- element :: [Attribute for Node] -> [child category Node]-> parent category Node
data Node (cat :: Category) where
  A      :: [Attribute 'TagType.Anchor]        -> [Node 'Flow]     -> Node 'Phrasing
  Div    :: [Attribute 'TagType.Division]      -> [Node 'Flow]     -> Node 'Flow
  Span   :: [Attribute 'TagType.Span]          -> [Node 'Phrasing] -> Node 'Phrasing
  P      :: [Attribute 'TagType.Paragraph]     -> [Node 'Phrasing] -> Node 'Flow
  H1     :: [Attribute 'TagType.H1]            -> [Node 'Phrasing] -> Node 'Heading
  Ul     :: [Attribute 'TagType.UnorderedList] -> [Node 'ListItem] -> Node 'Flow
  Li     :: [Attribute 'TagType.ListItem]      -> [Node 'Flow]     -> Node 'ListItem
  Img    :: [Attribute 'TagType.Image]                             -> Node 'Phrasing
  Iframe :: [Attribute 'TagType.IFrame]                            -> Node 'Phrasing
    -- etc.
