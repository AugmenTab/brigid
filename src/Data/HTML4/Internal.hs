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
      )
  , Attribute
      ( Id
      , Class
      , Width
      )
  , ElementType(..)
  , Division
  ) where

type Division = 'DivType

data ElementType = AType | DivType | SpanType | PType | H1Type | UlType | LiType | AudioType -- etc.
data Category = Flow | Phrasing | Embedded | Heading | Interactive | ListItem -- etc.
data AttributeType = IdType | ClassType | WidthType -- etc.

class Contains (list :: [ElementType]) (eType :: ElementType)
instance Contains (eType ': es) eType
instance {-# OVERLAPPABLE #-} Contains es eType => Contains (e ': es) eType

data Attribute (tag :: ElementType) where
  Id    ::                                               String -> Attribute tag
  Class ::                                               String -> Attribute tag
  Width :: Contains (ValidElementsFor 'WidthType) tag => Int    -> Attribute tag

type family ValidElementsFor (attribute :: AttributeType) :: [ElementType] where
  ValidElementsFor IdType    = ['DivType, 'SpanType]
  ValidElementsFor ClassType = '[DivType]
  ValidElementsFor WidthType = '[]

type HTML eType =
  Node (ElementCategory eType)

-- element :: [Attribute for Node] -> [child category Node]-> parent category Node
data Node (cat :: Category) where
  A    :: [Attribute 'AType]    -> [Node 'Flow]     -> Node 'Phrasing
  Div  :: [Attribute 'DivType]  -> [Node 'Flow]     -> Node 'Flow
  Span :: [Attribute 'SpanType] -> [Node 'Phrasing] -> Node 'Phrasing
  P    :: [Attribute 'PType]    -> [Node 'Phrasing] -> Node 'Flow
  H1   :: [Attribute 'H1Type]   -> [Node 'Phrasing] -> Node 'Heading
  Ul   :: [Attribute 'UlType]   -> [Node 'ListItem] -> Node 'Flow
  Li   :: [Attribute 'LiType]   -> [Node 'Flow]     -> Node 'ListItem
    -- etc.

-- Type family for mapping elements to categories
type family ElementCategory (eType :: ElementType) :: Category where
  ElementCategory 'DivType = 'Flow
  ElementCategory 'SpanType = 'Phrasing
  ElementCategory 'PType = 'Flow
  ElementCategory 'H1Type = 'Heading
-- Add mappings for other elements
