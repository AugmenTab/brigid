{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML4.Elements.Children
  ( ValidChild
  ) where

import Data.HTML4.Elements.TagType (TagType(..))
import Data.HTML4.Types.Contains (Contains)

type ValidChild tag parent =
  Contains (ValidChildrenFor parent) tag

type family ValidChildrenFor (parent :: TagType) :: [TagType]

type instance ValidChildrenFor 'Anchor        = FlowWithoutAnchor
type instance ValidChildrenFor 'Division      = Flow
type instance ValidChildrenFor 'Span          = [ 'Anchor, 'Span, 'Image, 'IFrame ]
type instance ValidChildrenFor 'Paragraph     = [ 'Anchor, 'Span, 'Image, 'IFrame ]
type instance ValidChildrenFor 'H1            = [ 'Anchor, 'Span, 'Image, 'IFrame ]
type instance ValidChildrenFor 'UnorderedList = '[ ListItem ]
type instance ValidChildrenFor 'ListItem      = [ 'Division, 'Paragraph, 'UnorderedList ]
type instance ValidChildrenFor 'Image         = '[]
type instance ValidChildrenFor 'IFrame        = '[]
type instance ValidChildrenFor 'Html          = '[ Division ]
type instance ValidChildrenFor 'Document      = '[ Html ]

type FlowWithoutAnchor = [ 'Division, 'Image, 'Paragraph, 'UnorderedList ]

type Flow = 'Anchor ': FlowWithoutAnchor
