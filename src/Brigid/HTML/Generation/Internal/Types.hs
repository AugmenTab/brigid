module Brigid.HTML.Generation.Internal.Types
  ( Element (..)
  , ElementType (..)
  , ElementNode (..)
  , NodeType (..)
  , GeneratorParams (..)
  , Range
  , mkRange
  , minRange
  , maxRange
  ) where

import Data.Maybe (catMaybes)
import Data.NonEmptyText qualified as NET

import Brigid.HTML.Generation.Internal.Attributes qualified as A

data Element =
  Element
    { elementType :: ElementType
    , elementAttrs :: [A.Attribute]
    , elementChildren :: ElementNode
    }

instance Show Element where
  show e =
    unwords
      . catMaybes
      $ [ Just . show $ elementType e
        , Just . show $ elementAttrs e
        , case elementChildren e of
            Branch nodes -> Just $ show nodes
            Leaf net -> Just $ show net
            Void -> Nothing
        ]

-- This is effectively just `Brigid.HTML.Elements.TagType`, but we don't want
-- to expose that ADT, and this has fewer constructors because we don't
-- particularly care to generate a few constructors of that type (like
-- CustomHTML).
--
data ElementType
  = Comment
  | Anchor
  | Abbreviation
  | ContactAddress
  | Area
  | Article
  | Aside
  | Audio
  | BringAttentionTo
  | Base
  | BidirectionalIsolation
  | BidirectionalOverride
  | Blockquote
  | Body
  | LineBreak
  | Button
  | Canvas
  | TableCaption
  | Citation
  | Code
  | TableColumn
  | TableColumnGroup
  | Data
  | DataList
  | DescriptionDetails
  | DeletedText
  | Details
  | Definition
  | Dialog
  | Division
  | DescriptionList
  | DescriptionTerm
  | Emphasis
  | Embed
  | Fieldset
  | FigureCaption
  | Figure
  | Footer
  | Form
  | H1
  | H2
  | H3
  | H4
  | H5
  | H6
  | Head
  | Header
  | HeadingGroup
  | HorizontalRule
  | Html
  | IdiomaticText
  | IFrame
  | Image
  | Input
  | InsertedText
  | KeyboardInput
  | Label
  | Legend
  | ListItem
  | Link
  | Main
  | Map
  | Mark
  | Menu
  | Meta
  | Meter
  | Nav
  | NoScriptBody
  | NoScriptHead
  | Object
  | OrderedList
  | OptionGroup
  | Option
  | Output
  | Paragraph
  | Picture
  | PreformattedText
  | Progress
  | Quotation
  | RubyParenthesis
  | RubyText
  | Ruby
  | Strikethrough
  | Sample
  | Script
  | Search
  | Section
  | Select
  | Slot
  | SideComment
  | Source
  | Span
  | Strong
  | Style
  | Subscript
  | Summary
  | Superscript
  | Table
  | TableBody
  | TableDataCell
  | ContentTemplate
  | TextArea
  | TableFoot
  | TableHeader
  | TableHead
  | Time
  | Title
  | TableRow
  | Track
  | Underline
  | UnorderedList
  | Variable
  | Video
  | WordBreakOpportunity
  deriving (Bounded, Enum, Eq, Ord, Show)

data ElementNode
  = Branch [Element]
  | Leaf NET.NonEmptyText
  | Void
  deriving Show

data NodeType
  = BranchNode
  | LeafNode
  | VoidNode
  deriving (Bounded, Enum, Eq, Ord, Show)

data GeneratorParams =
  GeneratorParams
    { startingElement :: ElementType
    , maximumTotalNodes :: Int
    , maximumDepth :: Int
    , childrenPerNode :: Range
    , attributesPerNode :: Range
    } deriving Show

data Range =
  Range
    { minRange :: Int
    , maxRange :: Int
    } deriving Show

mkRange :: Int -> Int -> Range
mkRange n1 n2 =
  Range
    { minRange = max 0 $ min n1 n2
    , maxRange = max 0 $ max n1 n2
    }
