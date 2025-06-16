module Main
  ( main
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Hedgehog qualified as HH
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as TastyHH

import Brigid.HTML.Generation qualified as Gen
import Brigid.HTML.Generation.Elements (ElementType (..))

main :: IO ()
main = do
  Tasty.defaultMain $
    Tasty.testGroup "Brigid tests"
      [ Tasty.testGroup "Spec compliance in DOM generation"
          . fmap mkElementTestCase
          $ Set.toList _workingElements
       -- . Set.toList
       -- $ Set.difference allElements brokenElements
      ]

_workingElements :: Set ElementType
_workingElements =
  Set.fromList
    [ Comment
    , Anchor
    -- , Abbreviation
    -- , ContactAddress
    , Area
    -- , Article
    , Aside
    , Audio
    , BringAttentionTo
    , Base
    , BidirectionalIsolation
    , BidirectionalOverride
    , Blockquote
    , Body
    , LineBreak
    , Button
    , Canvas
    , TableCaption
    , Citation
    , Code
    , TableColumn
    , TableColumnGroup
    , Data
    , DataList
    , DescriptionDetails
    , DeletedText
    , Details
    , Definition
    , Dialog
    , Division
    , DescriptionList
    , DescriptionTerm
    , Emphasis
    , Embed
    , Fieldset
    , FigureCaption
    , Figure
    , Footer
    , Form
    , H1
    , H2
    , H3
    , H4
    , H5
    , H6
    , Head
    , Header
    , HeadingGroup
    , HorizontalRule
    , Html
    , IdiomaticText
    , IFrame
    , Image
    , Input
    , InsertedText
    , KeyboardInput
    , Label
    , Legend
    , ListItem
    , Link
    , Main
    , Map
    , Mark
    , Menu
    , Meta
    , Meter
    , Nav
    , NoScript
    , Object
    , OrderedList
    , OptionGroup
    , Option
    , Output
    , Paragraph
    , Picture
    , PreformattedText
    , Progress
    , Quotation
    , RubyParenthesis
    , RubyText
    , Ruby
    , Strikethrough
    , Sample
    , Script
    , Search
    , Section
    , Select
    , Slot
    , SideComment
    , Source
    , Span
    , Strong
    , Style
    , Subscript
    , Summary
    , Superscript
    , Table
    , TableBody
    , TableDataCell
    , ContentTemplate
    , TextArea
    , TableFoot
    , TableHeader
    , TableHead
    , Time
    , Title
    , TableRow
    , Track
    , Underline
    , UnorderedList
    , Variable
    , Video
    , WordBreakOpportunity
    ]

_brokenElements :: Set ElementType
_brokenElements =
  Set.fromList
    [
    ]

mkElementTestCase :: ElementType -> Tasty.TestTree
mkElementTestCase e =
  TastyHH.testProperty (show e) $
    HH.property $ do
      dom <- HH.forAll . Gen.generateDOM $ testParams e

      case Gen.toBrigid dom of
        Left err -> fail $ unlines err
        Right _brigid -> pure ()

testParams :: ElementType -> Gen.GeneratorParams
testParams e =
  Gen.GeneratorParams
    { Gen.startingElement = e
    , Gen.maximumTotalNodes = 25
    , Gen.maximumDepth = 2
    , Gen.childrenPerNode = Gen.mkRange 1 20
    , Gen.attributesPerNode = Gen.mkRange 1 10
    }
