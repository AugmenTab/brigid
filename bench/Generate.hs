module Generate
  ( html
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NEL
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Prelude hiding (div, head, map, span)

import Generation.Element (Element (..))
import Generation.Generators qualified as Generators

comment :: Gen Element
comment =
  Comment <$> Generators.text

text :: Gen Element
text =
  Text <$> Generators.text

anchor :: Int -> Int -> Gen Element
anchor maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    aChildren = NEL.toList anchorContent

  Anchor
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice aChildren)

anchorContent :: NonEmpty (Gen Element)
anchorContent = NEL.singleton text

abbreviation :: Int -> Int -> Gen Element
abbreviation maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    abbrChildren = NEL.toList $ abbreviationContent maxAttrs maxChildren

  Abbreviation
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice abbrChildren)

abbreviationContent :: Int -> Int -> NonEmpty (Gen Element)
abbreviationContent = phrasingContent

contactAddress :: Int -> Int -> Gen Element
contactAddress maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    contactAddressChildren =
      NEL.toList $ contactAddressContent maxAttrs maxChildren

  ContactAddress
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice contactAddressChildren)

contactAddressContent :: Int -> Int -> NonEmpty (Gen Element)
contactAddressContent maxAttrs maxChildren =
  text
    :| [ comment
       , anchor maxAttrs maxChildren
       , abbreviation maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , deletedText maxAttrs maxChildren
       , details maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , division maxAttrs maxChildren
       , descriptionList maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , form maxAttrs maxChildren
       , horizontalRule maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , insertedText maxAttrs maxChildren
       , keyboardInput maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , orderedList maxAttrs maxChildren
       , output maxAttrs maxChildren
       , paragraph maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , preformattedText maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , table maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , unorderedList maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

area :: Int -> Gen Element
area maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  Area <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

article :: Int -> Int -> Gen Element
article maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    articleChildren = NEL.toList $ articleContent maxAttrs maxChildren

  Article
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice articleChildren)

articleContent :: Int -> Int -> NonEmpty (Gen Element)
articleContent = flowContent

aside :: Int -> Int -> Gen Element
aside maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    asideChildren = NEL.toList $ asideContent maxAttrs maxChildren

  Aside
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice asideChildren)

asideContent :: Int -> Int -> NonEmpty (Gen Element)
asideContent = flowContent

audio :: Int -> Int -> Gen Element
audio maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    audioChildren = NEL.toList $ audioContent maxAttrs

  Audio
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice audioChildren)

audioContent :: Int -> NonEmpty (Gen Element)
audioContent = audioVideoContent

bringAttentionTo :: Int -> Int -> Gen Element
bringAttentionTo maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    bChildren = NEL.toList $ bringAttentionToContent maxAttrs maxChildren

  BringAttentionTo
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice bChildren)

bringAttentionToContent :: Int -> Int -> NonEmpty (Gen Element)
bringAttentionToContent = phrasingContent

base :: Int -> Gen Element
base maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  Base <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

bidirectionalIsolation :: Int -> Int -> Gen Element
bidirectionalIsolation maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    bdiChildren = NEL.toList $ bidirectionalIsolationContent maxAttrs maxChildren

  BidirectionalIsolation
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice bdiChildren)

bidirectionalIsolationContent :: Int -> Int -> NonEmpty (Gen Element)
bidirectionalIsolationContent = phrasingContent

bidirectionalOverride :: Int -> Int -> Gen Element
bidirectionalOverride maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    bdoChildren = NEL.toList $ bidirectionalOverrideContent maxAttrs maxChildren

  BidirectionalOverride
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice bdoChildren)

bidirectionalOverrideContent :: Int -> Int -> NonEmpty (Gen Element)
bidirectionalOverrideContent = phrasingContent

blockquote :: Int -> Int -> Gen Element
blockquote maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    blockquoteChildren = NEL.toList $ blockquoteContent maxAttrs maxChildren

  Blockquote
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice blockquoteChildren)

blockquoteContent :: Int -> Int -> NonEmpty (Gen Element)
blockquoteContent = flowContent

body :: Int -> Int -> Gen Element
body maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    bodyChildren = NEL.toList $ bodyContent maxAttrs maxChildren

  Body
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice bodyChildren)

bodyContent :: Int -> Int -> NonEmpty (Gen Element)
bodyContent = flowContent

lineBreak :: Int -> Gen Element
lineBreak maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  LineBreak <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

button :: Int -> Int -> Gen Element
button maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    buttonChildren = NEL.toList $ buttonContent maxAttrs maxChildren

  Button
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice buttonChildren)

buttonContent :: Int -> Int -> NonEmpty (Gen Element)
buttonContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbreviation maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , lineBreak maxAttrs
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , idiomaticText maxAttrs maxChildren
       , image maxAttrs
       , keyboardInput maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , script maxAttrs
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

canvas :: Int -> Int -> Gen Element
canvas maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    canvasChildren = NEL.toList canvasContent

  Canvas
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice canvasChildren)

canvasContent :: NonEmpty (Gen Element)
canvasContent = NEL.singleton text

tableCaption :: Int -> Int -> Gen Element
tableCaption maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tableCaptionChildren = NEL.toList $ tableCaptionContent maxAttrs maxChildren

  TableCaption
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice tableCaptionChildren)

tableCaptionContent :: Int -> Int -> NonEmpty (Gen Element)
tableCaptionContent = flowContent

citation :: Int -> Int -> Gen Element
citation maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    citeChildren = NEL.toList $ citationContent maxAttrs maxChildren

  Citation
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice citeChildren)

citationContent :: Int -> Int -> NonEmpty (Gen Element)
citationContent = phrasingContent

code :: Int -> Int -> Gen Element
code maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    codeChildren = NEL.toList $ codeContent maxAttrs maxChildren

  Code
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice codeChildren)

codeContent :: Int -> Int -> NonEmpty (Gen Element)
codeContent = phrasingContent

tableColumn :: Int -> Gen Element
tableColumn maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  TableColumn <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

tableColumnGroup :: Int -> Int -> Gen Element
tableColumnGroup maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    colgroupChildren = NEL.toList $ tableColumnGroupContent maxAttrs

  TableColumnGroup
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice colgroupChildren)

tableColumnGroupContent :: Int -> NonEmpty (Gen Element)
tableColumnGroupContent maxAttrs =
  comment :| [ tableColumn maxAttrs ]

data_ :: Int -> Int -> Gen Element
data_ maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    dataChildren = NEL.toList $ dataContent maxAttrs maxChildren

  Data
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice dataChildren)

dataContent :: Int -> Int -> NonEmpty (Gen Element)
dataContent = phrasingContent

dataList :: Int -> Int -> Gen Element
dataList maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    datalistChildren = NEL.toList $ dataListContent maxAttrs maxChildren

  DataList
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice datalistChildren)

dataListContent :: Int -> Int -> NonEmpty (Gen Element)
dataListContent maxAttrs maxChildren =
  NEL.cons
    (option maxAttrs)
    (phrasingContent maxAttrs maxChildren)

descriptionDetails :: Int -> Int -> Gen Element
descriptionDetails maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    ddChildren = NEL.toList $ descriptionDetailsContent maxAttrs maxChildren

  DescriptionDetails
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice ddChildren)

descriptionDetailsContent :: Int -> Int -> NonEmpty (Gen Element)
descriptionDetailsContent = flowContent

deletedText :: Int -> Int -> Gen Element
deletedText maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    delChildren = NEL.toList deletedTextContent

  DeletedText
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice delChildren)

deletedTextContent :: NonEmpty (Gen Element)
deletedTextContent = NEL.singleton text

details :: Int -> Int -> Gen Element
details maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    detailsChildren = NEL.toList $ detailsContent maxAttrs maxChildren

  Details
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice detailsChildren)

detailsContent :: Int -> Int -> NonEmpty (Gen Element)
detailsContent maxAttrs maxChildren =
  NEL.cons
    (summary maxAttrs maxChildren)
    (flowContent maxAttrs maxChildren)

definition :: Int -> Int -> Gen Element
definition maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    dfnChildren = NEL.toList $ definitionContent maxAttrs maxChildren

  Definition
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice dfnChildren)

definitionContent :: Int -> Int -> NonEmpty (Gen Element)
definitionContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbreviation maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , keyboardInput maxAttrs maxChildren
       , label maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

dialog :: Int -> Int -> Gen Element
dialog maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    dialogChildren = NEL.toList $ dialogContent maxAttrs maxChildren

  Dialog
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice dialogChildren)

dialogContent :: Int -> Int -> NonEmpty (Gen Element)
dialogContent = flowContent

division :: Int -> Int -> Gen Element
division maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    divChildren = NEL.toList $ divisionContent maxAttrs maxChildren

  Division
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice divChildren)

divisionContent :: Int -> Int -> NonEmpty (Gen Element)
divisionContent = flowContent

descriptionList :: Int -> Int -> Gen Element
descriptionList maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    dlChildren = NEL.toList $ descriptionListContent maxAttrs maxChildren

  DescriptionList
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice dlChildren)

descriptionListContent :: Int -> Int -> NonEmpty (Gen Element)
descriptionListContent maxAttrs maxChildren =
  NEL.cons (descriptionTerm maxAttrs maxChildren)
    . NEL.cons (descriptionDetails maxAttrs maxChildren)
    . NEL.cons (division maxAttrs maxChildren)
    $ scriptSupportingContent maxAttrs maxChildren

descriptionTerm :: Int -> Int -> Gen Element
descriptionTerm maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    dtChildren =
      NEL.toList $ descriptionTermContent maxAttrs maxChildren

  DescriptionTerm
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice dtChildren)

descriptionTermContent :: Int -> Int -> NonEmpty (Gen Element)
descriptionTermContent maxAttrs maxChildren =
  text
    :| [ comment
       , anchor maxAttrs maxChildren
       , abbreviation maxAttrs maxChildren
       , contactAddress maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , deletedText maxAttrs maxChildren
       , details maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , division maxAttrs maxChildren
       , descriptionList maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , form maxAttrs maxChildren
       , horizontalRule maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , insertedText maxAttrs maxChildren
       , keyboardInput maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , orderedList maxAttrs maxChildren
       , output maxAttrs maxChildren
       , paragraph maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , preformattedText maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , table maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , unorderedList maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

emphasis :: Int -> Int -> Gen Element
emphasis maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    emChildren = NEL.toList $ emphasisContent maxAttrs maxChildren

  Emphasis
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice emChildren)

emphasisContent :: Int -> Int -> NonEmpty (Gen Element)
emphasisContent = phrasingContent

embed :: Int -> Gen Element
embed maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  Embed <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

fieldset :: Int -> Int -> Gen Element
fieldset maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    fieldsetChildren = NEL.toList $ fieldsetContent maxAttrs maxChildren

  Fieldset
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice fieldsetChildren)

fieldsetContent :: Int -> Int -> NonEmpty (Gen Element)
fieldsetContent maxAttrs maxChildren =
  NEL.cons
    (legend maxAttrs maxChildren)
    (flowContent maxAttrs maxChildren)

figureCaption :: Int -> Int -> Gen Element
figureCaption maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    figcaptionChildren = NEL.toList $ figureCaptionContent maxAttrs maxChildren

  FigureCaption
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice figcaptionChildren)

figureCaptionContent :: Int -> Int -> NonEmpty (Gen Element)
figureCaptionContent = flowContent

figure :: Int -> Int -> Gen Element
figure maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    figureChildren = NEL.toList $ figureContent maxAttrs maxChildren

  Figure
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice figureChildren)

figureContent :: Int -> Int -> NonEmpty (Gen Element)
figureContent maxAttrs maxChildren =
  NEL.cons
    (figureCaption maxAttrs maxChildren)
    (flowContent maxAttrs maxChildren)

footer :: Int -> Int -> Gen Element
footer maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    footerChildren = NEL.toList $ footerContent maxAttrs maxChildren

  Footer
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice footerChildren)

footerContent :: Int -> Int -> NonEmpty (Gen Element)
footerContent = marginalContent

form :: Int -> Int -> Gen Element
form maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    formChildren = NEL.toList $ formContent maxAttrs maxChildren

  Form
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice formChildren)

formContent :: Int -> Int -> NonEmpty (Gen Element)
formContent maxAttrs maxChildren =
  text
    :| [ comment
       , anchor maxAttrs maxChildren
       , abbreviation maxAttrs maxChildren
       , contactAddress maxAttrs maxChildren
       , article maxAttrs maxChildren
       , aside maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , deletedText maxAttrs maxChildren
       , details maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , division maxAttrs maxChildren
       , descriptionList maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , footer maxAttrs maxChildren
       , h1 maxAttrs maxChildren
       , h2 maxAttrs maxChildren
       , h3 maxAttrs maxChildren
       , h4 maxAttrs maxChildren
       , h5 maxAttrs maxChildren
       , h6 maxAttrs maxChildren
       , header maxAttrs maxChildren
       , headingGroup maxAttrs maxChildren
       , horizontalRule maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , insertedText maxAttrs maxChildren
       , keyboardInput maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , nav maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , orderedList maxAttrs maxChildren
       , output maxAttrs maxChildren
       , paragraph maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , preformattedText maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , section maxAttrs maxChildren
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , table maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , unorderedList maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

h1 :: Int -> Int -> Gen Element
h1 maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h1Children = NEL.toList $ h1Content maxAttrs maxChildren

  H1
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice h1Children)

h1Content :: Int -> Int -> NonEmpty (Gen Element)
h1Content = phrasingContent

h2 :: Int -> Int -> Gen Element
h2 maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h2Children = NEL.toList $ h2Content maxAttrs maxChildren

  H2
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice h2Children)

h2Content :: Int -> Int -> NonEmpty (Gen Element)
h2Content = phrasingContent

h3 :: Int -> Int -> Gen Element
h3 maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h3Children = NEL.toList $ h3Content maxAttrs maxChildren

  H3
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice h3Children)

h3Content :: Int -> Int -> NonEmpty (Gen Element)
h3Content = phrasingContent

h4 :: Int -> Int -> Gen Element
h4 maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h4Children = NEL.toList $ h4Content maxAttrs maxChildren

  H4
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice h4Children)

h4Content :: Int -> Int -> NonEmpty (Gen Element)
h4Content = phrasingContent

h5 :: Int -> Int -> Gen Element
h5 maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h5Children = NEL.toList $ h5Content maxAttrs maxChildren

  H5
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice h5Children)

h5Content :: Int -> Int -> NonEmpty (Gen Element)
h5Content = phrasingContent

h6 :: Int -> Int -> Gen Element
h6 maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    h6Children = NEL.toList $ h6Content maxAttrs maxChildren

  H6
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice h6Children)

h6Content :: Int -> Int -> NonEmpty (Gen Element)
h6Content = phrasingContent

head :: Int -> Int -> Gen Element
head maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    headChildren = NEL.toList $ headContent maxAttrs maxChildren

  Head
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice headChildren)

headContent :: Int -> Int -> NonEmpty (Gen Element)
headContent maxAttrs maxChildren =
  comment
    :| [ base maxAttrs
       , link maxAttrs
       , meta maxAttrs
       , noScript maxAttrs maxChildren
       , script maxAttrs
       , style maxAttrs
       , title maxAttrs
       ]

header :: Int -> Int -> Gen Element
header maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    headerChildren = NEL.toList $ headerContent maxAttrs maxChildren

  Header
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice headerChildren)

headerContent :: Int -> Int -> NonEmpty (Gen Element)
headerContent = marginalContent

headingGroup :: Int -> Int -> Gen Element
headingGroup maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    hgroupChildren = NEL.toList $ headingGroupContent maxAttrs maxChildren

  HeadingGroup
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice hgroupChildren)

headingGroupContent :: Int -> Int -> NonEmpty (Gen Element)
headingGroupContent maxAttrs maxChildren =
  NEL.cons comment
    . NEL.cons (paragraph maxAttrs maxChildren)
    $ headings maxAttrs maxChildren

horizontalRule :: Int -> Gen Element
horizontalRule maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  HorizontalRule <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

html :: Int -> Int -> Gen Element
html maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)

  Html
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> sequence [ head maxAttrs maxChildren, body maxAttrs maxChildren ]

idiomaticText :: Int -> Int -> Gen Element
idiomaticText maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    iChildren = NEL.toList $ idiomaticTextContent maxAttrs maxChildren

  IdiomaticText
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice iChildren)

idiomaticTextContent :: Int -> Int -> NonEmpty (Gen Element)
idiomaticTextContent = phrasingContent

iFrame :: Int -> Gen Element
iFrame maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  IFrame <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

image :: Int -> Gen Element
image maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  Image <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

input :: Int -> Gen Element
input maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  Input <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

insertedText :: Int -> Int -> Gen Element
insertedText maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    insChildren = NEL.toList insertedTextContent

  InsertedText
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice insChildren)

insertedTextContent :: NonEmpty (Gen Element)
insertedTextContent = NEL.singleton text

keyboardInput :: Int -> Int -> Gen Element
keyboardInput maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    kbdChildren = NEL.toList $ keyboardInputContent maxAttrs maxChildren

  KeyboardInput
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice kbdChildren)

keyboardInputContent :: Int -> Int -> NonEmpty (Gen Element)
keyboardInputContent = phrasingContent

label :: Int -> Int -> Gen Element
label maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    labelChildren = NEL.toList $ labelContent maxAttrs maxChildren

  Label
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice labelChildren)

labelContent :: Int -> Int -> NonEmpty (Gen Element)
labelContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbreviation maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , keyboardInput maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

legend :: Int -> Int -> Gen Element
legend maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    legendChildren = NEL.toList $ legendContent maxAttrs maxChildren

  Legend
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice legendChildren)

legendContent :: Int -> Int -> NonEmpty (Gen Element)
legendContent maxAttrs maxChildren =
  headings maxAttrs maxChildren
    <> phrasingContent maxAttrs maxChildren

listItem :: Int -> Int -> Gen Element
listItem maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    liChildren = NEL.toList $ listItemContent maxAttrs maxChildren

  ListItem
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice liChildren)

listItemContent :: Int -> Int -> NonEmpty (Gen Element)
listItemContent = flowContent

link :: Int -> Gen Element
link maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  Link <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

main :: Int -> Int -> Gen Element
main maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    mainChildren = NEL.toList $ mainContent maxAttrs maxChildren

  Main
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice mainChildren)

mainContent :: Int -> Int -> NonEmpty (Gen Element)
mainContent = flowContent

map :: Int -> Int -> Gen Element
map maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    mapChildren = NEL.toList $ mapContent maxAttrs maxChildren

  Map
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice mapChildren)

mapContent :: Int -> Int -> NonEmpty (Gen Element)
mapContent maxAttrs maxChildren =
  comment
    :| [ anchor maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , deletedText maxAttrs maxChildren
       , insertedText maxAttrs maxChildren
       , map maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , video maxAttrs maxChildren
       ]

mark :: Int -> Int -> Gen Element
mark maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    markChildren = NEL.toList $ markContent maxAttrs maxChildren

  Mark
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice markChildren)

markContent :: Int -> Int -> NonEmpty (Gen Element)
markContent = phrasingContent

menu :: Int -> Int -> Gen Element
menu maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    menuChildren = NEL.toList $ menuContent maxAttrs maxChildren

  Menu
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice menuChildren)

menuContent :: Int -> Int -> NonEmpty (Gen Element)
menuContent = listContent

meta :: Int -> Gen Element
meta maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  Meta <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

meter :: Int -> Int -> Gen Element
meter maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    meterChildren = NEL.toList $ meterContent maxAttrs maxChildren

  Meter
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice meterChildren)

meterContent :: Int -> Int -> NonEmpty (Gen Element)
meterContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbreviation maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , keyboardInput maxAttrs maxChildren
       , label maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

nav :: Int -> Int -> Gen Element
nav maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    navChildren = NEL.toList $ navContent maxAttrs maxChildren

  Nav
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice navChildren)

navContent :: Int -> Int -> NonEmpty (Gen Element)
navContent = flowContent

noScript :: Int -> Int -> Gen Element
noScript maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    noScriptChildren = NEL.toList noScriptContent

  NoScript
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice noScriptChildren)

noScriptContent :: NonEmpty (Gen Element)
noScriptContent = NEL.singleton text

object :: Int -> Int -> Gen Element
object maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    objectChildren = NEL.toList objectContent

  Object
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice objectChildren)

objectContent :: NonEmpty (Gen Element)
objectContent = NEL.singleton text

orderedList :: Int -> Int -> Gen Element
orderedList maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    olChildren = NEL.toList $ orderedListContent maxAttrs maxChildren

  OrderedList
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice olChildren)

orderedListContent :: Int -> Int -> NonEmpty (Gen Element)
orderedListContent = listContent

optionGroup :: Int -> Int -> Gen Element
optionGroup maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    optGroupChildren = NEL.toList $ optionGroupContent maxAttrs

  OptionGroup
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice optGroupChildren)

optionGroupContent :: Int -> NonEmpty (Gen Element)
optionGroupContent maxAttrs =
  comment :| [ option maxAttrs ]

option :: Int -> Gen Element
option maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)

  Option
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Generators.text

output :: Int -> Int -> Gen Element
output maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    outputChildren = NEL.toList $ outputContent maxAttrs maxChildren

  Output
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice outputChildren)

outputContent :: Int -> Int -> NonEmpty (Gen Element)
outputContent = phrasingContent

paragraph :: Int -> Int -> Gen Element
paragraph maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    pChildren = NEL.toList $ paragraphContent maxAttrs maxChildren

  Paragraph
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice pChildren)

paragraphContent :: Int -> Int -> NonEmpty (Gen Element)
paragraphContent = phrasingContent

picture :: Int -> Int -> Gen Element
picture maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    pictureChildren = NEL.toList $ pictureContent maxAttrs maxChildren

  Picture
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice pictureChildren)

pictureContent :: Int -> Int -> NonEmpty (Gen Element)
pictureContent maxAttrs maxChildren =
  NEL.cons comment
    . NEL.cons (source maxAttrs)
    . NEL.cons (image maxAttrs)
    $ scriptSupportingContent maxAttrs maxChildren

preformattedText :: Int -> Int -> Gen Element
preformattedText maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    preChildren = NEL.toList $ preformattedTextContent maxAttrs maxChildren

  PreformattedText
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice preChildren)

preformattedTextContent :: Int -> Int -> NonEmpty (Gen Element)
preformattedTextContent = phrasingContent

progress :: Int -> Int -> Gen Element
progress maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    progressChildren = NEL.toList $ progressContent maxAttrs maxChildren

  Progress
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice progressChildren)

progressContent :: Int -> Int -> NonEmpty (Gen Element)
progressContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbreviation maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , keyboardInput maxAttrs maxChildren
       , label maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

quotation :: Int -> Int -> Gen Element
quotation maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    qChildren = NEL.toList $ quotationContent maxAttrs maxChildren

  Quotation
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice qChildren)

quotationContent :: Int -> Int -> NonEmpty (Gen Element)
quotationContent = phrasingContent

rubyText :: Int -> Int -> Gen Element
rubyText maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    rtChildren = NEL.toList $ rubyTextContent maxAttrs maxChildren

  RubyText
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice rtChildren)

rubyTextContent :: Int -> Int -> NonEmpty (Gen Element)
rubyTextContent = phrasingContent

ruby :: Int -> Int -> Gen Element
ruby maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)
  rubyContent <- Gen.list (Range.singleton children) text
  rubyTexts <- Gen.list (Range.singleton children) (rubyText maxAttrs maxChildren)

  let
    addParens (content, rt) =
      [ content
      , RubyParenthesis [] "("
      , rt
      , RubyParenthesis [] ")"
      ]

  Ruby
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> pure (concatMap addParens $ zip rubyContent rubyTexts)

strikethrough :: Int -> Int -> Gen Element
strikethrough maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    sChildren = NEL.toList $ strikethroughContent maxAttrs maxChildren

  Strikethrough
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice sChildren)

strikethroughContent :: Int -> Int -> NonEmpty (Gen Element)
strikethroughContent = phrasingContent

sample :: Int -> Int -> Gen Element
sample maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    sampleChildren = NEL.toList $ sampleContent maxAttrs maxChildren

  Sample
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice sampleChildren)

sampleContent :: Int -> Int -> NonEmpty (Gen Element)
sampleContent = phrasingContent

script :: Int -> Gen Element
script maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  Script
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Generators.nonEmptyText

search :: Int -> Int -> Gen Element
search maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    searchChildren = NEL.toList $ searchContent maxAttrs maxChildren

  Search
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice searchChildren)

searchContent :: Int -> Int -> NonEmpty (Gen Element)
searchContent = flowContent

section :: Int -> Int -> Gen Element
section maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    sectionChildren = NEL.toList $ sectionContent maxAttrs maxChildren

  Section
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice sectionChildren)

sectionContent :: Int -> Int -> NonEmpty (Gen Element)
sectionContent = flowContent

select :: Int -> Int -> Gen Element
select maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    selectChildren = NEL.toList $ selectContent maxAttrs maxChildren

  Select
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice selectChildren)

selectContent :: Int -> Int -> NonEmpty (Gen Element)
selectContent maxAttrs maxChildren =
  comment
    :| [ option maxAttrs
       , optionGroup maxAttrs maxChildren
       ]

slot :: Int -> Int -> Gen Element
slot maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    slotChildren = NEL.toList slotContent

  Slot
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice slotChildren)

slotContent :: NonEmpty (Gen Element)
slotContent = NEL.singleton text

sideComment :: Int -> Int -> Gen Element
sideComment maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    smallChildren = NEL.toList $ sideCommentContent maxAttrs maxChildren

  SideComment
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice smallChildren)

sideCommentContent :: Int -> Int -> NonEmpty (Gen Element)
sideCommentContent = phrasingContent

source :: Int -> Gen Element
source maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  Source <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

span :: Int -> Int -> Gen Element
span maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    spanChildren = NEL.toList $ spanContent maxAttrs maxChildren

  Span
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice spanChildren)

spanContent :: Int -> Int -> NonEmpty (Gen Element)
spanContent = phrasingContent

strong :: Int -> Int -> Gen Element
strong maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    strongChildren = NEL.toList $ strongContent maxAttrs maxChildren

  Strong
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice strongChildren)

strongContent :: Int -> Int -> NonEmpty (Gen Element)
strongContent = phrasingContent

style :: Int -> Gen Element
style maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  Style <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

subscript :: Int -> Int -> Gen Element
subscript maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    subChildren = NEL.toList $ subscriptContent maxAttrs maxChildren

  Subscript
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice subChildren)

subscriptContent :: Int -> Int -> NonEmpty (Gen Element)
subscriptContent = phrasingContent

summary :: Int -> Int -> Gen Element
summary maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    summaryChildren = NEL.toList $ summaryContent maxAttrs maxChildren

  Summary
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice summaryChildren)

summaryContent :: Int -> Int -> NonEmpty (Gen Element)
summaryContent maxAttrs maxChildren =
  NEL.cons (headingGroup maxAttrs maxChildren) $
    headings maxAttrs maxChildren
      <> phrasingContent maxAttrs maxChildren

superscript :: Int -> Int -> Gen Element
superscript maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    supChildren = NEL.toList $ superscriptContent maxAttrs maxChildren

  Superscript
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice supChildren)

superscriptContent :: Int -> Int -> NonEmpty (Gen Element)
superscriptContent = phrasingContent

table :: Int -> Int -> Gen Element
table maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tableChildren = NEL.toList $ tableContent maxAttrs maxChildren

  Table
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice tableChildren)

tableContent :: Int -> Int -> NonEmpty (Gen Element)
tableContent maxAttrs maxChildren =
  tableCaption maxAttrs maxChildren
    :| [ tableColumnGroup maxAttrs maxChildren
       , tableHead maxAttrs maxChildren
       , tableBody maxAttrs maxChildren
       , tableRow maxAttrs maxChildren
       , tableFoot maxAttrs maxChildren
       ]

tableBody :: Int -> Int -> Gen Element
tableBody maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tbodyChildren = NEL.toList $ tableBodyContent maxAttrs maxChildren

  TableBody
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice tbodyChildren)

tableBodyContent :: Int -> Int -> NonEmpty (Gen Element)
tableBodyContent maxAttrs maxChildren =
  comment :| [ tableRow maxAttrs maxChildren ]

tableDataCell :: Int -> Int -> Gen Element
tableDataCell maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tdChildren = NEL.toList $ tableDataCellContent maxAttrs maxChildren

  TableDataCell
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice tdChildren)

tableDataCellContent :: Int -> Int -> NonEmpty (Gen Element)
tableDataCellContent = flowContent

contentTemplate :: Int -> Int -> Gen Element
contentTemplate maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    contentTemplateChildren = NEL.toList $ contentTemplateContent maxAttrs maxChildren

  ContentTemplate
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice contentTemplateChildren)

contentTemplateContent :: Int -> Int -> NonEmpty (Gen Element)
contentTemplateContent = allElements

textArea :: Int -> Int -> Gen Element
textArea maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  TextArea
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) text

tableFoot :: Int -> Int -> Gen Element
tableFoot maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tfootChildren = NEL.toList $ tableFootContent maxAttrs maxChildren

  TableFoot
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice tfootChildren)

tableFootContent :: Int -> Int -> NonEmpty (Gen Element)
tableFootContent maxAttrs maxChildren =
  comment :| [ tableRow maxAttrs maxChildren ]

tableHeader :: Int -> Int -> Gen Element
tableHeader maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    theadChildren = NEL.toList $ tableHeaderContent maxAttrs maxChildren

  TableHeader
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice theadChildren)

tableHeaderContent :: Int -> Int -> NonEmpty (Gen Element)
tableHeaderContent maxAttrs maxChildren =
  text
    :| [ comment
       , anchor maxAttrs maxChildren
       , abbreviation maxAttrs maxChildren
       , contactAddress maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , deletedText maxAttrs maxChildren
       , details maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , division maxAttrs maxChildren
       , descriptionList maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , form maxAttrs maxChildren
       , horizontalRule maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , insertedText maxAttrs maxChildren
       , keyboardInput maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , orderedList maxAttrs maxChildren
       , output maxAttrs maxChildren
       , paragraph maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , preformattedText maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , table maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , unorderedList maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

tableHead :: Int -> Int -> Gen Element
tableHead maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    thChildren = NEL.toList $ tableHeadContent maxAttrs maxChildren

  TableHead
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice thChildren)

tableHeadContent :: Int -> Int -> NonEmpty (Gen Element)
tableHeadContent maxAttrs maxChildren =
  comment :| [ tableRow maxAttrs maxChildren ]

time :: Int -> Int -> Gen Element
time maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    timeChildren = NEL.toList $ timeContent maxAttrs maxChildren

  Time
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice timeChildren)

timeContent :: Int -> Int -> NonEmpty (Gen Element)
timeContent = phrasingContent

title :: Int -> Gen Element
title maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)

  Title
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Generators.text

tableRow :: Int -> Int -> Gen Element
tableRow maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    tableRowChildren = NEL.toList $ tableRowContent maxAttrs maxChildren

  TableRow
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice tableRowChildren)

tableRowContent :: Int -> Int -> NonEmpty (Gen Element)
tableRowContent maxAttrs maxChildren =
  NEL.cons (tableDataCell maxAttrs maxChildren)
    . NEL.cons (tableHeader maxAttrs maxChildren)
    $ scriptSupportingContent maxAttrs maxChildren

track :: Int -> Gen Element
track maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  Track <$> pure [] -- TODO: <$> Gen.list (Range.singleton attrs) (Gen.choice [])

underline :: Int -> Int -> Gen Element
underline maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    uChildren = NEL.toList $ underlineContent maxAttrs maxChildren

  Underline
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice uChildren)

underlineContent :: Int -> Int -> NonEmpty (Gen Element)
underlineContent = phrasingContent

unorderedList :: Int -> Int -> Gen Element
unorderedList maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    ulChildren = NEL.toList $ unorderedListContent maxAttrs maxChildren

  UnorderedList
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice ulChildren)

unorderedListContent :: Int -> Int -> NonEmpty (Gen Element)
unorderedListContent = listContent

variable :: Int -> Int -> Gen Element
variable maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    varChildren = NEL.toList $ variableContent maxAttrs maxChildren

  Variable
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice varChildren)

variableContent :: Int -> Int -> NonEmpty (Gen Element)
variableContent = phrasingContent

video :: Int -> Int -> Gen Element
video maxAttrs maxChildren = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)
  children <- Gen.int (Range.linear 0 maxChildren)

  let
    videoChildren = NEL.toList $ videoContent maxAttrs

  Video
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])
    <*> Gen.list (Range.singleton children) (Gen.choice videoChildren)

videoContent :: Int -> NonEmpty (Gen Element)
videoContent = audioVideoContent

wordBreakOpportunity :: Int -> Gen Element
wordBreakOpportunity maxAttrs = do
  _attrs <- Gen.int (Range.linear 0 maxAttrs)

  WordBreakOpportunity
    <$> pure [] -- TODO: Gen.list (Range.singleton attrs) (Gen.choice [])

-- Content Categories
--

allElements :: Int -> Int -> NonEmpty (Gen Element)
allElements maxAttrs maxChildren =
  comment
    :| [ text
       , anchor maxAttrs maxChildren
       , abbreviation maxAttrs maxChildren
       , contactAddress maxAttrs maxChildren
       , area maxAttrs
       , article maxAttrs maxChildren
       , aside maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , base maxAttrs
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , body maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , tableCaption maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , tableColumn maxAttrs
       , tableColumnGroup maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , descriptionDetails maxAttrs maxChildren
       , deletedText maxAttrs maxChildren
       , details maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , division maxAttrs maxChildren
       , descriptionList maxAttrs maxChildren
       , descriptionTerm maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figureCaption maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , footer maxAttrs maxChildren
       , form maxAttrs maxChildren
       , h1 maxAttrs maxChildren
       , h2 maxAttrs maxChildren
       , h3 maxAttrs maxChildren
       , h4 maxAttrs maxChildren
       , h5 maxAttrs maxChildren
       , h6 maxAttrs maxChildren
       , head maxAttrs maxChildren
       , header maxAttrs maxChildren
       , headingGroup maxAttrs maxChildren
       , horizontalRule maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , insertedText maxAttrs maxChildren
       , keyboardInput maxAttrs maxChildren
       , label maxAttrs maxChildren
       , legend maxAttrs maxChildren
       , listItem maxAttrs maxChildren
       , link maxAttrs
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meta maxAttrs
       , meter maxAttrs maxChildren
       , nav maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , orderedList maxAttrs maxChildren
       , optionGroup maxAttrs maxChildren
       , option maxAttrs
       , output maxAttrs maxChildren
       , paragraph maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , preformattedText maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , script maxAttrs
       , search maxAttrs maxChildren
       , section maxAttrs maxChildren
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , source maxAttrs
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , style maxAttrs
       , subscript maxAttrs maxChildren
       , summary maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , table maxAttrs maxChildren
       , tableBody maxAttrs maxChildren
       , tableDataCell maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , tableFoot maxAttrs maxChildren
       , tableHeader maxAttrs maxChildren
       , tableHead maxAttrs maxChildren
       , time maxAttrs maxChildren
       , title maxAttrs
       , tableRow maxAttrs maxChildren
       , track maxAttrs
       , underline maxAttrs maxChildren
       , unorderedList maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

audioVideoContent :: Int -> NonEmpty (Gen Element)
audioVideoContent maxAttrs =
  comment
    :| [ source maxAttrs
       , track maxAttrs
       ]

flowContent :: Int -> Int -> NonEmpty (Gen Element)
flowContent maxAttrs maxChildren =
  text
    :| [ comment
       , anchor maxAttrs maxChildren
       , abbreviation maxAttrs maxChildren
       , contactAddress maxAttrs maxChildren
       , article maxAttrs maxChildren
       , aside maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , deletedText maxAttrs maxChildren
       , details maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , division maxAttrs maxChildren
       , descriptionList maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , footer maxAttrs maxChildren
       , form maxAttrs maxChildren
       , h1 maxAttrs maxChildren
       , h2 maxAttrs maxChildren
       , h3 maxAttrs maxChildren
       , h4 maxAttrs maxChildren
       , h5 maxAttrs maxChildren
       , h6 maxAttrs maxChildren
       , header maxAttrs maxChildren
       , headingGroup maxAttrs maxChildren
       , horizontalRule maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , insertedText maxAttrs maxChildren
       , keyboardInput maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , nav maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , orderedList maxAttrs maxChildren
       , output maxAttrs maxChildren
       , paragraph maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , preformattedText maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , section maxAttrs maxChildren
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , table maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , unorderedList maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

headings :: Int -> Int -> NonEmpty (Gen Element)
headings maxAttrs maxChildren =
  comment
    :| [ h1 maxAttrs maxChildren
       , h2 maxAttrs maxChildren
       , h3 maxAttrs maxChildren
       , h4 maxAttrs maxChildren
       , h5 maxAttrs maxChildren
       , h6 maxAttrs maxChildren
       ]

listContent :: Int -> Int -> NonEmpty (Gen Element)
listContent maxAttrs maxChildren =
  NEL.cons
    (listItem maxAttrs maxChildren)
    (scriptSupportingContent maxAttrs maxChildren)

marginalContent :: Int -> Int -> NonEmpty (Gen Element)
marginalContent maxAttrs maxChildren =
  text
    :| [ comment
       , anchor maxAttrs maxChildren
       , abbreviation maxAttrs maxChildren
       , contactAddress maxAttrs maxChildren
       , article maxAttrs maxChildren
       , aside maxAttrs maxChildren
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , blockquote maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , deletedText maxAttrs maxChildren
       , details maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , dialog maxAttrs maxChildren
       , division maxAttrs maxChildren
       , descriptionList maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , fieldset maxAttrs maxChildren
       , figure maxAttrs maxChildren
       , form maxAttrs maxChildren
       , h1 maxAttrs maxChildren
       , h2 maxAttrs maxChildren
       , h3 maxAttrs maxChildren
       , h4 maxAttrs maxChildren
       , h5 maxAttrs maxChildren
       , h6 maxAttrs maxChildren
       , headingGroup maxAttrs maxChildren
       , horizontalRule maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , insertedText maxAttrs maxChildren
       , keyboardInput maxAttrs maxChildren
       , label maxAttrs maxChildren
       , main maxAttrs maxChildren
       , map maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , menu maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , nav maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , orderedList maxAttrs maxChildren
       , output maxAttrs maxChildren
       , paragraph maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , preformattedText maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , search maxAttrs maxChildren
       , script maxAttrs
       , section maxAttrs maxChildren
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , table maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , unorderedList maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

phrasingContent :: Int -> Int -> NonEmpty (Gen Element)
phrasingContent maxAttrs maxChildren =
  text
    :| [ comment
       , abbreviation maxAttrs maxChildren
       , area maxAttrs
       , audio maxAttrs maxChildren
       , bringAttentionTo maxAttrs maxChildren
       , bidirectionalIsolation maxAttrs maxChildren
       , bidirectionalOverride maxAttrs maxChildren
       , lineBreak maxAttrs
       , button maxAttrs maxChildren
       , canvas maxAttrs maxChildren
       , citation maxAttrs maxChildren
       , code maxAttrs maxChildren
       , data_ maxAttrs maxChildren
       , dataList maxAttrs maxChildren
       , definition maxAttrs maxChildren
       , emphasis maxAttrs maxChildren
       , embed maxAttrs
       , idiomaticText maxAttrs maxChildren
       , iFrame maxAttrs
       , image maxAttrs
       , input maxAttrs
       , keyboardInput maxAttrs maxChildren
       , label maxAttrs maxChildren
       , mark maxAttrs maxChildren
       , meter maxAttrs maxChildren
       , noScript maxAttrs maxChildren
       , object maxAttrs maxChildren
       , output maxAttrs maxChildren
       , picture maxAttrs maxChildren
       , progress maxAttrs maxChildren
       , quotation maxAttrs maxChildren
       , ruby maxAttrs maxChildren
       , strikethrough maxAttrs maxChildren
       , sample maxAttrs maxChildren
       , script maxAttrs
       , select maxAttrs maxChildren
       , slot maxAttrs maxChildren
       , sideComment maxAttrs maxChildren
       , span maxAttrs maxChildren
       , strong maxAttrs maxChildren
       , subscript maxAttrs maxChildren
       , superscript maxAttrs maxChildren
       , contentTemplate maxAttrs maxChildren
       , textArea maxAttrs maxChildren
       , time maxAttrs maxChildren
       , underline maxAttrs maxChildren
       , variable maxAttrs maxChildren
       , video maxAttrs maxChildren
       , wordBreakOpportunity maxAttrs
       ]

scriptSupportingContent :: Int -> Int -> NonEmpty (Gen Element)
scriptSupportingContent maxAttrs maxChildren =
  comment
    :| [ script maxAttrs
       , contentTemplate maxAttrs maxChildren
       ]
