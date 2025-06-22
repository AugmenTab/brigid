module Brigid.HTML.Generation.Internal.Elements
  ( elementNodeType
  , elementValidAttrs
  , elementValidChildren
  , elementWeight
  , branchElements
  , leafElements
  , leafBranchElements
  , voidElements
  , withGlobalAttrs
  ) where

import Data.Containers.ListUtils (nubOrdOn)
import Data.Set (Set)
import Data.Set qualified as Set
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Brigid.HTML.Generation.Internal.Attributes qualified as A
import Brigid.HTML.Generation.Internal.Types (ElementType (..), GeneratorParams (..), NodeType (..), maxRange, minRange)

elementNodeType :: ElementType -> NodeType
elementNodeType element =
  if Set.member element voidElements
    then VoidNode
    else
      if Set.member element leafElements
        then LeafNode
        else BranchNode

elementValidAttrs :: MonadGen m => ElementType -> [m A.Attribute]
elementValidAttrs element =
  case element of
    Comment -> []
    Anchor -> anchorAttrs
    Abbreviation -> [ A.elementtiming ]
    ContactAddress -> []
    Area -> areaAttrs
    Article -> [ A.elementtiming ]
    Aside -> [ A.elementtiming ]
    Audio -> audioAttrs
    BringAttentionTo -> [ A.elementtiming ]
    Base -> baseAttrs
    BidirectionalIsolation -> []
    BidirectionalOverride -> []
    Blockquote -> blockquoteAttrs
    Body -> []
    LineBreak -> []
    Button -> buttonAttrs
    Canvas -> canvasAttrs
    TableCaption -> [ A.elementtiming ]
    Citation -> [ A.elementtiming ]
    Code -> [ A.elementtiming ]
    TableColumn -> [ A.span ]
    TableColumnGroup -> [ A.span ]
    Data -> [ A.value ]
    DataList -> []
    DescriptionDetails -> [ A.elementtiming ]
    DeletedText -> deletedTextAttrs
    Details -> detailsAttrs
    Definition -> [ A.elementtiming ]
    Dialog -> [ A.open ]
    Division -> [ A.elementtiming ]
    DescriptionList -> []
    DescriptionTerm -> [ A.elementtiming ]
    Emphasis -> [ A.elementtiming ]
    Embed -> embedAttrs
    Fieldset -> fieldsetAttrs
    FigureCaption -> [ A.elementtiming ]
    Figure -> []
    Footer -> [ A.elementtiming ]
    Form -> formAttrs
    H1 -> [ A.elementtiming ]
    H2 -> [ A.elementtiming ]
    H3 -> [ A.elementtiming ]
    H4 -> [ A.elementtiming ]
    H5 -> [ A.elementtiming ]
    H6 -> [ A.elementtiming ]
    Head -> []
    Header -> [ A.elementtiming ]
    HeadingGroup -> []
    HorizontalRule -> []
    Html -> [ A.xmlns ]
    IdiomaticText -> [ A.elementtiming ]
    IFrame -> iFrameAttrs
    Image -> imageAttrs
    Input -> inputAttrs
    InsertedText -> insertedTextAttrs
    KeyboardInput -> [ A.elementtiming ]
    Label -> labelAttrs
    Legend -> [ A.elementtiming ]
    ListItem -> listItemAttrs
    Link -> linkAttrs
    Main -> [ A.elementtiming ]
    Map -> [ A.name ]
    Mark -> [ A.elementtiming ]
    Menu -> []
    Meta -> metaAttrs
    Meter -> meterAttrs
    Nav -> [ A.elementtiming ]
    NoScriptHead -> []
    NoScriptBody -> []
    Object -> objectAttrs
    OrderedList -> orderedListAttrs
    OptionGroup -> optionGroupAttrs
    Option -> optionAttrs
    Output -> outputAttrs
    Paragraph -> [ A.elementtiming ]
    Picture -> []
    PreformattedText -> [ A.elementtiming ]
    Progress -> progressAttrs
    Quotation -> quotationAttrs
    RubyParenthesis -> []
    RubyText -> []
    Ruby -> []
    Strikethrough -> []
    Sample -> [ A.elementtiming ]
    Script -> scriptAttrs
    Search -> []
    Section -> [ A.elementtiming ]
    Select -> selectAttrs
    Slot -> [ A.name ]
    SideComment -> [ A.elementtiming ]
    Source -> sourceAttrs
    Span -> [ A.elementtiming ]
    Strong -> [ A.elementtiming ]
    Style -> styleAttrs
    Subscript -> []
    Summary -> [ A.elementtiming ]
    Superscript -> []
    Table -> []
    TableBody -> []
    TableDataCell -> tableDataCellAttrs
    ContentTemplate -> contentTemplateAttrs
    TextArea -> textAreaAttrs
    TableFoot -> []
    TableHeader -> tableHeaderAttrs
    TableHead -> []
    Time -> timeAttrs
    Title -> []
    TableRow -> []
    Track -> trackAttrs
    Underline -> [ A.elementtiming ]
    UnorderedList -> []
    Variable -> [ A.elementtiming ]
    Video -> videoAttrs
    WordBreakOpportunity -> []

elementValidChildren :: ElementType -> Set ElementType
elementValidChildren element =
  case element of
    Comment -> Set.empty
    Anchor -> Set.empty
    Abbreviation -> abbreviationContent
    ContactAddress -> contactAddressContent
    Area -> Set.empty
    Article -> articleContent
    Aside -> asideContent
    Audio -> audioContent
    BringAttentionTo -> bringAttentionToContent
    Base -> Set.empty
    BidirectionalIsolation -> bidirectionalIsolationContent
    BidirectionalOverride -> bidirectionalOverrideContent
    Blockquote -> blockquoteContent
    Body -> bodyContent
    LineBreak -> Set.empty
    Button -> buttonContent
    Canvas -> canvasContent
    TableCaption -> tableCaptionContent
    Citation -> citationContent
    Code -> codeContent
    TableColumn -> tableColumnGroupContent
    TableColumnGroup -> tableColumnGroupContent
    Data -> dataContent
    DataList -> dataListContent
    DescriptionDetails -> descriptionDetailsContent
    DeletedText -> Set.empty
    Details -> detailsContent
    Definition -> definitionContent
    Dialog -> dialogContent
    Division -> divisionContent
    DescriptionList -> descriptionListContent
    DescriptionTerm -> descriptionTermContent
    Emphasis -> emphasisContent
    Embed -> Set.empty
    Fieldset -> fieldsetContent
    FigureCaption -> figureCaptionContent
    Figure -> figureContent
    Footer -> footerContent
    Form -> formContent
    H1 -> h1Content
    H2 -> h2Content
    H3 -> h3Content
    H4 -> h4Content
    H5 -> h5Content
    H6 -> h6Content
    Head -> headContent
    Header -> headerContent
    HeadingGroup -> headingGroupContent
    HorizontalRule -> Set.empty
    Html -> Set.fromList [ Head, Body ]
    IdiomaticText -> idiomaticTextContent
    IFrame -> Set.empty
    Image -> Set.empty
    Input -> Set.empty
    InsertedText -> Set.empty
    KeyboardInput -> keyboardInputContent
    Label -> labelContent
    Legend -> legendContent
    ListItem -> listItemContent
    Link -> Set.empty
    Main -> mainContent
    Map -> mapContent
    Mark -> markContent
    Menu -> menuContent
    Meta -> Set.empty
    Meter -> meterContent
    Nav -> navContent
    NoScriptHead -> noScriptHeadContent
    NoScriptBody -> noScriptBodyContent
    Object -> Set.empty
    OrderedList -> orderedListContent
    OptionGroup -> optionGroupContent
    Option -> Set.empty
    Output -> outputContent
    Paragraph -> paragraphContent
    Picture -> pictureContent
    PreformattedText -> preformattedTextContent
    Progress -> progressContent
    Quotation -> quotationContent
    RubyParenthesis -> Set.empty
    RubyText -> rubyTextContent
    Ruby -> rubyContent
    Strikethrough -> strikethroughContent
    Sample -> sampleContent
    Script -> Set.empty
    Search -> searchContent
    Section -> sectionContent
    Select -> selectContent
    Slot -> Set.empty
    SideComment -> sideCommentContent
    Source -> Set.empty
    Span -> spanContent
    Strong -> strongContent
    Style -> Set.empty
    Subscript -> subscriptContent
    Summary -> summaryContent
    Superscript -> superscriptContent
    Table -> tableContent
    TableBody -> tableBodyContent
    TableDataCell -> tableDataCellContent
    ContentTemplate -> contentTemplateContent
    TextArea -> Set.empty
    TableFoot -> tableFootContent
    TableHeader -> tableHeaderContent
    TableHead -> tableHeadContent
    Time -> timeContent
    Title -> Set.empty
    TableRow -> tableRowContent
    Track -> Set.empty
    Underline -> underlineContent
    UnorderedList -> unorderedListContent
    Variable -> variableContent
    Video -> videoContent
    WordBreakOpportunity -> Set.empty

elementWeight :: ElementType -> Maybe Int
elementWeight et =
  case et of
    Comment -> Nothing
    Anchor -> Just 10
    Abbreviation -> Just 1
    ContactAddress -> Just 1
    Area -> Just 1
    Article -> Just 8
    Aside -> Just 1
    Audio -> Just 2
    BringAttentionTo -> Just 1
    Base -> Just 1
    BidirectionalIsolation -> Just 1
    BidirectionalOverride -> Just 1
    Blockquote -> Just 3
    Body -> Nothing
    LineBreak -> Just 5
    Button -> Just 5
    Canvas -> Just 2
    TableCaption -> Just 1
    Citation -> Just 1
    Code -> Just 4
    TableColumn -> Just 1
    TableColumnGroup -> Just 1
    Data -> Just 1
    DataList -> Just 1
    DescriptionDetails -> Just 5
    DeletedText -> Just 1
    Details -> Just 1
    Definition -> Just 1
    Dialog -> Just 1
    Division -> Just 25
    DescriptionList -> Just 4
    DescriptionTerm -> Just 5
    Emphasis -> Just 6
    Embed -> Just 1
    Fieldset -> Just 3
    FigureCaption -> Just 2
    Figure -> Just 3
    Footer -> Just 6
    Form -> Just 5
    H1 -> Just 5
    H2 -> Just 5
    H3 -> Just 5
    H4 -> Just 3
    H5 -> Just 3
    H6 -> Just 3
    Head -> Nothing
    Header -> Just 6
    HeadingGroup -> Just 1
    HorizontalRule -> Just 4
    Html -> Nothing
    IdiomaticText -> Just 1
    IFrame -> Just 1
    Image -> Just 7
    Input -> Just 8
    InsertedText -> Just 1
    KeyboardInput -> Just 1
    Label -> Just 6
    Legend -> Just 2
    ListItem -> Just 20
    Link -> Just 1
    Main -> Just 3
    Map -> Just 1
    Mark -> Just 1
    Menu -> Just 1
    Meta -> Just 1
    Meter -> Just 1
    Nav -> Just 5
    NoScriptHead -> Just 1
    NoScriptBody -> Just 1
    Object -> Just 1
    OrderedList -> Just 8
    OptionGroup -> Just 1
    Option -> Just 20
    Output -> Just 1
    Paragraph -> Just 15
    Picture -> Just 1
    PreformattedText -> Just 3
    Progress -> Just 1
    Quotation -> Just 1
    RubyParenthesis -> Just 1
    RubyText -> Just 1
    Ruby -> Just 1
    Strikethrough -> Just 1
    Sample -> Just 1
    Script -> Just 1
    Search -> Just 1
    Section -> Just 10
    Select -> Just 4
    Slot -> Just 1
    SideComment -> Just 1
    Source -> Just 3
    Span -> Just 12
    Strong -> Just 6
    Style -> Just 1
    Subscript -> Just 1
    Summary -> Just 1
    Superscript -> Just 1
    Table -> Just 4
    TableBody -> Just 3
    TableDataCell -> Just 12
    ContentTemplate -> Just 1
    TextArea -> Just 4
    TableFoot -> Just 1
    TableHeader -> Just 4
    TableHead -> Just 2
    Time -> Just 1
    Title -> Just 1
    TableRow -> Just 10
    Track -> Just 1
    Underline -> Just 1
    UnorderedList -> Just 12
    Variable -> Just 1
    Video -> Just 2
    WordBreakOpportunity -> Just 1

anchorAttrs :: MonadGen m => [m A.Attribute]
anchorAttrs =
  [ A.download
  , A.elementtiming
  , A.href
  , A.hreflang
  , A.ping
  , A.referrerpolicy
  , A.rel
  , A.target
  , A.type_
  ]

abbreviationContent :: Set ElementType
abbreviationContent = phrasingContent

contactAddressContent :: Set ElementType
contactAddressContent =
  Set.fromList
    [ Anchor
    , Abbreviation
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , Blockquote
    , LineBreak
    , Button
    , Canvas
    , Citation
    , Code
    , Data
    , DataList
    , DeletedText
    , Details
    , Definition
    , Dialog
    , Division
    , DescriptionList
    , Emphasis
    , Embed
    , Fieldset
    , Figure
    , Form
    , HorizontalRule
    , IdiomaticText
    , IFrame
    , Image
    , Input
    , InsertedText
    , KeyboardInput
    , Label
    , Main
    , Map
    , Mark
    , Menu
    , Meter
    , NoScriptBody
    , Object
    , OrderedList
    , Output
    , Paragraph
    , Picture
    , PreformattedText
    , Progress
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Search
    , Script
    , Select
    , Slot
    , SideComment
    , Span
    , Strong
    , Subscript
    , Superscript
    , Table
    , ContentTemplate
    , TextArea
    , Time
    , Underline
    , UnorderedList
    , Variable
    , Video
    , WordBreakOpportunity
    ]

areaAttrs :: MonadGen m => [m A.Attribute]
areaAttrs =
  [ A.alt
  , A.coords
  , A.download
  , A.href
  , A.ping
  , A.referrerpolicy
  , A.rel
  , A.shape
  , A.target
  ]

articleContent :: Set ElementType
articleContent = flowContent

asideContent :: Set ElementType
asideContent = flowContent

audioAttrs :: MonadGen m => [m A.Attribute]
audioAttrs =
  [ A.autoplay
  , A.controls
  , A.controlslist
  , A.crossorigin
  , A.disableremoteplayback
  , A.loop
  , A.muted
  , A.preload
  , A.src
  ]

audioContent :: Set ElementType
audioContent = audioVideoContent

bringAttentionToContent :: Set ElementType
bringAttentionToContent = phrasingContent

baseAttrs :: MonadGen m => [m A.Attribute]
baseAttrs =
  [ A.href
  , A.target
  ]

bidirectionalIsolationContent :: Set ElementType
bidirectionalIsolationContent = phrasingContent

bidirectionalOverrideContent :: Set ElementType
bidirectionalOverrideContent = phrasingContent

blockquoteContent :: Set ElementType
blockquoteContent = flowContent

blockquoteAttrs :: MonadGen m => [m A.Attribute]
blockquoteAttrs =
  [ A.cite
  , A.elementtiming
  ]

bodyContent :: Set ElementType
bodyContent = flowContent

buttonAttrs :: MonadGen m => [m A.Attribute]
buttonAttrs =
  [ A.command
  , A.commandfor
  , A.disabled
  , A.elementtiming
  , A.form
  , A.formaction
  , A.formenctype
  , A.formmethod
  , A.formnovalidate
  , A.formtarget
  , A.name
  , A.popovertarget
  , A.popovertargetaction
  , A.type_
  , A.value
  ]

buttonContent :: Set ElementType
buttonContent =
  Set.fromList
    [ Abbreviation
    , Area
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , LineBreak
    , Canvas
    , Citation
    , Code
    , Data
    , DataList
    , Definition
    , Emphasis
    , IdiomaticText
    , Image
    , KeyboardInput
    , Mark
    , Meter
    , NoScriptBody
    , Object
    , Output
    , Picture
    , Progress
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Script
    , Slot
    , SideComment
    , Span
    , Strong
    , Subscript
    , Superscript
    , ContentTemplate
    , Time
    , Underline
    , Variable
    , Video
    , WordBreakOpportunity
    ]

canvasAttrs :: MonadGen m => [m A.Attribute]
canvasAttrs =
  [ A.elementtiming
  , A.height
  , A.width
  ]

canvasContent :: Set ElementType
canvasContent =
  Set.fromList
    [ Anchor
    , Button
    , Input
    ]

tableCaptionContent :: Set ElementType
tableCaptionContent = flowContent

citationContent :: Set ElementType
citationContent = phrasingContent

codeContent :: Set ElementType
codeContent = phrasingContent

tableColumnGroupContent :: Set ElementType
tableColumnGroupContent =
  Set.singleton TableColumn

dataContent :: Set ElementType
dataContent = phrasingContent

dataListContent :: Set ElementType
dataListContent =
  Set.insert Option phrasingContent

descriptionDetailsContent :: Set ElementType
descriptionDetailsContent = flowContent

deletedTextAttrs :: MonadGen m => [m A.Attribute]
deletedTextAttrs =
  [ A.cite
  , A.datetime
  ]

detailsContent :: Set ElementType
detailsContent =
  Set.insert Summary flowContent

detailsAttrs :: MonadGen m => [m A.Attribute]
detailsAttrs =
  [ A.elementtiming
  , A.name
  , A.open
  ]

definitionContent :: Set ElementType
definitionContent =
  Set.delete Definition phrasingContent

dialogContent :: Set ElementType
dialogContent = flowContent

divisionContent :: Set ElementType
divisionContent = flowContent

descriptionListContent :: Set ElementType
descriptionListContent =
  Set.fromList [ DescriptionTerm, DescriptionDetails, Division ]
    <> scriptSupportingContent

descriptionTermContent :: Set ElementType
descriptionTermContent =
  Set.fromList
    [ Anchor
    , Abbreviation
    , ContactAddress
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , Blockquote
    , LineBreak
    , Button
    , Canvas
    , Citation
    , Code
    , Data
    , DataList
    , DeletedText
    , Details
    , Definition
    , Dialog
    , Division
    , DescriptionList
    , Emphasis
    , Embed
    , Fieldset
    , Figure
    , Form
    , HorizontalRule
    , IdiomaticText
    , IFrame
    , Image
    , Input
    , InsertedText
    , KeyboardInput
    , Label
    , Main
    , Map
    , Mark
    , Menu
    , Meter
    , NoScriptBody
    , Object
    , OrderedList
    , Output
    , Paragraph
    , Picture
    , PreformattedText
    , Progress
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Search
    , Script
    , Select
    , Slot
    , SideComment
    , Span
    , Strong
    , Subscript
    , Superscript
    , Table
    , ContentTemplate
    , TextArea
    , Time
    , Underline
    , UnorderedList
    , Variable
    , Video
    , WordBreakOpportunity
    ]

emphasisContent :: Set ElementType
emphasisContent = phrasingContent

embedAttrs :: MonadGen m => [m A.Attribute]
embedAttrs =
  [ A.height
  , A.src
  , A.type_
  , A.width
  ]

fieldsetAttrs :: MonadGen m => [m A.Attribute]
fieldsetAttrs =
  [ A.disabled
  , A.elementtiming
  , A.form
  , A.name
  ]

fieldsetContent :: Set ElementType
fieldsetContent =
  Set.insert Legend flowContent

figureCaptionContent :: Set ElementType
figureCaptionContent = flowContent

figureContent :: Set ElementType
figureContent =
  Set.insert FigureCaption flowContent

footerContent :: Set ElementType
footerContent = marginalContent

formAttrs :: MonadGen m => [m A.Attribute]
formAttrs =
  [ A.acceptCharset
  , A.action
  , A.autocomplete
  , A.enctype
  , A.method
  , A.name
  , A.novalidate
  , A.rel
  , A.target
  ]

formContent :: Set ElementType
formContent =
  Set.fromList
    [ Anchor
    , Abbreviation
    , ContactAddress
    , Article
    , Aside
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , Blockquote
    , LineBreak
    , Button
    , Canvas
    , Citation
    , Code
    , Data
    , DataList
    , DeletedText
    , Details
    , Definition
    , Dialog
    , Division
    , DescriptionList
    , Emphasis
    , Embed
    , Fieldset
    , Figure
    , Footer
    , H1
    , H2
    , H3
    , H4
    , H5
    , H6
    , Header
    , HeadingGroup
    , HorizontalRule
    , IdiomaticText
    , IFrame
    , Image
    , Input
    , InsertedText
    , KeyboardInput
    , Label
    , Main
    , Map
    , Mark
    , Menu
    , Meter
    , Nav
    , NoScriptBody
    , Object
    , OrderedList
    , Output
    , Paragraph
    , Picture
    , PreformattedText
    , Progress
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Search
    , Script
    , Section
    , Select
    , Slot
    , SideComment
    , Span
    , Strong
    , Subscript
    , Superscript
    , Table
    , ContentTemplate
    , TextArea
    , Time
    , Underline
    , UnorderedList
    , Variable
    , Video
    , WordBreakOpportunity
    ]

h1Content :: Set ElementType
h1Content = phrasingContent

h2Content :: Set ElementType
h2Content = phrasingContent

h3Content :: Set ElementType
h3Content = phrasingContent

h4Content :: Set ElementType
h4Content = phrasingContent

h5Content :: Set ElementType
h5Content = phrasingContent

h6Content :: Set ElementType
h6Content = phrasingContent

headContent :: Set ElementType
headContent =
  Set.fromList
    [ Base
    , Link
    , Meta
    , NoScriptHead
    , Script
    , Style
    , Title
    ]

headerContent :: Set ElementType
headerContent = marginalContent

headingGroupContent :: Set ElementType
headingGroupContent =
  Set.insert Paragraph headings

idiomaticTextContent :: Set ElementType
idiomaticTextContent = phrasingContent

iFrameAttrs :: MonadGen m => [m A.Attribute]
iFrameAttrs =
  [ A.allow
  , A.elementtiming
  , A.height
  , A.loading
  , A.name
  , A.referrerpolicy
  , A.sandbox
  , A.src
  , A.srcdoc
  , A.width
  ]

imageAttrs :: MonadGen m => [m A.Attribute]
imageAttrs =
  [ A.alt
  , A.crossorigin
  , A.decoding
  , A.elementtiming
  , A.fetchpriority
  , A.height
  , A.ismap
  , A.loading
  , A.referrerpolicy
  , A.sizes
  , A.src
  , A.srcset
  , A.usemap
  , A.width
  ]

inputAttrs :: MonadGen m => [m A.Attribute]
inputAttrs =
  [ A.accept
  , A.alt
  , A.autocomplete
  , A.capture
  , A.checked
  , A.dirname
  , A.disabled
  , A.form
  , A.formaction
  , A.formenctype
  , A.formmethod
  , A.formnovalidate
  , A.formtarget
  , A.height
  , A.list
  , A.max
  , A.maxlength
  , A.min
  , A.minlength
  , A.multiple
  , A.name
  , A.pattern
  , A.placeholder
  , A.popovertarget
  , A.popovertargetaction
  , A.readonly
  , A.required
  , A.size
  , A.src
  , A.step
  , A.type_
  , A.value
  , A.width
  ]

insertedTextAttrs :: MonadGen m => [m A.Attribute]
insertedTextAttrs =
  [ A.cite
  , A.datetime
  ]

keyboardInputContent :: Set ElementType
keyboardInputContent = phrasingContent

labelContent :: Set ElementType
labelContent =
  Set.fromList
    [ Abbreviation
    , Area
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , LineBreak
    , Button
    , Canvas
    , Citation
    , Code
    , Data
    , DataList
    , Definition
    , Emphasis
    , Embed
    , IdiomaticText
    , IFrame
    , Image
    , Input
    , KeyboardInput
    , Mark
    , Meter
    , NoScriptBody
    , Object
    , Output
    , Picture
    , Progress
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Script
    , Select
    , Slot
    , SideComment
    , Span
    , Strong
    , Subscript
    , Superscript
    , ContentTemplate
    , TextArea
    , Time
    , Underline
    , Variable
    , Video
    , WordBreakOpportunity
    ]

labelAttrs :: MonadGen m => [m A.Attribute]
labelAttrs =
  [ A.elementtiming
  , A.forLabel
  ]

legendContent :: Set ElementType
legendContent =
  headings <> phrasingContent

listItemContent :: Set ElementType
listItemContent = flowContent

listItemAttrs :: MonadGen m => [m A.Attribute]
listItemAttrs =
  [ A.elementtiming
  , A.valueInteger
  ]

linkAttrs :: MonadGen m => [m A.Attribute]
linkAttrs =
  [ A.as
  , A.blocking
  , A.crossorigin
  , A.disabled
  , A.fetchpriority
  , A.href
  , A.hreflang
  , A.imagesizes
  , A.imagesrcset
  , A.integrity
  , A.media
  , A.referrerpolicy
  , A.rel
  , A.sizes
  , A.type_
  ]

mainContent :: Set ElementType
mainContent = flowContent

mapContent :: Set ElementType
mapContent =
  Set.fromList
    [ Anchor
    , Audio
    , Canvas
    , DeletedText
    , InsertedText
    , Map
    , NoScriptBody
    , Object
    , Slot
    , Video
    ]

markContent :: Set ElementType
markContent = phrasingContent

menuContent :: Set ElementType
menuContent = listContent

metaAttrs :: MonadGen m => [m A.Attribute]
metaAttrs =
  [ A.charset
  , A.content
  , A.httpEquiv
  , A.media
  , A.nameMeta
  ]

meterAttrs :: MonadGen m => [m A.Attribute]
meterAttrs =
  [ A.form
  , A.high
  , A.low
  , A.max
  , A.min
  , A.optimum
  , A.valueNumber
  ]

meterContent :: Set ElementType
meterContent =
  Set.fromList
    [ Abbreviation
    , Area
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , LineBreak
    , Button
    , Canvas
    , Citation
    , Code
    , Data
    , DataList
    , Definition
    , Emphasis
    , Embed
    , IdiomaticText
    , IFrame
    , Image
    , Input
    , KeyboardInput
    , Label
    , Mark
    , NoScriptBody
    , Object
    , Output
    , Picture
    , Progress
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Script
    , Select
    , Slot
    , SideComment
    , Span
    , Strong
    , Subscript
    , Superscript
    , ContentTemplate
    , TextArea
    , Time
    , Underline
    , Variable
    , Video
    , WordBreakOpportunity
    ]

navContent :: Set ElementType
navContent = flowContent

noScriptHeadContent :: Set ElementType
noScriptHeadContent =
  Set.fromList
    [ Link
    , Meta
    , Style
    ]

noScriptBodyContent :: Set ElementType
noScriptBodyContent =
  Set.union flowContent phrasingContent
    `Set.difference` Set.fromList [ NoScriptHead, NoScriptBody ]

objectAttrs :: MonadGen m => [m A.Attribute]
objectAttrs =
  [ A.data_
  , A.form
  , A.height
  , A.name
  , A.type_
  , A.width
  ]

orderedListAttrs :: MonadGen m => [m A.Attribute]
orderedListAttrs =
  [ A.reversed
  , A.start
  , A.type_
  ]

orderedListContent :: Set ElementType
orderedListContent = listContent

optionGroupAttrs :: MonadGen m => [m A.Attribute]
optionGroupAttrs =
  [ A.disabled
  , A.label
  ]

optionGroupContent :: Set ElementType
optionGroupContent =
  Set.singleton Option

optionAttrs :: MonadGen m => [m A.Attribute]
optionAttrs =
  [ A.disabled
  , A.label
  , A.selected
  , A.value
  ]

outputAttrs :: MonadGen m => [m A.Attribute]
outputAttrs =
  [ A.elementtiming
  , A.forOutput
  , A.form
  , A.name
  ]

outputContent :: Set ElementType
outputContent = phrasingContent

paragraphContent :: Set ElementType
paragraphContent = phrasingContent

pictureContent :: Set ElementType
pictureContent =
  Set.fromList [ Source, Image ] <> scriptSupportingContent

preformattedTextContent :: Set ElementType
preformattedTextContent = phrasingContent

progressAttrs :: MonadGen m => [m A.Attribute]
progressAttrs =
  [ A.max
  , A.valueNumber
  ]

progressContent :: Set ElementType
progressContent =
  Set.fromList
    [ Abbreviation
    , Area
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , LineBreak
    , Button
    , Canvas
    , Citation
    , Code
    , Data
    , DataList
    , Definition
    , Emphasis
    , Embed
    , IdiomaticText
    , IFrame
    , Image
    , Input
    , KeyboardInput
    , Label
    , Mark
    , Meter
    , NoScriptBody
    , Object
    , Output
    , Picture
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Script
    , Select
    , Slot
    , SideComment
    , Span
    , Strong
    , Subscript
    , Superscript
    , ContentTemplate
    , TextArea
    , Time
    , Underline
    , Variable
    , Video
    , WordBreakOpportunity
    ]

quotationContent :: Set ElementType
quotationContent = phrasingContent

quotationAttrs :: MonadGen m => [m A.Attribute]
quotationAttrs =
  [ A.cite
  , A.elementtiming
  ]

rubyTextContent :: Set ElementType
rubyTextContent = phrasingContent

rubyContent :: Set ElementType
rubyContent =
  Set.fromList
    [ RubyParenthesis
    , RubyText
    ]

strikethroughContent :: Set ElementType
strikethroughContent = phrasingContent

sampleContent :: Set ElementType
sampleContent = phrasingContent

scriptAttrs :: MonadGen m => [m A.Attribute]
scriptAttrs =
  [ A.async
  , A.blocking
  , A.crossorigin
  , A.defer
  , A.fetchpriority
  , A.integrity
  , A.nomodule
  , A.referrerpolicy
  , A.src
  , A.type_
  ]

searchContent :: Set ElementType
searchContent = flowContent

sectionContent :: Set ElementType
sectionContent = flowContent

selectAttrs :: MonadGen m => [m A.Attribute]
selectAttrs =
  [ A.autocomplete
  , A.disabled
  , A.form
  , A.multiple
  , A.name
  , A.required
  , A.size
  ]

selectContent :: Set ElementType
selectContent =
  Set.fromList
    [ Option
    , OptionGroup
    ]

sideCommentContent :: Set ElementType
sideCommentContent = phrasingContent

sourceAttrs :: MonadGen m => [m A.Attribute]
sourceAttrs =
  [ A.height
  , A.media
  , A.sizes
  , A.src
  , A.srcset
  , A.type_
  , A.width
  ]

spanContent :: Set ElementType
spanContent = phrasingContent

strongContent :: Set ElementType
strongContent = phrasingContent

styleAttrs :: MonadGen m => [m A.Attribute]
styleAttrs =
  [ A.blocking
  , A.media
  ]

subscriptContent :: Set ElementType
subscriptContent = phrasingContent

summaryContent :: Set ElementType
summaryContent =
  Set.insert HeadingGroup headings <> phrasingContent

superscriptContent :: Set ElementType
superscriptContent = phrasingContent

tableContent :: Set ElementType
tableContent =
  Set.fromList
    [ TableCaption
    , TableColumnGroup
    , TableHead
    , TableBody
    , TableRow
    , TableFoot
    ]

tableBodyContent :: Set ElementType
tableBodyContent =
  Set.singleton TableRow

tableDataCellAttrs :: MonadGen m => [m A.Attribute]
tableDataCellAttrs =
  [ A.colspan
  , A.elementtiming
  , A.headers
  , A.rowspan
  ]

tableDataCellContent :: Set ElementType
tableDataCellContent = flowContent

contentTemplateContent :: Set ElementType
contentTemplateContent = allElements

contentTemplateAttrs :: MonadGen m => [m A.Attribute]
contentTemplateAttrs =
  [ A.shadowrootclonable
  , A.shadowrootdelegatesfocus
  , A.shadowrootmode
  ]

textAreaAttrs :: MonadGen m => [m A.Attribute]
textAreaAttrs =
  [ A.autocomplete
  , A.cols
  , A.dirname
  , A.disabled
  , A.form
  , A.maxlength
  , A.minlength
  , A.name
  , A.placeholder
  , A.readonly
  , A.required
  , A.rows
  , A.wrap
  ]

tableFootContent :: Set ElementType
tableFootContent =
  Set.singleton TableRow

tableHeaderAttrs :: MonadGen m => [m A.Attribute]
tableHeaderAttrs =
  [ A.abbr
  , A.colspan
  , A.elementtiming
  , A.headers
  , A.rowspan
  , A.scope
  ]

tableHeaderContent :: Set ElementType
tableHeaderContent =
  Set.fromList
    [ Anchor
    , Abbreviation
    , ContactAddress
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , Blockquote
    , LineBreak
    , Button
    , Canvas
    , Citation
    , Code
    , Data
    , DataList
    , DeletedText
    , Details
    , Definition
    , Dialog
    , Division
    , DescriptionList
    , Emphasis
    , Embed
    , Fieldset
    , Figure
    , Form
    , HorizontalRule
    , IdiomaticText
    , IFrame
    , Image
    , Input
    , InsertedText
    , KeyboardInput
    , Label
    , Main
    , Map
    , Mark
    , Menu
    , Meter
    , NoScriptBody
    , Object
    , OrderedList
    , Output
    , Paragraph
    , Picture
    , PreformattedText
    , Progress
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Search
    , Script
    , Select
    , Slot
    , SideComment
    , Span
    , Strong
    , Subscript
    , Superscript
    , Table
    , ContentTemplate
    , TextArea
    , Time
    , Underline
    , UnorderedList
    , Variable
    , Video
    , WordBreakOpportunity
    ]

tableHeadContent :: Set ElementType
tableHeadContent =
  Set.singleton TableRow

timeContent :: Set ElementType
timeContent = phrasingContent

timeAttrs :: MonadGen m => [m A.Attribute]
timeAttrs =
  [ A.datetime
  , A.elementtiming
  ]

tableRowContent :: Set ElementType
tableRowContent =
  Set.fromList [ TableDataCell, TableHeader ]
    <> scriptSupportingContent

trackAttrs :: MonadGen m => [m A.Attribute]
trackAttrs =
  [ A.default_
  , A.kind
  , A.label
  , A.src
  , A.srclang
  ]

underlineContent :: Set ElementType
underlineContent = phrasingContent

unorderedListContent :: Set ElementType
unorderedListContent = listContent

variableContent :: Set ElementType
variableContent = phrasingContent

videoAttrs :: MonadGen m => [m A.Attribute]
videoAttrs =
  [ A.autoplay
  , A.controls
  , A.controlslist
  , A.crossorigin
  , A.disablepictureinpicture
  , A.disableremoteplayback
  , A.elementtiming
  , A.height
  , A.loop
  , A.muted
  , A.playsinline
  , A.poster
  , A.preload
  , A.src
  , A.width
  ]

videoContent :: Set ElementType
videoContent = audioVideoContent

-- Content Categories
--

allElements :: Set ElementType
allElements =
  Set.fromList
    [ Anchor
    , Abbreviation
    , ContactAddress
    , Area
    , Article
    , Aside
    , Audio
    , BringAttentionTo
    , Base
    , BidirectionalIsolation
    , BidirectionalOverride
    , Blockquote
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
    , Header
    , HeadingGroup
    , HorizontalRule
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
    , NoScriptBody
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

audioVideoContent :: Set ElementType
audioVideoContent =
  Set.fromList
    [ Source
    , Track
    ]

flowContent :: Set ElementType
flowContent =
  Set.fromList
    [ Anchor
    , Abbreviation
    , ContactAddress
    , Article
    , Aside
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , Blockquote
    , LineBreak
    , Button
    , Canvas
    , Citation
    , Code
    , Data
    , DataList
    , DeletedText
    , Details
    , Definition
    , Dialog
    , Division
    , DescriptionList
    , Emphasis
    , Embed
    , Fieldset
    , Figure
    , Footer
    , Form
    , H1
    , H2
    , H3
    , H4
    , H5
    , H6
    , Header
    , HeadingGroup
    , HorizontalRule
    , IdiomaticText
    , IFrame
    , Image
    , Input
    , InsertedText
    , KeyboardInput
    , Label
    , Main
    , Map
    , Mark
    , Menu
    , Meter
    , Nav
    , NoScriptBody
    , Object
    , OrderedList
    , Output
    , Paragraph
    , Picture
    , PreformattedText
    , Progress
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Search
    , Script
    , Section
    , Select
    , Slot
    , SideComment
    , Span
    , Strong
    , Subscript
    , Superscript
    , Table
    , ContentTemplate
    , TextArea
    , Time
    , Underline
    , UnorderedList
    , Variable
    , Video
    , WordBreakOpportunity
    ]

headings :: Set ElementType
headings =
  Set.fromList
    [ H1
    , H2
    , H3
    , H4
    , H5
    , H6
    ]

listContent :: Set ElementType
listContent =
  Set.insert ListItem scriptSupportingContent

marginalContent :: Set ElementType
marginalContent =
  Set.fromList
    [ Anchor
    , Abbreviation
    , ContactAddress
    , Article
    , Aside
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , Blockquote
    , LineBreak
    , Button
    , Canvas
    , Citation
    , Code
    , Data
    , DataList
    , DeletedText
    , Details
    , Definition
    , Dialog
    , Division
    , DescriptionList
    , Emphasis
    , Embed
    , Fieldset
    , Figure
    , Form
    , H1
    , H2
    , H3
    , H4
    , H5
    , H6
    , HeadingGroup
    , HorizontalRule
    , IdiomaticText
    , IFrame
    , Image
    , Input
    , InsertedText
    , KeyboardInput
    , Label
    , Main
    , Map
    , Mark
    , Menu
    , Meter
    , Nav
    , NoScriptBody
    , Object
    , OrderedList
    , Output
    , Paragraph
    , Picture
    , PreformattedText
    , Progress
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Search
    , Script
    , Section
    , Select
    , Slot
    , SideComment
    , Span
    , Strong
    , Subscript
    , Superscript
    , Table
    , ContentTemplate
    , TextArea
    , Time
    , Underline
    , UnorderedList
    , Variable
    , Video
    , WordBreakOpportunity
    ]

phrasingContent :: Set ElementType
phrasingContent =
  Set.fromList
    [ Abbreviation
    , Area
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , LineBreak
    , Button
    , Canvas
    , Citation
    , Code
    , Data
    , DataList
    , Definition
    , Emphasis
    , Embed
    , IdiomaticText
    , IFrame
    , Image
    , Input
    , KeyboardInput
    , Label
    , Mark
    , Meter
    , NoScriptBody
    , Object
    , Output
    , Picture
    , Progress
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Script
    , Select
    , Slot
    , SideComment
    , Span
    , Strong
    , Subscript
    , Superscript
    , ContentTemplate
    , TextArea
    , Time
    , Underline
    , Variable
    , Video
    , WordBreakOpportunity
    ]

scriptSupportingContent :: Set ElementType
scriptSupportingContent =
  Set.fromList
    [ Script
    , ContentTemplate
    ]

-- Node Element Lists

branchElements :: Set ElementType
branchElements =
  Set.fromList
    [ Abbreviation
    , ContactAddress
    , Article
    , Aside
    , Audio
    , BringAttentionTo
    , BidirectionalIsolation
    , BidirectionalOverride
    , Blockquote
    , Body
    , Button
    , TableCaption
    , Canvas
    , Citation
    , Code
    , TableColumn
    , TableColumnGroup
    , Data
    , DataList
    , DescriptionDetails
    , Details
    , Definition
    , Dialog
    , Division
    , DescriptionList
    , DescriptionTerm
    , Emphasis
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
    , IdiomaticText
    , KeyboardInput
    , Label
    , Legend
    , ListItem
    , Main
    , Map
    , Mark
    , Menu
    , Meter
    , Nav
    , NoScriptHead
    , NoScriptBody
    , OrderedList
    , OptionGroup
    , Output
    , Paragraph
    , Picture
    , PreformattedText
    , Progress
    , Quotation
    , Ruby
    , Strikethrough
    , Sample
    , Search
    , Section
    , Select
    , SideComment
    , Span
    , Strong
    , Subscript
    , Summary
    , Superscript
    , Table
    , TableBody
    , TableDataCell
    , ContentTemplate
    , TableFoot
    , TableHeader
    , TableHead
    , Time
    , TableRow
    , Underline
    , UnorderedList
    , Variable
    , Video
    ]

leafElements :: Set ElementType
leafElements =
  Set.fromList
    [ Comment
    , Anchor
    , DeletedText
    , InsertedText
    , Object
    , Option
    , RubyParenthesis
    , RubyText
    , Script
    , Slot
    , Style
    , TextArea
    , Title
    ]

leafBranchElements :: Set ElementType
leafBranchElements =
  Set.fromList
    [ Abbreviation
    , BringAttentionTo
    , Button
    , Citation
    , Code
    , DescriptionTerm
    , Division
    , Emphasis
    , FigureCaption
    , IdiomaticText
    , KeyboardInput
    , Label
    , ListItem
    , Mark
    , NoScriptBody
    , Paragraph
    , Quotation
    , Sample
    , SideComment
    , Span
    , Strong
    , Subscript
    , Summary
    , Superscript
    , TableCaption
    , TableDataCell
    , TableHeader
    , Time
    , Underline
    , Variable
    ]

voidElements :: Set ElementType
voidElements =
  Set.fromList
    [ Area
    , Base
    , LineBreak
    , TableColumn
    , Embed
    , HorizontalRule
    , IFrame
    , Image
    , Input
    , Link
    , Meta
    , Source
    , Track
    , WordBreakOpportunity
    ]

-- Global Attributes

withGlobalAttrs :: MonadGen m
                => GeneratorParams -> ElementType -> m [A.Attribute]
withGlobalAttrs params element =
  let
    attrsRange = attributesPerNode params
    minAttrs = minRange attrsRange
    maxAttrs = maxRange attrsRange
  in
    fmap (nubOrdOn A.attributeText)
      . Gen.list (Range.linear minAttrs maxAttrs)
      . Gen.choice
      . mappend (elementValidAttrs element)
      $ [ A.accesskey
        , A.autocapitalize
        , A.autocorrect
        , A.autofocus
        , A.class_
        , A.contenteditable
        , A.customData
        , A.dir
        , A.draggable
        , A.enterkeyhint
        , A.exportparts
        , A.hidden
        , A.id
        , A.inert
        , A.inputmode
        , A.is
        , A.itemid
        , A.itemprop
        , A.itemref
        , A.itemscope
        , A.itemtype
        , A.lang
        , A.nonce
        , A.part
        , A.popover
        , A.role
        , A.slot
        , A.spellcheck
        , A.style
        , A.tabindex
        , A.title
        , A.translate
        , A.writingsuggestions
        ]
