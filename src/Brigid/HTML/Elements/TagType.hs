{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Brigid.HTML.Elements.TagType
  ( TagErrorMessage
  , TagType
      ( NoElement
      , Document
      , Comment
      , Text
      , RawHTML
      , CustomHTML
      , Anchor
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
      , InputButton
      , InputCheckbox
      , InputColor
      , InputDate
      , InputDatetimeLocal
      , InputEmail
      , InputFile
      , InputHidden
      , InputImage
      , InputMonth
      , InputNumber
      , InputPassword
      , InputRadio
      , InputRange
      , InputReset
      , InputSearch
      , InputSubmit
      , InputTel
      , InputText
      , InputTime
      , InputUrl
      , InputWeek
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
      )
  ) where

import GHC.TypeLits qualified as TypeLits

data TagType
  = NoElement
  | Document
  | Comment
  | Text
  | RawHTML
  | CustomHTML
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
  | InputButton
  | InputCheckbox
  | InputColor
  | InputDate
  | InputDatetimeLocal
  | InputEmail
  | InputFile
  | InputHidden
  | InputImage
  | InputMonth
  | InputNumber
  | InputPassword
  | InputRadio
  | InputRange
  | InputReset
  | InputSearch
  | InputSubmit
  | InputTel
  | InputText
  | InputTime
  | InputUrl
  | InputWeek
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
  | NoScript
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

type family TagErrorMessage (tag :: TagType) :: TypeLits.ErrorMessage where
  TagErrorMessage Document               = 'TypeLits.Text "Document"
  TagErrorMessage Comment                = 'TypeLits.Text "Comment"
  TagErrorMessage Text                   = 'TypeLits.Text "Text"
  TagErrorMessage RawHTML                = 'TypeLits.Text "RawHTML"
  TagErrorMessage CustomHTML             = 'TypeLits.Text "CustomHTML"
  TagErrorMessage Anchor                 = 'TypeLits.Text "Anchor (<a>)"
  TagErrorMessage Abbreviation           = 'TypeLits.Text "Abbreviation (<abbr>)"
  TagErrorMessage ContactAddress         = 'TypeLits.Text "ContactAddress (<address>)"
  TagErrorMessage Area                   = 'TypeLits.Text "Area (<area>)"
  TagErrorMessage Article                = 'TypeLits.Text "Article (<article>)"
  TagErrorMessage Aside                  = 'TypeLits.Text "Aside (<aside>)"
  TagErrorMessage Audio                  = 'TypeLits.Text "Audio (<audio>)"
  TagErrorMessage BringAttentionTo       = 'TypeLits.Text "BringAttentionTo (<b>)"
  TagErrorMessage Base                   = 'TypeLits.Text "Base (<base>)"
  TagErrorMessage BidirectionalIsolation = 'TypeLits.Text "BidirectionalIsolation (<bdi>)"
  TagErrorMessage BidirectionalOverride  = 'TypeLits.Text "BidirectionalOverride (<bdo>)"
  TagErrorMessage Blockquote             = 'TypeLits.Text "Blockquote (<blockquote>)"
  TagErrorMessage Body                   = 'TypeLits.Text "Body (<body>)"
  TagErrorMessage LineBreak              = 'TypeLits.Text "LineBreak (<br>)"
  TagErrorMessage Button                 = 'TypeLits.Text "Button (<button>)"
  TagErrorMessage Canvas                 = 'TypeLits.Text "Canvas (<canvas>)"
  TagErrorMessage TableCaption           = 'TypeLits.Text "TableCaption (<caption>)"
  TagErrorMessage Citation               = 'TypeLits.Text "Citation (<cite>)"
  TagErrorMessage Code                   = 'TypeLits.Text "Code (<code>)"
  TagErrorMessage TableColumn            = 'TypeLits.Text "TableColumn (<col>)"
  TagErrorMessage TableColumnGroup       = 'TypeLits.Text "TableColumnGroup (<colgroup>)"
  TagErrorMessage Data                   = 'TypeLits.Text "Data (<data>)"
  TagErrorMessage DataList               = 'TypeLits.Text "DataList (<datalist>)"
  TagErrorMessage DescriptionDetails     = 'TypeLits.Text "DescriptionDetails (<dd>)"
  TagErrorMessage DeletedText            = 'TypeLits.Text "DeletedText (<del>)"
  TagErrorMessage Details                = 'TypeLits.Text "Details (<details>)"
  TagErrorMessage Definition             = 'TypeLits.Text "Definition (<dfn>)"
  TagErrorMessage Dialog                 = 'TypeLits.Text "Dialog (<dialog>)"
  TagErrorMessage Division               = 'TypeLits.Text "Division (<div>)"
  TagErrorMessage DescriptionList        = 'TypeLits.Text "DescriptionList (<dl>)"
  TagErrorMessage DescriptionTerm        = 'TypeLits.Text "DescriptionTerm (<dt>)"
  TagErrorMessage Emphasis               = 'TypeLits.Text "Emphasis (<em>)"
  TagErrorMessage Embed                  = 'TypeLits.Text "Embed (<embed>)"
  TagErrorMessage Fieldset               = 'TypeLits.Text "Fieldset (<fieldset>)"
  TagErrorMessage FigureCaption          = 'TypeLits.Text "FigureCaption (<figcaption>)"
  TagErrorMessage Figure                 = 'TypeLits.Text "Figure (<figure>)"
  TagErrorMessage Footer                 = 'TypeLits.Text "Footer (<footer>)"
  TagErrorMessage Form                   = 'TypeLits.Text "Form (<form>)"
  TagErrorMessage H1                     = 'TypeLits.Text "H1 (<h1>)"
  TagErrorMessage H2                     = 'TypeLits.Text "H2 (<h2>)"
  TagErrorMessage H3                     = 'TypeLits.Text "H3 (<h3>)"
  TagErrorMessage H4                     = 'TypeLits.Text "H4 (<h4>)"
  TagErrorMessage H5                     = 'TypeLits.Text "H5 (<h5>)"
  TagErrorMessage H6                     = 'TypeLits.Text "H6 (<h6>)"
  TagErrorMessage Head                   = 'TypeLits.Text "Head (<head>)"
  TagErrorMessage Header                 = 'TypeLits.Text "Header (<header>)"
  TagErrorMessage HeadingGroup           = 'TypeLits.Text "HeadingGroup (<hgroup>)"
  TagErrorMessage HorizontalRule         = 'TypeLits.Text "HorizontalRule (<hr>)"
  TagErrorMessage Html                   = 'TypeLits.Text "Html (<html>)"
  TagErrorMessage IdiomaticText          = 'TypeLits.Text "IdiomaticText (<i>)"
  TagErrorMessage IFrame                 = 'TypeLits.Text "IFrame (<iframe>)"
  TagErrorMessage Image                  = 'TypeLits.Text "Image (<img>)"
  TagErrorMessage Input                  = 'TypeLits.Text "Input (<input>)"
  TagErrorMessage InputButton            = 'TypeLits.Text "Button Input (<input type=\"button\">)"
  TagErrorMessage InputCheckbox          = 'TypeLits.Text "Checkbox Input (<input type=\"checkbox\">)"
  TagErrorMessage InputColor             = 'TypeLits.Text "Color Input (<input type=\"color\">)"
  TagErrorMessage InputDate              = 'TypeLits.Text "Date Input (<input type=\"date\">)"
  TagErrorMessage InputDatetimeLocal     = 'TypeLits.Text "Datetime Local Input (<input type=\"datetime-local\">)"
  TagErrorMessage InputEmail             = 'TypeLits.Text "Email Input (<input type=\"email\">)"
  TagErrorMessage InputFile              = 'TypeLits.Text "File Input (<input type=\"file\">)"
  TagErrorMessage InputHidden            = 'TypeLits.Text "Hidden Input (<input type=\"hidden\">)"
  TagErrorMessage InputImage             = 'TypeLits.Text "Image Input (<input type=\"image\">)"
  TagErrorMessage InputMonth             = 'TypeLits.Text "Month Input (<input type=\"month\">)"
  TagErrorMessage InputNumber            = 'TypeLits.Text "Number Input (<input type=\"number\">)"
  TagErrorMessage InputPassword          = 'TypeLits.Text "Password Input (<input type=\"password\">)"
  TagErrorMessage InputRadio             = 'TypeLits.Text "Radio Input (<input type=\"radio\">)"
  TagErrorMessage InputRange             = 'TypeLits.Text "Range Input (<input type=\"range\">)"
  TagErrorMessage InputReset             = 'TypeLits.Text "Reset Input (<input type=\"reset\">)"
  TagErrorMessage InputSearch            = 'TypeLits.Text "Search Input (<input type=\"search\">)"
  TagErrorMessage InputSubmit            = 'TypeLits.Text "Submit Input (<input type=\"submit\">)"
  TagErrorMessage InputTel               = 'TypeLits.Text "Telephone Number Input (<input type=\"tel\">)"
  TagErrorMessage InputText              = 'TypeLits.Text "Text Input (<input type=\"text\">)"
  TagErrorMessage InputTime              = 'TypeLits.Text "Time Input (<input type=\"time\">)"
  TagErrorMessage InputUrl               = 'TypeLits.Text "URL Input (<input type=\"url\">)"
  TagErrorMessage InputWeek              = 'TypeLits.Text "Week Input (<input type=\"week\">)"
  TagErrorMessage InsertedText           = 'TypeLits.Text "InsertedText (<ins>)"
  TagErrorMessage KeyboardInput          = 'TypeLits.Text "KeyboardInput (<kbd>)"
  TagErrorMessage Label                  = 'TypeLits.Text "Label (<label>)"
  TagErrorMessage Legend                 = 'TypeLits.Text "Legend (<legend>)"
  TagErrorMessage ListItem               = 'TypeLits.Text "ListItem (<li>)"
  TagErrorMessage Link                   = 'TypeLits.Text "Link (<link>)"
  TagErrorMessage Main                   = 'TypeLits.Text "Main (<main>)"
  TagErrorMessage Map                    = 'TypeLits.Text "Map (<map>)"
  TagErrorMessage Mark                   = 'TypeLits.Text "Mark (<mark>)"
  TagErrorMessage Menu                   = 'TypeLits.Text "Menu (<menu>)"
  TagErrorMessage Meta                   = 'TypeLits.Text "Meta (<meta>)"
  TagErrorMessage Meter                  = 'TypeLits.Text "Meter (<metere>)"
  TagErrorMessage Nav                    = 'TypeLits.Text "Nav (<nav>)"
  TagErrorMessage NoScript               = 'TypeLits.Text "NoScript (<noscript>)"
  TagErrorMessage Object                 = 'TypeLits.Text "Object (<object>)"
  TagErrorMessage OrderedList            = 'TypeLits.Text "OrderedList (<ol>)"
  TagErrorMessage OptionGroup            = 'TypeLits.Text "OptionGroup (<optgroup>)"
  TagErrorMessage Option                 = 'TypeLits.Text "Option (<option>)"
  TagErrorMessage Output                 = 'TypeLits.Text "Output (<output>)"
  TagErrorMessage Paragraph              = 'TypeLits.Text "Paragraph (<p>)"
  TagErrorMessage Picture                = 'TypeLits.Text "Picture (<picture>)"
  TagErrorMessage PreformattedText       = 'TypeLits.Text "PreformattedText (<pre>)"
  TagErrorMessage Progress               = 'TypeLits.Text "Progress (<progress>)"
  TagErrorMessage Quotation              = 'TypeLits.Text "Quotation (<q>)"
  TagErrorMessage RubyParenthesis        = 'TypeLits.Text "RubyParenthesis (<rp>)"
  TagErrorMessage RubyText               = 'TypeLits.Text "RubyText (<rt>)"
  TagErrorMessage Ruby                   = 'TypeLits.Text "Ruby (<ruby>)"
  TagErrorMessage Strikethrough          = 'TypeLits.Text "Strikethrough (<s>)"
  TagErrorMessage Sample                 = 'TypeLits.Text "Sample (<samp>)"
  TagErrorMessage Script                 = 'TypeLits.Text "Script (<script>)"
  TagErrorMessage Search                 = 'TypeLits.Text "Search (<search>)"
  TagErrorMessage Section                = 'TypeLits.Text "Section (<section>)"
  TagErrorMessage Select                 = 'TypeLits.Text "Select (<select>)"
  TagErrorMessage Slot                   = 'TypeLits.Text "Slot (<slot>)"
  TagErrorMessage SideComment            = 'TypeLits.Text "SideComment (<small>)"
  TagErrorMessage Source                 = 'TypeLits.Text "Source (<source>)"
  TagErrorMessage Span                   = 'TypeLits.Text "Span (<span>)"
  TagErrorMessage Strong                 = 'TypeLits.Text "Strong (<strong>)"
  TagErrorMessage Style                  = 'TypeLits.Text "Style (<style>)"
  TagErrorMessage Subscript              = 'TypeLits.Text "Subscript (<sub>)"
  TagErrorMessage Summary                = 'TypeLits.Text "Summary (<summary>)"
  TagErrorMessage Superscript            = 'TypeLits.Text "Superscript (<sup>)"
  TagErrorMessage Table                  = 'TypeLits.Text "Table (<table>)"
  TagErrorMessage TableBody              = 'TypeLits.Text "TableBody (<tbody>)"
  TagErrorMessage TableDataCell          = 'TypeLits.Text "TableDataCell (<td>)"
  TagErrorMessage ContentTemplate        = 'TypeLits.Text "ContentTemplate (<template>)"
  TagErrorMessage TextArea               = 'TypeLits.Text "TextArea (<textarea>)"
  TagErrorMessage TableFoot              = 'TypeLits.Text "TableFoot (<tfoot>)"
  TagErrorMessage TableHeader            = 'TypeLits.Text "TableHeader (<th>)"
  TagErrorMessage TableHead              = 'TypeLits.Text "TableHead (<thead>)"
  TagErrorMessage Time                   = 'TypeLits.Text "Time (<time>)"
  TagErrorMessage Title                  = 'TypeLits.Text "Title (<title>)"
  TagErrorMessage TableRow               = 'TypeLits.Text "TableRow (<tr>)"
  TagErrorMessage Track                  = 'TypeLits.Text "Track (<track>)"
  TagErrorMessage Underline              = 'TypeLits.Text "Underline (<u>)"
  TagErrorMessage UnorderedList          = 'TypeLits.Text "UnorderedList (<ul>)"
  TagErrorMessage Variable               = 'TypeLits.Text "Variable (<var>)"
  TagErrorMessage Video                  = 'TypeLits.Text "Video (<video>)"
  TagErrorMessage WordBreakOpportunity   = 'TypeLits.Text "WordBreakOpportunity (<wbr>)"
