{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module HTML.Render.Elements
  ( tests
  ) where

import Data.NonEmptyText qualified as NET
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.Safe qualified as Safe
import Brigid.HTML.Render.ByteString qualified as RBS
import Brigid.HTML.Render.Text qualified as RT
import Brigid.Types (Id (..))

tests :: TestTree
tests =
  testGroup "HTML element rendering"
    [ contentNodes
    , bodyElements
    , htmlElements
    , headElements
    , tableElements
    , constrainedElements
    , mediaElements
    , attributeSpacingTests
    , nestedContentTests
    ]

contentNodes :: TestTree
contentNodes =
  testGroup "Content nodes"
    [ testCase "noElement" $ do
        let node = E.noElement :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= ""
        RT.renderHTML  node @?= ""

    , testCase "comment" $ do
        let node = E.comment "hello" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<!-- hello -->"
        RT.renderHTML  node @?= "<!-- hello -->"

    , testCase "text" $ do
        let node = E.text "hello" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "hello"
        RT.renderHTML  node @?= "hello"

    , testCase "rawHTML" $ do
        let node = E.rawHTML "<b>bold</b>" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<b>bold</b>"
        RT.renderHTML  node @?= "<b>bold</b>"
    ]

bodyElements :: TestTree
bodyElements =
  testGroup "Flow content elements"
    [ testCase "a" $ do
        let node = E.a [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<a></a>"
        RT.renderHTML  node @?= "<a></a>"

    , testCase "abbr" $ do
        let node = E.abbr [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<abbr></abbr>"
        RT.renderHTML  node @?= "<abbr></abbr>"

    , testCase "address" $ do
        let node = E.address [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<address></address>"
        RT.renderHTML  node @?= "<address></address>"

    , testCase "article" $ do
        let node = E.article [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<article></article>"
        RT.renderHTML  node @?= "<article></article>"

    , testCase "aside" $ do
        let node = E.aside [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<aside></aside>"
        RT.renderHTML  node @?= "<aside></aside>"

    , testCase "audio" $ do
        let node = E.audio [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio></audio>"
        RT.renderHTML  node @?= "<audio></audio>"

    , testCase "b" $ do
        let node = E.b [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<b></b>"
        RT.renderHTML  node @?= "<b></b>"

    , testCase "bdi" $ do
        let node = E.bdi [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<bdi></bdi>"
        RT.renderHTML  node @?= "<bdi></bdi>"

    , testCase "bdo" $ do
        let node = E.bdo [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<bdo></bdo>"
        RT.renderHTML  node @?= "<bdo></bdo>"

    , testCase "blockquote" $ do
        let node = E.blockquote [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<blockquote></blockquote>"
        RT.renderHTML  node @?= "<blockquote></blockquote>"

    , testCase "br" $ do
        let node = E.br [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<br/>"
        RT.renderHTML  node @?= "<br/>"

    , testCase "button" $ do
        let node = E.button [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button></button>"
        RT.renderHTML  node @?= "<button></button>"

    , testCase "canvas" $ do
        let node = E.canvas [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<canvas></canvas>"
        RT.renderHTML  node @?= "<canvas></canvas>"

    , testCase "cite" $ do
        let node = E.cite [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<cite></cite>"
        RT.renderHTML  node @?= "<cite></cite>"

    , testCase "code" $ do
        let node = E.code [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<code></code>"
        RT.renderHTML  node @?= "<code></code>"

    , testCase "data" $ do
        let node = E.data_ [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<data></data>"
        RT.renderHTML  node @?= "<data></data>"

    , testCase "datalist" $ do
        let node = E.datalist [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<datalist></datalist>"
        RT.renderHTML  node @?= "<datalist></datalist>"

    , testCase "del" $ do
        let node = E.del [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<del></del>"
        RT.renderHTML  node @?= "<del></del>"

    , testCase "details" $ do
        let node = E.details [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<details></details>"
        RT.renderHTML  node @?= "<details></details>"

    , testCase "dfn" $ do
        let node = E.dfn [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<dfn></dfn>"
        RT.renderHTML  node @?= "<dfn></dfn>"

    , testCase "dialog" $ do
        let node = E.dialog [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<dialog></dialog>"
        RT.renderHTML  node @?= "<dialog></dialog>"

    , testCase "div" $ do
        let node = E.div [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<div></div>"
        RT.renderHTML  node @?= "<div></div>"

    , testCase "dl" $ do
        let node = E.dl [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<dl></dl>"
        RT.renderHTML  node @?= "<dl></dl>"

    , testCase "em" $ do
        let node = E.em [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<em></em>"
        RT.renderHTML  node @?= "<em></em>"

    , testCase "embed" $ do
        let node = E.embed [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<embed/>"
        RT.renderHTML  node @?= "<embed/>"

    , testCase "fieldset" $ do
        let node = E.fieldset [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<fieldset></fieldset>"
        RT.renderHTML  node @?= "<fieldset></fieldset>"

    , testCase "figure" $ do
        let node = E.figure [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<figure></figure>"
        RT.renderHTML  node @?= "<figure></figure>"

    , testCase "footer" $ do
        let node = E.footer [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<footer></footer>"
        RT.renderHTML  node @?= "<footer></footer>"

    , testCase "form" $ do
        let node = E.form [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<form></form>"
        RT.renderHTML  node @?= "<form></form>"

    , testCase "h1" $ do
        let node = E.h1 [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<h1></h1>"
        RT.renderHTML  node @?= "<h1></h1>"

    , testCase "h2" $ do
        let node = E.h2 [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<h2></h2>"
        RT.renderHTML  node @?= "<h2></h2>"

    , testCase "h3" $ do
        let node = E.h3 [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<h3></h3>"
        RT.renderHTML  node @?= "<h3></h3>"

    , testCase "h4" $ do
        let node = E.h4 [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<h4></h4>"
        RT.renderHTML  node @?= "<h4></h4>"

    , testCase "h5" $ do
        let node = E.h5 [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<h5></h5>"
        RT.renderHTML  node @?= "<h5></h5>"

    , testCase "h6" $ do
        let node = E.h6 [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<h6></h6>"
        RT.renderHTML  node @?= "<h6></h6>"

    , testCase "header" $ do
        let node = E.header [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<header></header>"
        RT.renderHTML  node @?= "<header></header>"

    , testCase "hgroup" $ do
        let node = E.hgroup [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<hgroup></hgroup>"
        RT.renderHTML  node @?= "<hgroup></hgroup>"

    , testCase "hr" $ do
        let node = E.hr [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<hr/>"
        RT.renderHTML  node @?= "<hr/>"

    , testCase "i" $ do
        let node = E.i [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<i></i>"
        RT.renderHTML  node @?= "<i></i>"

    , testCase "iframe" $ do
        let node = E.iframe [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<iframe></iframe>"
        RT.renderHTML  node @?= "<iframe></iframe>"

    , testCase "img" $ do
        let node = E.img [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img/>"
        RT.renderHTML  node @?= "<img/>"

    , testCase "input" $ do
        let node = E.input [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input/>"
        RT.renderHTML  node @?= "<input/>"

    , testCase "input type=button" $ do
        let node = Safe.button [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"button\"/>"
        RT.renderHTML  node @?= "<input type=\"button\"/>"

    , testCase "input type=checkbox" $ do
        let node = Safe.checkbox [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"checkbox\"/>"
        RT.renderHTML  node @?= "<input type=\"checkbox\"/>"

    , testCase "input type=color" $ do
        let node = Safe.color [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"color\"/>"
        RT.renderHTML  node @?= "<input type=\"color\"/>"

    , testCase "input type=date" $ do
        let node = Safe.date [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"date\"/>"
        RT.renderHTML  node @?= "<input type=\"date\"/>"

    , testCase "input type=datetime-local" $ do
        let node = Safe.datetimeLocal [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"datetime-local\"/>"
        RT.renderHTML  node @?= "<input type=\"datetime-local\"/>"

    , testCase "input type=email" $ do
        let node = Safe.email [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"email\"/>"
        RT.renderHTML  node @?= "<input type=\"email\"/>"

    , testCase "input type=file" $ do
        let node = Safe.file [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"file\"/>"
        RT.renderHTML  node @?= "<input type=\"file\"/>"

    , testCase "input type=hidden" $ do
        let node = Safe.hidden [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"hidden\"/>"
        RT.renderHTML  node @?= "<input type=\"hidden\"/>"

    , testCase "input type=image" $ do
        let node = Safe.image [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"image\"/>"
        RT.renderHTML  node @?= "<input type=\"image\"/>"

    , testCase "input type=month" $ do
        let node = Safe.month [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"month\"/>"
        RT.renderHTML  node @?= "<input type=\"month\"/>"

    , testCase "input type=number" $ do
        let node = Safe.number [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"number\"/>"
        RT.renderHTML  node @?= "<input type=\"number\"/>"

    , testCase "input type=password" $ do
        let node = Safe.password [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"password\"/>"
        RT.renderHTML  node @?= "<input type=\"password\"/>"

    , testCase "input type=radio" $ do
        let node = Safe.radio [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"radio\"/>"
        RT.renderHTML  node @?= "<input type=\"radio\"/>"

    , testCase "input type=range" $ do
        let node = Safe.range [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"range\"/>"
        RT.renderHTML  node @?= "<input type=\"range\"/>"

    , testCase "input type=reset" $ do
        let node = Safe.reset [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"reset\"/>"
        RT.renderHTML  node @?= "<input type=\"reset\"/>"

    , testCase "input type=search" $ do
        let node = Safe.search [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"search\"/>"
        RT.renderHTML  node @?= "<input type=\"search\"/>"

    , testCase "input type=submit" $ do
        let node = Safe.submit [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"submit\"/>"
        RT.renderHTML  node @?= "<input type=\"submit\"/>"

    , testCase "input type=tel" $ do
        let node = Safe.tel [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"tel\"/>"
        RT.renderHTML  node @?= "<input type=\"tel\"/>"

    , testCase "input type=text" $ do
        let node = Safe.text [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"text\"/>"
        RT.renderHTML  node @?= "<input type=\"text\"/>"

    , testCase "input type=time" $ do
        let node = Safe.time [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"time\"/>"
        RT.renderHTML  node @?= "<input type=\"time\"/>"

    , testCase "input type=url" $ do
        let node = Safe.url [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"url\"/>"
        RT.renderHTML  node @?= "<input type=\"url\"/>"

    , testCase "input type=week" $ do
        let node = Safe.week [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"week\"/>"
        RT.renderHTML  node @?= "<input type=\"week\"/>"

    , testCase "ins" $ do
        let node = E.ins [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<ins></ins>"
        RT.renderHTML  node @?= "<ins></ins>"

    , testCase "kbd" $ do
        let node = E.kbd [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<kbd></kbd>"
        RT.renderHTML  node @?= "<kbd></kbd>"

    , testCase "label" $ do
        let node = E.label [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<label></label>"
        RT.renderHTML  node @?= "<label></label>"

    , testCase "main" $ do
        let node = E.main [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<main></main>"
        RT.renderHTML  node @?= "<main></main>"

    , testCase "map" $ do
        let node = E.map [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<map></map>"
        RT.renderHTML  node @?= "<map></map>"

    , testCase "mark" $ do
        let node = E.mark [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<mark></mark>"
        RT.renderHTML  node @?= "<mark></mark>"

    , testCase "menu" $ do
        let node = E.menu [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<menu></menu>"
        RT.renderHTML  node @?= "<menu></menu>"

    , testCase "meter" $ do
        let node = E.meter [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<meter></meter>"
        RT.renderHTML  node @?= "<meter></meter>"

    , testCase "nav" $ do
        let node = E.nav [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<nav></nav>"
        RT.renderHTML  node @?= "<nav></nav>"

    , testCase "noscript" $ do
        let node = E.noscript [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<noscript></noscript>"
        RT.renderHTML  node @?= "<noscript></noscript>"

    , testCase "object" $ do
        let node = E.object [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<object></object>"
        RT.renderHTML  node @?= "<object></object>"

    , testCase "ol" $ do
        let node = E.ol [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<ol></ol>"
        RT.renderHTML  node @?= "<ol></ol>"

    , testCase "output" $ do
        let node = E.output [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<output></output>"
        RT.renderHTML  node @?= "<output></output>"

    , testCase "p" $ do
        let node = E.p [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<p></p>"
        RT.renderHTML  node @?= "<p></p>"

    , testCase "picture" $ do
        let node = E.picture [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<picture></picture>"
        RT.renderHTML  node @?= "<picture></picture>"

    , testCase "pre" $ do
        let node = E.pre [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<pre></pre>"
        RT.renderHTML  node @?= "<pre></pre>"

    , testCase "progress" $ do
        let node = E.progress [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<progress></progress>"
        RT.renderHTML  node @?= "<progress></progress>"

    , testCase "q" $ do
        let node = E.q [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<q></q>"
        RT.renderHTML  node @?= "<q></q>"

    , testCase "ruby" $ do
        let node = E.ruby [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<ruby></ruby>"
        RT.renderHTML  node @?= "<ruby></ruby>"

    , testCase "s" $ do
        let node = E.s [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<s></s>"
        RT.renderHTML  node @?= "<s></s>"

    , testCase "samp" $ do
        let node = E.samp [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<samp></samp>"
        RT.renderHTML  node @?= "<samp></samp>"

    , testCase "script" $ do
        let node = E.script [] Nothing :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<script></script>"
        RT.renderHTML  node @?= "<script></script>"

    , testCase "search" $ do
        let node = E.search [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<search></search>"
        RT.renderHTML  node @?= "<search></search>"

    , testCase "section" $ do
        let node = E.section [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<section></section>"
        RT.renderHTML  node @?= "<section></section>"

    , testCase "select" $ do
        let node = E.select [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<select></select>"
        RT.renderHTML  node @?= "<select></select>"

    , testCase "slot" $ do
        let node = E.slot [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<slot></slot>"
        RT.renderHTML  node @?= "<slot></slot>"

    , testCase "small" $ do
        let node = E.small [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<small></small>"
        RT.renderHTML  node @?= "<small></small>"

    , testCase "span" $ do
        let node = E.span [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<span></span>"
        RT.renderHTML  node @?= "<span></span>"

    , testCase "strong" $ do
        let node = E.strong [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<strong></strong>"
        RT.renderHTML  node @?= "<strong></strong>"

    , testCase "sub" $ do
        let node = E.sub [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<sub></sub>"
        RT.renderHTML  node @?= "<sub></sub>"

    , testCase "sup" $ do
        let node = E.sup [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<sup></sup>"
        RT.renderHTML  node @?= "<sup></sup>"

    , testCase "table" $ do
        let node = E.table [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<table></table>"
        RT.renderHTML  node @?= "<table></table>"

    , testCase "template" $ do
        let node = E.template [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<template></template>"
        RT.renderHTML  node @?= "<template></template>"

    , testCase "textarea" $ do
        let node = E.textarea [] "" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<textarea></textarea>"
        RT.renderHTML  node @?= "<textarea></textarea>"

    , testCase "time" $ do
        let node = E.time [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<time></time>"
        RT.renderHTML  node @?= "<time></time>"

    , testCase "u" $ do
        let node = E.u [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<u></u>"
        RT.renderHTML  node @?= "<u></u>"

    , testCase "ul" $ do
        let node = E.ul [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<ul></ul>"
        RT.renderHTML  node @?= "<ul></ul>"

    , testCase "var" $ do
        let node = E.var [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<var></var>"
        RT.renderHTML  node @?= "<var></var>"

    , testCase "video" $ do
        let node = E.video [] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<video></video>"
        RT.renderHTML  node @?= "<video></video>"

    , testCase "wbr" $ do
        let node = E.wbr [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<wbr/>"
        RT.renderHTML  node @?= "<wbr/>"
    ]

htmlElements :: TestTree
htmlElements =
  testGroup "Html-level elements"
    [ testCase "html" $ do
        let node = E.html [] [] :: E.HTML
        RBS.renderHTML node @?= "<!DOCTYPE html><html></html>"
        RT.renderHTML  node @?= "<!DOCTYPE html><html></html>"

    , testCase "head" $ do
        let node = E.head [] [] :: E.ChildHTML E.Html E.NoElement
        RBS.renderHTML node @?= "<head></head>"
        RT.renderHTML  node @?= "<head></head>"

    , testCase "body" $ do
        let node = E.body [] [] :: E.ChildHTML E.Html E.NoElement
        RBS.renderHTML node @?= "<body></body>"
        RT.renderHTML  node @?= "<body></body>"
    ]

headElements :: TestTree
headElements =
  testGroup "Metadata content elements"
    [ testCase "base" $ do
        let node = E.base [] :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<base/>"
        RT.renderHTML  node @?= "<base/>"

    , testCase "link" $ do
        let node = E.link [] :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<link/>"
        RT.renderHTML  node @?= "<link/>"

    , testCase "meta" $ do
        let node = E.meta [] :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<meta/>"
        RT.renderHTML  node @?= "<meta/>"

    , testCase "style" $ do
        let node = E.style [] "" :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<style></style>"
        RT.renderHTML  node @?= "<style></style>"

    , testCase "title" $ do
        let node = E.title [] [] :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<title></title>"
        RT.renderHTML  node @?= "<title></title>"
    ]

tableElements :: TestTree
tableElements =
  testGroup "Table structure elements"
    [ testCase "caption" $ do
        let node = E.caption [] [] :: E.ChildHTML E.Table E.Body
        RBS.renderHTML node @?= "<caption></caption>"
        RT.renderHTML  node @?= "<caption></caption>"

    , testCase "colgroup" $ do
        let node = E.colgroup [] [] :: E.ChildHTML E.Table E.Body
        RBS.renderHTML node @?= "<colgroup></colgroup>"
        RT.renderHTML  node @?= "<colgroup></colgroup>"

    , testCase "col" $ do
        let node = E.col [] :: E.ChildHTML E.TableColumnGroup E.Table
        RBS.renderHTML node @?= "<col/>"
        RT.renderHTML  node @?= "<col/>"

    , testCase "thead" $ do
        let node = E.thead [] [] :: E.ChildHTML E.Table E.Body
        RBS.renderHTML node @?= "<thead></thead>"
        RT.renderHTML  node @?= "<thead></thead>"

    , testCase "tbody" $ do
        let node = E.tbody [] [] :: E.ChildHTML E.Table E.Body
        RBS.renderHTML node @?= "<tbody></tbody>"
        RT.renderHTML  node @?= "<tbody></tbody>"

    , testCase "tfoot" $ do
        let node = E.tfoot [] [] :: E.ChildHTML E.Table E.Body
        RBS.renderHTML node @?= "<tfoot></tfoot>"
        RT.renderHTML  node @?= "<tfoot></tfoot>"

    , testCase "tr" $ do
        let node = E.tr [] [] :: E.ChildHTML E.TableBody E.Table
        RBS.renderHTML node @?= "<tr></tr>"
        RT.renderHTML  node @?= "<tr></tr>"

    , testCase "td" $ do
        let node = E.td [] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<td></td>"
        RT.renderHTML  node @?= "<td></td>"

    , testCase "th" $ do
        let node = E.th [] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<th></th>"
        RT.renderHTML  node @?= "<th></th>"
    ]

constrainedElements :: TestTree
constrainedElements =
  testGroup "Elements with specific parent requirements"
    [ testCase "area (in paragraph)" $ do
        let node = E.area [] :: E.ChildHTML E.Paragraph E.Body
        RBS.renderHTML node @?= "<area/>"
        RT.renderHTML  node @?= "<area/>"

    , testCase "dd (in dl)" $ do
        let node = E.dd [] [] :: E.ChildHTML E.DescriptionList E.Body
        RBS.renderHTML node @?= "<dd></dd>"
        RT.renderHTML  node @?= "<dd></dd>"

    , testCase "dt (in dl)" $ do
        let node = E.dt [] [] :: E.ChildHTML E.DescriptionList E.Body
        RBS.renderHTML node @?= "<dt></dt>"
        RT.renderHTML  node @?= "<dt></dt>"

    , testCase "figcaption (in figure)" $ do
        let node = E.figcaption [] [] :: E.ChildHTML E.Figure E.Body
        RBS.renderHTML node @?= "<figcaption></figcaption>"
        RT.renderHTML  node @?= "<figcaption></figcaption>"

    , testCase "legend (in fieldset)" $ do
        let node = E.legend [] [] :: E.ChildHTML E.Fieldset E.Body
        RBS.renderHTML node @?= "<legend></legend>"
        RT.renderHTML  node @?= "<legend></legend>"

    , testCase "li (in ol)" $ do
        let node = E.li [] [] :: E.ChildHTML E.OrderedList E.Body
        RBS.renderHTML node @?= "<li></li>"
        RT.renderHTML  node @?= "<li></li>"

    , testCase "optgroup (in select)" $ do
        let node = E.optgroup [] [] :: E.ChildHTML E.Select E.Body
        RBS.renderHTML node @?= "<optgroup></optgroup>"
        RT.renderHTML  node @?= "<optgroup></optgroup>"

    , testCase "option (in select)" $ do
        let node = E.option [] [] :: E.ChildHTML E.Select E.Body
        RBS.renderHTML node @?= "<option></option>"
        RT.renderHTML  node @?= "<option></option>"

    , testCase "rp (in ruby)" $ do
        let node = E.rp [] [] :: E.ChildHTML E.Ruby E.Body
        RBS.renderHTML node @?= "<rp></rp>"
        RT.renderHTML  node @?= "<rp></rp>"

    , testCase "rt (in ruby)" $ do
        let node = E.rt [] [] :: E.ChildHTML E.Ruby E.Body
        RBS.renderHTML node @?= "<rt></rt>"
        RT.renderHTML  node @?= "<rt></rt>"

    , testCase "summary (in details)" $ do
        let node = E.summary [] [] :: E.ChildHTML E.Details E.Body
        RBS.renderHTML node @?= "<summary></summary>"
        RT.renderHTML  node @?= "<summary></summary>"
    ]

mediaElements :: TestTree
mediaElements =
  testGroup "Media child elements"
    [ testCase "source" $ do
        let node = E.source [] :: E.ChildHTML E.Audio E.Body
        RBS.renderHTML node @?= "<source/>"
        RT.renderHTML  node @?= "<source/>"

    , testCase "track" $ do
        let node = E.track [] :: E.ChildHTML E.Audio E.Body
        RBS.renderHTML node @?= "<track/>"
        RT.renderHTML  node @?= "<track/>"
    ]

attributeSpacingTests :: TestTree
attributeSpacingTests =
  testGroup "Attribute spacing"
    [ testCase "void element with two attributes" $ do
        let node = E.br [ A.id (Id (NET.singleton 'x'))
                        , A.class_ "foo"
                        ] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<br id=\"x\" class=\"foo\"/>"
        RT.renderHTML  node @?= "<br id=\"x\" class=\"foo\"/>"

    , testCase "content element with two attributes" $ do
        let node = E.div [ A.id (Id (NET.singleton 'x'))
                         , A.class_ "foo"
                         ] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<div id=\"x\" class=\"foo\"></div>"
        RT.renderHTML  node @?= "<div id=\"x\" class=\"foo\"></div>"

    , testCase "constrained element with two attributes" $ do
        let node = E.td [ A.id (Id (NET.singleton 'x'))
                        , A.class_ "foo"
                        ] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<td id=\"x\" class=\"foo\"></td>"
        RT.renderHTML  node @?= "<td id=\"x\" class=\"foo\"></td>"
    ]

nestedContentTests :: TestTree
nestedContentTests =
  testGroup "Nested content"
    [ testCase "element with text child" $ do
        let node = E.p [] [E.text "hello"] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<p>hello</p>"
        RT.renderHTML  node @?= "<p>hello</p>"

    , testCase "nested elements" $ do
        let node = E.div []
                     [ E.p [] [E.text "first"]
                     , E.p [] [E.text "second"]
                     ] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<div><p>first</p><p>second</p></div>"
        RT.renderHTML  node @?= "<div><p>first</p><p>second</p></div>"

    , testCase "deeply nested elements" $ do
        let node = E.div []
                     [ E.ul []
                         [ E.li [] [E.span [] [E.text "item"]]
                         ]
                     ] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<div><ul><li><span>item</span></li></ul></div>"
        RT.renderHTML  node @?= "<div><ul><li><span>item</span></li></ul></div>"

    , testCase "text content is escaped" $ do
        let node = E.p [] [E.text "<b>not bold</b>"] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<p>&#60;b&#62;not bold&#60;/b&#62;</p>"
        RT.renderHTML  node @?= "<p>&#60;b&#62;not bold&#60;/b&#62;</p>"

    , testCase "attribute value is escaped" $ do
        let node = E.div [A.class_ "a\"b"] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<div class=\"a&#34;b\"></div>"
        RT.renderHTML  node @?= "<div class=\"a&#34;b\"></div>"
    ]
