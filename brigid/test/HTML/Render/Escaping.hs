{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Regression tests for attribute-value escaping and attribute-name safety
-- in the HTML renderers. Each check renders through the ByteString, Text and
-- String renderers and requires all three to agree. It then compares the
-- exact serialized output, and uses "Escaping.Oracle" to confirm that parsing
-- yields exactly the intended attributes with their original values.
module HTML.Render.Escaping
  ( tests
  ) where

import Control.Monad (forM_)
import Data.ByteString.Lazy qualified as LBS
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.SVG qualified as SVG
import Brigid.HTML.Render.ByteString qualified as RBS
import Brigid.HTML.Render.String qualified as RS
import Brigid.HTML.Render.Text qualified as RT
import Brigid.HTML.Types qualified as HTML
import Brigid.Types (RawJavaScript (..))
import Brigid.Types qualified as Types

import Escaping.Oracle (Mode (HTMLMode), allAttributes)

tests :: TestTree
tests =
  testGroup "HTML attribute escaping"
    [ basicEscaping
    , injectionRegression
    , entityLiterals
    , attributeNames
    , layeredAndStructuredValues
    , existingBehaviour
    , rendererAgreement
    ]

-- Inputs
--

-- | Raw input paired with its expected serialized (escaped) form.
basicSamples :: [(T.Text, T.Text)]
basicSamples =
  [ ("ordinary text", "ordinary text")
  , ("double \"quotes\"", "double &#34;quotes&#34;")
  , ("single 'quotes'", "single &#39;quotes&#39;")
  , ("ampersands & entities", "ampersands &#38; entities")
  , ("angle <brackets>", "angle &#60;brackets&#62;")
  , ("Unicode: café — 日本語", "Unicode: café — 日本語")
  , ("mixed \"quotes\" & <tags>", "mixed &#34;quotes&#34; &#38; &#60;tags&#62;")
  ]

entitySamples :: [(T.Text, T.Text)]
entitySamples =
  [ ("Tom & Jerry", "Tom &#38; Jerry")
  , ("&quote;", "&#38;quote;")
  , ("&quot;", "&#38;quot;")
  , ("&quot", "&#38;quot")
  , ("&#34;", "&#38;#34;")
  , ("&#x22;", "&#38;#x22;")
  , ("&amp;lt;", "&#38;amp;lt;")
  ]

injectionPayloads :: [(T.Text, T.Text)]
injectionPayloads =
  [ ( "description\" onerror=\"alert(1)"
    , "description&#34; onerror=&#34;alert(1)"
    )
  , ( "x' onfocus='alert(1)"
    , "x&#39; onfocus=&#39;alert(1)"
    )
  , ( "\"><script>alert(1)</script>"
    , "&#34;&#62;&#60;script&#62;alert(1)&#60;/script&#62;"
    )
  , ( "&#34; onerror=&#34;alert(1)"
    , "&#38;#34; onerror=&#38;#34;alert(1)"
    )
  , ( "a\"b & 'c' <d> 日本"
    , "a&#34;b &#38; &#39;c&#39; &#60;d&#62; 日本"
    )
  ]

-- Representative attributes
--

-- | A case renders a raw value into a node, gives the expected full output
-- for an already-escaped value, and names the attribute the value lands in.
data AttrCase =
  AttrCase
    { caseLabel :: String
    , caseCheck :: T.Text -> T.Text -> Assertion
    }

mkCase :: String
       -> T.Text
       -> (T.Text -> E.ChildHTML parent grandparent)
       -> (T.Text -> T.Text)
       -> AttrCase
mkCase label attrName mkNode expected =
  AttrCase label $ \raw escaped ->
    checkSingleAttribute (mkNode raw) (expected escaped) attrName raw

attrCases :: [AttrCase]
attrCases =
  [ mkCase "alt" "alt"
      (\v -> E.img [A.alt v] :: E.ChildHTML E.Body E.Html)
      (\e -> "<img alt=\"" <> e <> "\"/>")
  , mkCase "title" "title"
      (\v -> E.div [A.title v] [] :: E.ChildHTML E.Body E.Html)
      (\e -> "<div title=\"" <> e <> "\"></div>")
  , mkCase "placeholder" "placeholder"
      (\v -> E.textarea [A.placeholder v] "" :: E.ChildHTML E.Body E.Html)
      (\e -> "<textarea placeholder=\"" <> e <> "\"></textarea>")
  , mkCase "value" "value"
      (\v -> E.button [A.value v] [] :: E.ChildHTML E.Body E.Html)
      (\e -> "<button value=\"" <> e <> "\"></button>")
  , mkCase "label" "label"
      (\v -> E.select [] [E.optgroup [A.label v] []] :: E.ChildHTML E.Body E.Html)
      (\e -> "<select><optgroup label=\"" <> e <> "\"></optgroup></select>")
  , mkCase "content" "content"
      (\v -> E.meta [A.content v] :: E.ChildHTML E.Head E.Html)
      (\e -> "<meta content=\"" <> e <> "\"/>")
  , mkCase "pattern" "pattern"
      (\v -> E.input [A.pattern v] :: E.ChildHTML E.Body E.Html)
      (\e -> "<input pattern=\"" <> e <> "\"/>")
  , mkCase "href (raw URL)" "href"
      (\v -> E.a [A.href (Types.mkRawURL v)] [] :: E.ChildHTML E.Body E.Html)
      (\e -> "<a href=\"" <> e <> "\"></a>")
  , mkCase "custom attribute" "x-note"
      (\v -> E.div [A.customAttribute "x-note" v] [] :: E.ChildHTML E.Body E.Html)
      (\e -> "<div x-note=\"" <> e <> "\"></div>")
  , mkCase "data-*" "data-note"
      (\v -> E.div [A.customData "note" v] [] :: E.ChildHTML E.Body E.Html)
      (\e -> "<div data-note=\"" <> e <> "\"></div>")
  , mkCase "aria-label" "aria-label"
      (\v -> E.div [A.aria_label v] [] :: E.ChildHTML E.Body E.Html)
      (\e -> "<div aria-label=\"" <> e <> "\"></div>")
  , mkCase "aria (raw)" "aria-note"
      (\v -> E.div [A.aria (NET.new 'n' "ote") v] [] :: E.ChildHTML E.Body E.Html)
      (\e -> "<div aria-note=\"" <> e <> "\"></div>")
  , mkCase "hx-confirm" "hx-confirm"
      (\v -> E.div [A.hxConfirm v] [] :: E.ChildHTML E.Body E.Html)
      (\e -> "<div hx-confirm=\"" <> e <> "\"></div>")
  , mkCase "hx-prompt" "hx-prompt"
      (\v -> E.div [A.hxPrompt v] [] :: E.ChildHTML E.Body E.Html)
      (\e -> "<div hx-prompt=\"" <> e <> "\"></div>")
  , mkCase "style" "style"
      (\v -> E.div [A.style v] [] :: E.ChildHTML E.Body E.Html)
      (\e -> "<div style=\"" <> e <> "\"></div>")
  ]

-- Tests
--

basicEscaping :: TestTree
basicEscaping =
  testGroup "Basic escaping"
    [ testCase (caseLabel c <> ": " <> T.unpack raw) (caseCheck c raw escaped)
    | c <- attrCases
    , (raw, escaped) <- basicSamples
    ]

injectionRegression :: TestTree
injectionRegression =
  testGroup "Injection regression"
    [ testCase "img alt payload yields a single alt attribute" $ do
        let payload = "description\" onerror=\"alert(1)"
            node = E.img [A.alt payload] :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<img alt=\"description&#34; onerror=&#34;alert(1)\"/>"
        allAttributes HTMLMode out @?= Right [("alt", payload)]

    , testGroup "Representative attributes"
        [ testCase (caseLabel c <> ": " <> T.unpack raw) (caseCheck c raw escaped)
        | c <- attrCases
        , (raw, escaped) <- injectionPayloads
        ]

    , testCase "on* event handler content stays inside its attribute" $ do
        let script = "alert(\"x\") && a < b; x=\"\" onmouseover=\"steal()"
            node = E.div [A.on A.Click (RawJavaScript script)] [] :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        allAttributes HTMLMode out @?= Right [("onclick", "js:" <> script)]

    , testCase "hx-on content stays inside its attribute" $ do
        let action = "go(\"a\") \" onclick=\"evil()"
            node = E.div [A.hxOn HTML.ClickEvent action] [] :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        allAttributes HTMLMode out @?= Right [("hx-on-click", action)]
    ]

entityLiterals :: TestTree
entityLiterals =
  testGroup "Ampersands and entity-like input are literal"
    [ testCase (caseLabel c <> ": " <> T.unpack raw) (caseCheck c raw escaped)
    | c <- take 3 attrCases <> [c' | c' <- attrCases, caseLabel c' == "href (raw URL)"]
    , (raw, escaped) <- entitySamples
    ]

attributeNames :: TestTree
attributeNames =
  testGroup "Custom attribute names"
    [ testCase "unsafe custom names are omitted" $
        forM_ unsafeNames $ \name -> do
          let node =
                E.div
                  [ A.customAttribute name "v"
                  , A.customBooleanAttribute name True
                  , A.title "kept"
                  ] [] :: E.ChildHTML E.Body E.Html
          out <- renderAgreed node
          out @?= "<div title=\"kept\"></div>"

    , testCase "unsafe data-* names are omitted" $
        forM_ (filter (not . T.null) unsafeNames) $ \name -> do
          let node =
                E.div
                  [ A.customData name "v"
                  , A.customDataBool name
                  , A.title "kept"
                  ] [] :: E.ChildHTML E.Body E.Html
          out <- renderAgreed node
          out @?= "<div title=\"kept\"></div>"

    , testCase "unsafe raw ARIA names are omitted" $ do
        let node =
              E.div
                [ A.aria (NET.new 'x' "\" onclick=\"alert(1)") "v"
                , A.aria (NET.new 'a' " b") "v"
                , A.title "kept"
                ] [] :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<div title=\"kept\"></div>"

    , testCase "injection through a custom name produces no attributes" $ do
        let node =
              E.div [A.customAttribute "x\" onclick=\"alert(2)" "v"] []
                :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<div></div>"
        allAttributes HTMLMode out @?= Right []

    , testCase "ordinary custom names still render" $
        forM_ safeNames $ \name -> do
          let node =
                E.div
                  [ A.customAttribute name "a\"b"
                  , A.customBooleanAttribute (name <> "-flag") True
                  ] [] :: E.ChildHTML E.Body E.Html
          out <- renderAgreed node
          out @?= "<div " <> name <> "=\"a&#34;b\" " <> name <> "-flag></div>"
          allAttributes HTMLMode out @?= Right [(name, "a\"b"), (name <> "-flag", "")]

    , testCase "ordinary data-* names still render" $ do
        let node =
              E.div [A.customData "user-id" "42", A.customDataBool "active"] []
                :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<div data-user-id=\"42\" data-active></div>"
    ]

unsafeNames :: [T.Text]
unsafeNames =
  [ ""
  , "x\" onclick=\"alert(1)"
  , "a b"
  , "a\tb"
  , "a\nb"
  , "a=b"
  , "a>b"
  , "a<b"
  , "a/b"
  , "a'b"
  , "a\"b"
  , "a&b"
  , "a\NULb"
  ]

safeNames :: [T.Text]
safeNames =
  [ "x-custom"
  , "@click"
  , ":class"
  , "hx-on:click"
  , "v-bind.prop"
  , "café"
  ]

layeredAndStructuredValues :: TestTree
layeredAndStructuredValues =
  testGroup "Layered and structured values"
    [ testCase "URL query ampersands survive parsing" $ do
        let url = "/search?q=a&b=\"c\"&lt=1"
            node = E.a [A.href (Types.mkRawURL url)] [] :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<a href=\"/search?q=a&#38;b=&#34;c&#34;&#38;lt=1\"></a>"
        allAttributes HTMLMode out @?= Right [("href", url)]

    , testCase "non-ASCII raw URLs are UTF-8 encoded, not truncated to quotes" $ do
        -- U+0122 and U+013C used to be truncated to 0x22 (") and 0x3C (<)
        -- by the ByteString renderer.
        let url = "/p?q=\x0122 onerror=\x0122x\x013C"
            node = E.a [A.href (Types.mkRawURL url)] [] :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        RBS.renderHTML node @?= TE.encodeUtf8 ("<a href=\"" <> url <> "\"></a>")
        allAttributes HTMLMode out @?= Right [("href", url)]

    , testCase "hx-vals JSON is escaped once and round-trips" $ do
        let json = "{\"a\":\"b & c\",\"d\":\"<e>\"}"
            node = E.div [A.hxVals (HTML.InlineJSON (LBS.fromStrict (TE.encodeUtf8 json)))] [] :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<div hx-vals=\"{&#34;a&#34;:&#34;b &#38; c&#34;,&#34;d&#34;:&#34;&#60;e&#62;&#34;}\"></div>"
        allAttributes HTMLMode out @?= Right [("hx-vals", json)]

    , testCase "srcdoc nests escaping layers without double escaping" $ do
        let inner = E.p [A.title "a\"b & c"] [E.text "x < y"] :: E.ChildHTML E.Body E.Html
            node = E.iframe [A.srcdoc inner] :: E.ChildHTML E.Body E.Html
            innerHtml = RT.renderHTML inner
        innerHtml @?= "<p title=\"a&#34;b &#38; c\">x &#60; y</p>"
        out <- renderAgreed node
        out @?= "<iframe srcdoc=\"&#60;p title=&#34;a&#38;#34;b &#38;#38; c&#34;&#62;x &#38;#60; y&#60;/p&#62;\"></iframe>"
        -- One decode recovers the inner document; a second parse of that
        -- document recovers the original inner attribute value.
        allAttributes HTMLMode out @?= Right [("srcdoc", innerHtml)]
        allAttributes HTMLMode innerHtml @?= Right [("title", "a\"b & c")]
    ]

existingBehaviour :: TestTree
existingBehaviour =
  testGroup "Existing behaviour is preserved"
    [ testCase "text nodes escape & < > but not quotes" $ do
        let node = E.p [] [E.text "<b>\"q\" & 'a'</b>"] :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<p>&#60;b&#62;\"q\" &#38; 'a'&#60;/b&#62;</p>"

    , testCase "raw HTML is left unchecked" $ do
        let node = E.rawHTML "<b title=\"x\">&amp;</b>" :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<b title=\"x\">&amp;</b>"

    , testCase "typed id and class" $ do
        let node =
              E.div [A.id (Types.Id (NET.new 'm' "ain")), A.class_ "a b"] []
                :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<div id=\"main\" class=\"a b\"></div>"

    , testCase "typed id containing a quotation mark" $ do
        let node = E.div [A.id (Types.Id (NET.new 'a' "\"b"))] [] :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<div id=\"a&#34;b\"></div>"
        allAttributes HTMLMode out @?= Right [("id", "a\"b")]

    , testCase "numeric and enumerated attributes" $ do
        let node =
              E.textarea [A.maxlength (10 :: Natural), A.wrap HTML.WrapOff] ""
                :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<textarea maxlength=\"10\" wrap=\"off\"></textarea>"

    , testCase "boolean attributes" $ do
        let node =
              E.input [A.disabled, A.required, A.autofocus False]
                :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<input disabled required/>"

    , testCase "SVG attributes" $ do
        let node =
              SVG.svg [A.class_ "icon", A.customAttribute "viewBox" "0 0 10 10"] []
                :: E.ChildHTML E.Body E.Html
        out <- renderAgreed node
        out @?= "<svg class=\"icon\" viewBox=\"0 0 10 10\"></svg>"

    , testCase "Unicode is emitted verbatim in UTF-8" $ do
        let node = E.img [A.alt "café — 日本語 🎉"] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= TE.encodeUtf8 "<img alt=\"café — 日本語 🎉\"/>"
        RT.renderHTML node @?= "<img alt=\"café — 日本語 🎉\"/>"
    ]

rendererAgreement :: TestTree
rendererAgreement =
  testCase "Text, ByteString and String renderers agree on a mixed document" $ do
    let hostile = "mixed \"quotes\" & <tags> 'single' café — 日本語"
        doc =
          E.div
            [ A.title hostile
            , A.customAttribute "x-a" hostile
            , A.customData "b" hostile
            , A.aria_label hostile
            , A.hxConfirm hostile
            , A.class_ "c d"
            ]
            [ E.img [A.alt hostile, A.src (Types.mkRawURL "/i.png?a=1&b=2")]
            , E.a [A.href (Types.mkRawURL "/x?y=\"z\"")] [E.text hostile]
            , E.textarea [A.placeholder hostile] hostile
            , E.button [A.value hostile] [E.text "go"]
            ] :: E.ChildHTML E.Body E.Html
    out <- renderAgreed doc
    TL.toStrict (RT.renderLazyHTML doc) @?= out
    case allAttributes HTMLMode out of
      Left err -> assertFailure err
      Right attrs -> do
        map fst attrs
          @?= [ "title", "x-a", "data-b", "aria-label", "hx-confirm", "class"
              , "alt", "src", "href", "placeholder", "value"
              ]
        lookup "title" attrs @?= Just hostile
        lookup "x-a" attrs @?= Just hostile
        lookup "data-b" attrs @?= Just hostile
        lookup "alt" attrs @?= Just hostile
        lookup "src" attrs @?= Just "/i.png?a=1&b=2"
        lookup "href" attrs @?= Just "/x?y=\"z\""

-- Helpers
--

-- | Renders with every HTML backend, checks that they agree, and returns the
-- Text rendering.
renderAgreed :: E.ChildHTML parent grandparent -> IO T.Text
renderAgreed node = do
  let txt = RT.renderHTML node
  TE.decodeUtf8 (RBS.renderHTML node) @?= txt
  T.pack (RS.renderHTML node) @?= txt
  pure txt

-- | Checks the exact output, and that parsing it finds exactly one attribute,
-- with the given name and the original raw value.
checkSingleAttribute :: E.ChildHTML parent grandparent
                     -> T.Text
                     -> T.Text
                     -> T.Text
                     -> Assertion
checkSingleAttribute node expected attrName raw = do
  out <- renderAgreed node
  out @?= expected
  allAttributes HTMLMode out @?= Right [(attrName, raw)]
