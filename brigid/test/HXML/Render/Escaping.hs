{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Regression tests for attribute-value escaping and attribute-name safety
-- in the HXML renderers. HXML is XML, so beyond the HTML attribute escapes
-- the renderer also escapes tab, newline and carriage return. Without that,
-- XML attribute-value normalization would turn them into spaces.
module HXML.Render.Escaping
  ( tests
  ) where

import Control.Monad (forM_)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Brigid.HTML.Attributes qualified as HA
import Brigid.HTML.Elements qualified as HE
import Brigid.HTML.Render.Text qualified as HRT
import Brigid.HXML.Attributes qualified as A
import Brigid.HXML.Elements qualified as E
import Brigid.HXML.Render.ByteString qualified as RBS
import Brigid.HXML.Render.String qualified as RS
import Brigid.HXML.Render.Text qualified as RT
import Brigid.Types (RawJavaScript (..))

import Escaping.Oracle (Mode (HTMLMode, XMLMode), allAttributes)

tests :: TestTree
tests =
  testGroup "HXML attribute escaping"
    [ basicEscaping
    , injectionRegression
    , attributeNames
    , layeredValues
    ]

samples :: [(T.Text, T.Text)]
samples =
  [ ("ordinary text", "ordinary text")
  , ("double \"quotes\"", "double &#34;quotes&#34;")
  , ("single 'quotes'", "single &#39;quotes&#39;")
  , ("ampersands & entities", "ampersands &#38; entities")
  , ("angle <brackets>", "angle &#60;brackets&#62;")
  , ("Unicode: café — 日本語", "Unicode: café — 日本語")
  , ("mixed \"quotes\" & <tags>", "mixed &#34;quotes&#34; &#38; &#60;tags&#62;")
  , ("line\nbreak\ttab\rreturn", "line&#10;break&#9;tab&#13;return")
  , ("&quot; &#34; &#x22;", "&#38;quot; &#38;#34; &#38;#x22;")
  , ("description\" onerror=\"alert(1)", "description&#34; onerror=&#34;alert(1)")
  ]

basicEscaping :: TestTree
basicEscaping =
  testGroup "Basic escaping"
    [ testGroup "Custom attribute"
        [ testCase (show raw) $
            check (E.doc [A.customAttribute "x-note" raw] [] :: E.HXML)
              ("<doc x-note=\"" <> escaped <> "\"/>")
              [("x-note", raw)]
        | (raw, escaped) <- samples
        ]
    , testGroup "label and value"
        [ testCase (show raw) $
            check (E.pickerItem [A.label raw, A.value raw])
              ("<picker-item label=\"" <> escaped <> "\" value=\"" <> escaped <> "\"/>")
              [("label", raw), ("value", raw)]
        | (raw, escaped) <- samples
        ]
    , testGroup "placeholder"
        [ testCase (show raw) $
            check (E.textField [A.placeholder raw])
              ("<text-field placeholder=\"" <> escaped <> "\"/>")
              [("placeholder", raw)]
        | (raw, escaped) <- samples
        ]
    ]

injectionRegression :: TestTree
injectionRegression =
  testGroup "Injection regression"
    [ testCase "injected-java-script stays inside its attribute" $ do
        let script = "alert(\"hi\") && 1 < 2; x=\"\" onload=\"evil()"
        out <- renderAgreed (E.webView [A.injectedJavaScript (RawJavaScript script)])
        allAttributes XMLMode out @?= Right [("injected-java-script", "js:" <> script)]

    , testCase "value payload cannot add attributes" $ do
        let payload = "\"/><behavior trigger=\"load\" href=\"/evil\"/><x y=\""
        out <- renderAgreed (E.textField [A.value payload])
        allAttributes XMLMode out @?= Right [("value", payload)]
    ]

attributeNames :: TestTree
attributeNames =
  testGroup "Custom attribute names"
    [ testCase "unsafe custom names are omitted" $
        forM_ unsafe $ \name ->
          check (E.doc [A.customAttribute name "v", A.customAttribute "kept" "k"] [] :: E.HXML)
            "<doc kept=\"k\"/>"
            [("kept", "k")]

    , testCase "ordinary custom names still render" $
        forM_ ["x-custom", "xmlns:foo", "café"] $ \name ->
          check (E.doc [A.customAttribute name "a\"b"] [] :: E.HXML)
            ("<doc " <> name <> "=\"a&#34;b\"/>")
            [(name, "a\"b")]
    ]
  where
    unsafe =
      [ ""
      , "x\" onclick=\"alert(1)"
      , "a b"
      , "a\nb"
      , "a=b"
      , "a>b"
      , "a<b"
      , "a/b"
      , "a'b"
      , "a&b"
      ]

layeredValues :: TestTree
layeredValues =
  testGroup "Layered values"
    [ testCase "html attribute embeds escaped HTML without double escaping" $ do
        let inner =
              HE.p [HA.title "a\"b & c"] [HE.text "x < y"]
                :: HE.ChildHTML HE.Body HE.Html
            innerHtml = HRT.renderHTML inner
        innerHtml @?= "<p title=\"a&#34;b &#38; c\">x &#60; y</p>"
        out <- renderAgreed (E.webView [A.html inner])
        out @?= "<web-view html=\"&#60;p title=&#34;a&#38;#34;b &#38;#38; c&#34;&#62;x &#38;#60; y&#60;/p&#62;\"/>"
        allAttributes XMLMode out @?= Right [("html", innerHtml)]
        allAttributes HTMLMode innerHtml @?= Right [("title", "a\"b & c")]
    ]

-- Helpers
--


renderAgreed :: E.ChildHXML parent -> IO T.Text
renderAgreed node = do
  let txt = RT.renderHXML node
  TE.decodeUtf8 (RBS.renderHXML node) @?= txt
  T.pack (RS.renderHTML node) @?= txt
  pure txt

check :: E.ChildHXML parent -> T.Text -> [(T.Text, T.Text)] -> IO ()
check node expected attrs = do
  out <- renderAgreed node
  out @?= expected
  allAttributes XMLMode out @?= Right attrs
