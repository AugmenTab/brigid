{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module HTML.Render.Attributes
  ( tests
  ) where

import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Time qualified as Time
import Integer (Positive)
import Numeric.Natural (Natural)
import Ogma qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Beeline.HTTP.Client (NoPathParams (NoPathParams))
import Beeline.Routing ((/-))
import Beeline.Routing qualified as R

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.Safe qualified as Safe
import Brigid.HTML.Render.ByteString qualified as RBS
import Brigid.HTML.Render.Text qualified as RT
import Brigid.HTML.Types qualified as HTML
import Brigid.Types (RawJavaScript (..))
import Brigid.Types qualified as Types

tests :: TestTree
tests =
  testGroup "HTML attribute rendering"
    [ globalAttributes
    , formAttributes
    , anchorAttributes
    , imgAttributes
    , inputAttributes
    , tableAttributes
    , mediaAttributes
    , scriptLinkAttributes
    , metaAttributes
    , trackAttributes
    , textareaAttributes
    , buttonAttributes
    , meterAttributes
    , miscScopedAttributes
    , additionalGlobalAttributes
    , additionalScopedAttributes
    , ariaAttributes
    , eventAttributes
    , htmxAttributes
    , hyperscriptAttributes
    ]

-- Helpers
--

divWith :: [A.Attribute E.Division] -> (E.ChildHTML E.Body E.Html)
divWith attrs = E.div attrs []

-- Global Attributes (valid on any element)
--

globalAttributes :: TestTree
globalAttributes =
  testGroup "Global attributes"
    [ testCase "accesskey" $ do
        let node = divWith [A.accesskey 'a']
        RBS.renderHTML node @?= "<div accesskey=\"a\"></div>"
        RT.renderHTML  node @?= "<div accesskey=\"a\"></div>"

    , testCase "autocapitalize (none)" $ do
        let node = divWith [A.autocapitalize HTML.NoAutocapitalization]
        RBS.renderHTML node @?= "<div autocapitalize=\"none\"></div>"
        RT.renderHTML  node @?= "<div autocapitalize=\"none\"></div>"

    , testCase "autocapitalize (sentences)" $ do
        let node = divWith [A.autocapitalize HTML.Sentences]
        RBS.renderHTML node @?= "<div autocapitalize=\"sentences\"></div>"
        RT.renderHTML  node @?= "<div autocapitalize=\"sentences\"></div>"

    , testCase "autocapitalize (words)" $ do
        let node = divWith [A.autocapitalize HTML.Words]
        RBS.renderHTML node @?= "<div autocapitalize=\"words\"></div>"
        RT.renderHTML  node @?= "<div autocapitalize=\"words\"></div>"

    , testCase "autocapitalize (characters)" $ do
        let node = divWith [A.autocapitalize HTML.Characters]
        RBS.renderHTML node @?= "<div autocapitalize=\"characters\"></div>"
        RT.renderHTML  node @?= "<div autocapitalize=\"characters\"></div>"

    , testCase "autocorrect (on)" $ do
        let node = divWith [A.autocorrect HTML.On]
        RBS.renderHTML node @?= "<div autocorrect=\"on\"></div>"
        RT.renderHTML  node @?= "<div autocorrect=\"on\"></div>"

    , testCase "autocorrect (off)" $ do
        let node = divWith [A.autocorrect HTML.Off]
        RBS.renderHTML node @?= "<div autocorrect=\"off\"></div>"
        RT.renderHTML  node @?= "<div autocorrect=\"off\"></div>"

    , testCase "autofocus (True)" $ do
        let node = divWith [A.autofocus True]
        RBS.renderHTML node @?= "<div autofocus></div>"
        RT.renderHTML  node @?= "<div autofocus></div>"

    , testCase "autofocus (False)" $ do
        let node = divWith [A.autofocus False]
        RBS.renderHTML node @?= "<div></div>"
        RT.renderHTML  node @?= "<div></div>"

    , testCase "class_ (single)" $ do
        let node = divWith [A.class_ "foo"]
        RBS.renderHTML node @?= "<div class=\"foo\"></div>"
        RT.renderHTML  node @?= "<div class=\"foo\"></div>"

    , testCase "classes (multiple)" $ do
        let node = divWith [A.classes ["foo", "bar"]]
        RBS.renderHTML node @?= "<div class=\" foo bar\"></div>"
        RT.renderHTML  node @?= "<div class=\" foo bar\"></div>"

    , testCase "classes (deduplicates)" $ do
        let node = divWith [A.classes ["foo", "foo", "bar"]]
        RBS.renderHTML node @?= "<div class=\" foo bar\"></div>"
        RT.renderHTML  node @?= "<div class=\" foo bar\"></div>"

    , testCase "contenteditable (true)" $ do
        let node = divWith [A.contenteditable HTML.Editable]
        RBS.renderHTML node @?= "<div contenteditable=\"true\"></div>"
        RT.renderHTML  node @?= "<div contenteditable=\"true\"></div>"

    , testCase "contenteditable (false)" $ do
        let node = divWith [A.contenteditable HTML.NotEditable]
        RBS.renderHTML node @?= "<div contenteditable=\"false\"></div>"
        RT.renderHTML  node @?= "<div contenteditable=\"false\"></div>"

    , testCase "contenteditable (plaintext-only)" $ do
        let node = divWith [A.contenteditable HTML.PlaintextOnly]
        RBS.renderHTML node @?= "<div contenteditable=\"plaintext-only\"></div>"
        RT.renderHTML  node @?= "<div contenteditable=\"plaintext-only\"></div>"

    , testCase "customData" $ do
        let node = divWith [A.customData "color" "blue"]
        RBS.renderHTML node @?= "<div data-color=\"blue\"></div>"
        RT.renderHTML  node @?= "<div data-color=\"blue\"></div>"

    , testCase "customDataBool" $ do
        let node = divWith [A.customDataBool "active"]
        RBS.renderHTML node @?= "<div data-active></div>"
        RT.renderHTML  node @?= "<div data-active></div>"

    , testCase "dir (ltr)" $ do
        let node = divWith [A.dir HTML.LeftToRight]
        RBS.renderHTML node @?= "<div dir=\"ltr\"></div>"
        RT.renderHTML  node @?= "<div dir=\"ltr\"></div>"

    , testCase "dir (rtl)" $ do
        let node = divWith [A.dir HTML.RightToLeft]
        RBS.renderHTML node @?= "<div dir=\"rtl\"></div>"
        RT.renderHTML  node @?= "<div dir=\"rtl\"></div>"

    , testCase "dir (auto)" $ do
        let node = divWith [A.dir HTML.AutoDirection]
        RBS.renderHTML node @?= "<div dir=\"auto\"></div>"
        RT.renderHTML  node @?= "<div dir=\"auto\"></div>"

    , testCase "draggable (True)" $ do
        let node = divWith [A.draggable True]
        RBS.renderHTML node @?= "<div draggable=\"true\"></div>"
        RT.renderHTML  node @?= "<div draggable=\"true\"></div>"

    , testCase "draggable (False)" $ do
        let node = divWith [A.draggable False]
        RBS.renderHTML node @?= "<div draggable=\"false\"></div>"
        RT.renderHTML  node @?= "<div draggable=\"false\"></div>"

    , testCase "enterkeyhint (enter)" $ do
        let node = divWith [A.enterkeyhint HTML.KeyHintEnter]
        RBS.renderHTML node @?= "<div enterkeyhint=\"enter\"></div>"
        RT.renderHTML  node @?= "<div enterkeyhint=\"enter\"></div>"

    , testCase "enterkeyhint (done)" $ do
        let node = divWith [A.enterkeyhint HTML.KeyHintDone]
        RBS.renderHTML node @?= "<div enterkeyhint=\"done\"></div>"
        RT.renderHTML  node @?= "<div enterkeyhint=\"done\"></div>"

    , testCase "enterkeyhint (go)" $ do
        let node = divWith [A.enterkeyhint HTML.KeyHintGo]
        RBS.renderHTML node @?= "<div enterkeyhint=\"go\"></div>"
        RT.renderHTML  node @?= "<div enterkeyhint=\"go\"></div>"

    , testCase "enterkeyhint (next)" $ do
        let node = divWith [A.enterkeyhint HTML.KeyHintNext]
        RBS.renderHTML node @?= "<div enterkeyhint=\"next\"></div>"
        RT.renderHTML  node @?= "<div enterkeyhint=\"next\"></div>"

    , testCase "enterkeyhint (previous)" $ do
        let node = divWith [A.enterkeyhint HTML.KeyHintPrevious]
        RBS.renderHTML node @?= "<div enterkeyhint=\"previous\"></div>"
        RT.renderHTML  node @?= "<div enterkeyhint=\"previous\"></div>"

    , testCase "enterkeyhint (search)" $ do
        let node = divWith [A.enterkeyhint HTML.KeyHintSearch]
        RBS.renderHTML node @?= "<div enterkeyhint=\"search\"></div>"
        RT.renderHTML  node @?= "<div enterkeyhint=\"search\"></div>"

    , testCase "enterkeyhint (send)" $ do
        let node = divWith [A.enterkeyhint HTML.KeyHintSend]
        RBS.renderHTML node @?= "<div enterkeyhint=\"send\"></div>"
        RT.renderHTML  node @?= "<div enterkeyhint=\"send\"></div>"

    , testCase "exportparts (single)" $ do
        let node = divWith [ A.exportparts
                               [ HTML.ExportPart (HTML.Part "tab") Nothing
                               ]
                           ]
        RBS.renderHTML node @?= "<div exportparts=\"tab\"></div>"
        RT.renderHTML  node @?= "<div exportparts=\"tab\"></div>"

    , testCase "exportparts (renamed)" $ do
        let node = divWith [ A.exportparts
                               [ HTML.ExportPart (HTML.Part "tab") (Just "panel-tab")
                               ]
                           ]
        RBS.renderHTML node @?= "<div exportparts=\"tab:panel-tab\"></div>"
        RT.renderHTML  node @?= "<div exportparts=\"tab:panel-tab\"></div>"

    , testCase "exportparts (multiple)" $ do
        let node = divWith [ A.exportparts
                               [ HTML.ExportPart (HTML.Part "tab") Nothing
                               , HTML.ExportPart (HTML.Part "btn") (Just "panel-btn")
                               ]
                           ]
        RBS.renderHTML node @?= "<div exportparts=\"tab, btn:panel-btn\"></div>"
        RT.renderHTML  node @?= "<div exportparts=\"tab, btn:panel-btn\"></div>"

    , testCase "hidden (True)" $ do
        let node = divWith [A.hide True]
        RBS.renderHTML node @?= "<div hidden></div>"
        RT.renderHTML  node @?= "<div hidden></div>"

    , testCase "hidden (False)" $ do
        let node = divWith [A.hide False]
        RBS.renderHTML node @?= "<div></div>"
        RT.renderHTML  node @?= "<div></div>"

    , testCase "id" $ do
        let node = divWith [A.id (Types.Id (NET.singleton 'x'))]
        RBS.renderHTML node @?= "<div id=\"x\"></div>"
        RT.renderHTML  node @?= "<div id=\"x\"></div>"

    , testCase "inert (True)" $ do
        let node = divWith [A.inert True]
        RBS.renderHTML node @?= "<div inert></div>"
        RT.renderHTML  node @?= "<div inert></div>"

    , testCase "inert (False)" $ do
        let node = divWith [A.inert False]
        RBS.renderHTML node @?= "<div></div>"
        RT.renderHTML  node @?= "<div></div>"

    , testCase "inputmode (none)" $ do
        let node = divWith [A.inputmode HTML.NoInputMode]
        RBS.renderHTML node @?= "<div inputmode=\"none\"></div>"
        RT.renderHTML  node @?= "<div inputmode=\"none\"></div>"

    , testCase "inputmode (text)" $ do
        let node = divWith [A.inputmode HTML.TextMode]
        RBS.renderHTML node @?= "<div inputmode=\"text\"></div>"
        RT.renderHTML  node @?= "<div inputmode=\"text\"></div>"

    , testCase "inputmode (decimal)" $ do
        let node = divWith [A.inputmode HTML.DecimalMode]
        RBS.renderHTML node @?= "<div inputmode=\"decimal\"></div>"
        RT.renderHTML  node @?= "<div inputmode=\"decimal\"></div>"

    , testCase "inputmode (numeric)" $ do
        let node = divWith [A.inputmode HTML.NumericMode]
        RBS.renderHTML node @?= "<div inputmode=\"numeric\"></div>"
        RT.renderHTML  node @?= "<div inputmode=\"numeric\"></div>"

    , testCase "inputmode (tel)" $ do
        let node = divWith [A.inputmode HTML.TelephoneMode]
        RBS.renderHTML node @?= "<div inputmode=\"tel\"></div>"
        RT.renderHTML  node @?= "<div inputmode=\"tel\"></div>"

    , testCase "inputmode (search)" $ do
        let node = divWith [A.inputmode HTML.SearchMode]
        RBS.renderHTML node @?= "<div inputmode=\"search\"></div>"
        RT.renderHTML  node @?= "<div inputmode=\"search\"></div>"

    , testCase "inputmode (email)" $ do
        let node = divWith [A.inputmode HTML.EmailMode]
        RBS.renderHTML node @?= "<div inputmode=\"email\"></div>"
        RT.renderHTML  node @?= "<div inputmode=\"email\"></div>"

    , testCase "inputmode (url)" $ do
        let node = divWith [A.inputmode HTML.URLMode]
        RBS.renderHTML node @?= "<div inputmode=\"url\"></div>"
        RT.renderHTML  node @?= "<div inputmode=\"url\"></div>"

    , testCase "is" $ do
        let node = divWith [A.is "my-element"]
        RBS.renderHTML node @?= "<div is=\"my-element\"></div>"
        RT.renderHTML  node @?= "<div is=\"my-element\"></div>"

    , testCase "itemid" $ do
        let node = divWith [A.itemid "https://example.com/item/1"]
        RBS.renderHTML node @?= "<div itemid=\"https://example.com/item/1\"></div>"
        RT.renderHTML  node @?= "<div itemid=\"https://example.com/item/1\"></div>"

    , testCase "itemprop" $ do
        let node = divWith [A.itemprop "name"]
        RBS.renderHTML node @?= "<div itemprop=\"name\"></div>"
        RT.renderHTML  node @?= "<div itemprop=\"name\"></div>"

    , testCase "itemref (single)" $ do
        let node = divWith [ A.itemref
                               (Types.Id (NET.singleton 'a') NEL.:| [])
                           ]
        RBS.renderHTML node @?= "<div itemref=\"a\"></div>"
        RT.renderHTML  node @?= "<div itemref=\"a\"></div>"

    , testCase "itemref (multiple)" $ do
        let node = divWith [ A.itemref
                               ( Types.Id (NET.singleton 'a')
                                 NEL.:|
                                 [Types.Id (NET.singleton 'b')]
                               )
                           ]
        RBS.renderHTML node @?= "<div itemref=\"a b\"></div>"
        RT.renderHTML  node @?= "<div itemref=\"a b\"></div>"

    , testCase "itemscope" $ do
        let node = divWith [A.itemscope]
        RBS.renderHTML node @?= "<div itemscope></div>"
        RT.renderHTML  node @?= "<div itemscope></div>"

    , testCase "lang (known)" $ do
        let node = divWith [A.lang (Just (Ogma.simpleBCP_47 Ogma.English))]
        RBS.renderHTML node @?= "<div lang=\"en\"></div>"
        RT.renderHTML  node @?= "<div lang=\"en\"></div>"

    , testCase "lang (unknown)" $ do
        let node = divWith [A.lang Nothing]
        RBS.renderHTML node @?= "<div lang=\"\"></div>"
        RT.renderHTML  node @?= "<div lang=\"\"></div>"

    , testCase "nonce" $ do
        let node = divWith [A.nonce "abc123"]
        RBS.renderHTML node @?= "<div nonce=\"abc123\"></div>"
        RT.renderHTML  node @?= "<div nonce=\"abc123\"></div>"

    , testCase "part (single)" $ do
        let node = divWith [A.part [HTML.Part "tab"]]
        RBS.renderHTML node @?= "<div part=\"tab\"></div>"
        RT.renderHTML  node @?= "<div part=\"tab\"></div>"

    , testCase "part (multiple)" $ do
        let node = divWith [A.part [HTML.Part "tab", HTML.Part "active"]]
        RBS.renderHTML node @?= "<div part=\"tab active\"></div>"
        RT.renderHTML  node @?= "<div part=\"tab active\"></div>"

    , testCase "popover (auto)" $ do
        let node = divWith [A.popover HTML.AutoPopover]
        RBS.renderHTML node @?= "<div popover=\"auto\"></div>"
        RT.renderHTML  node @?= "<div popover=\"auto\"></div>"

    , testCase "popover (manual)" $ do
        let node = divWith [A.popover HTML.ManualPopover]
        RBS.renderHTML node @?= "<div popover=\"manual\"></div>"
        RT.renderHTML  node @?= "<div popover=\"manual\"></div>"

    , testCase "spellcheck (True)" $ do
        let node = divWith [A.spellcheck True]
        RBS.renderHTML node @?= "<div spellcheck=\"true\"></div>"
        RT.renderHTML  node @?= "<div spellcheck=\"true\"></div>"

    , testCase "spellcheck (False)" $ do
        let node = divWith [A.spellcheck False]
        RBS.renderHTML node @?= "<div spellcheck=\"false\"></div>"
        RT.renderHTML  node @?= "<div spellcheck=\"false\"></div>"

    , testCase "style" $ do
        let node = divWith [A.style "color:red"]
        RBS.renderHTML node @?= "<div style=\"color:red\"></div>"
        RT.renderHTML  node @?= "<div style=\"color:red\"></div>"

    , testCase "styles (multiple)" $ do
        let node = divWith [A.styles ["color:red", "font-size:1rem"]]
        RBS.renderHTML node @?= "<div style=\"color:red;font-size:1rem\"></div>"
        RT.renderHTML  node @?= "<div style=\"color:red;font-size:1rem\"></div>"

    , testCase "tabindex (reachable)" $ do
        let node = divWith [A.tabindex HTML.Reachable]
        RBS.renderHTML node @?= "<div tabindex=\"0\"></div>"
        RT.renderHTML  node @?= "<div tabindex=\"0\"></div>"

    , testCase "tabindex (not reachable)" $ do
        let node = divWith [A.tabindex HTML.NotReachable]
        RBS.renderHTML node @?= "<div tabindex=\"-1\"></div>"
        RT.renderHTML  node @?= "<div tabindex=\"-1\"></div>"

    , testCase "title" $ do
        let node = divWith [A.title "A description"]
        RBS.renderHTML node @?= "<div title=\"A description\"></div>"
        RT.renderHTML  node @?= "<div title=\"A description\"></div>"

    , testCase "translate (yes)" $ do
        let node = divWith [A.translate HTML.Yes]
        RBS.renderHTML node @?= "<div translate=\"yes\"></div>"
        RT.renderHTML  node @?= "<div translate=\"yes\"></div>"

    , testCase "translate (no)" $ do
        let node = divWith [A.translate HTML.No]
        RBS.renderHTML node @?= "<div translate=\"no\"></div>"
        RT.renderHTML  node @?= "<div translate=\"no\"></div>"

    , testCase "writingsuggestions (True)" $ do
        let node = divWith [A.writingsuggestions True]
        RBS.renderHTML node @?= "<div writingsuggestions=\"true\"></div>"
        RT.renderHTML  node @?= "<div writingsuggestions=\"true\"></div>"

    , testCase "writingsuggestions (False)" $ do
        let node = divWith [A.writingsuggestions False]
        RBS.renderHTML node @?= "<div writingsuggestions=\"false\"></div>"
        RT.renderHTML  node @?= "<div writingsuggestions=\"false\"></div>"
    ]

-- Form Attributes
--

formAttributes :: TestTree
formAttributes =
  testGroup "Form attributes"
    [ testCase "accept-charset" $ do
        let node = E.form [A.acceptCharset] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<form accept-charset=\"UTF-8\"></form>"
        RT.renderHTML  node @?= "<form accept-charset=\"UTF-8\"></form>"

    , testCase "action" $ do
        let node = E.form [A.action (Types.mkRawURL "/submit")] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<form action=\"/submit\"></form>"
        RT.renderHTML  node @?= "<form action=\"/submit\"></form>"

    , testCase "enctype" $ do
        let node = E.form [A.enctype "multipart/form-data"] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<form enctype=\"multipart/form-data\"></form>"
        RT.renderHTML  node @?= "<form enctype=\"multipart/form-data\"></form>"

    , testCase "method (GET)" $ do
        let node = E.form [A.method Types.FormGET] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<form method=\"get\"></form>"
        RT.renderHTML  node @?= "<form method=\"get\"></form>"

    , testCase "method (POST)" $ do
        let node = E.form [A.method Types.FormPOST] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<form method=\"post\"></form>"
        RT.renderHTML  node @?= "<form method=\"post\"></form>"

    , testCase "novalidate" $ do
        let node = E.form [A.novalidate] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<form novalidate></form>"
        RT.renderHTML  node @?= "<form novalidate></form>"

    , testCase "validate (False = novalidate)" $ do
        let node = E.form [A.validate False] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<form novalidate></form>"
        RT.renderHTML  node @?= "<form novalidate></form>"

    , testCase "validate (True = no attribute)" $ do
        let node = E.form [A.validate True] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<form></form>"
        RT.renderHTML  node @?= "<form></form>"
    ]

-- Anchor Attributes
--

anchorAttributes :: TestTree
anchorAttributes =
  testGroup "Anchor attributes"
    [ testCase "href" $ do
        let node = E.a [A.href (Types.mkRawURL "https://example.com")] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<a href=\"https://example.com\"></a>"
        RT.renderHTML  node @?= "<a href=\"https://example.com\"></a>"

    , testCase "hreflang" $ do
        let node = E.a [A.hreflang (Ogma.simpleBCP_47 Ogma.English)] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<a hreflang=\"en\"></a>"
        RT.renderHTML  node @?= "<a hreflang=\"en\"></a>"

    , testCase "download (no filename)" $ do
        let node = E.a [A.download Nothing] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<a download></a>"
        RT.renderHTML  node @?= "<a download></a>"

    , testCase "download (with filename)" $ do
        let node = E.a [A.download (Just (NET.new 'f' "ile.pdf"))] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<a download=\"file.pdf\"></a>"
        RT.renderHTML  node @?= "<a download=\"file.pdf\"></a>"

    , testCase "ping (single)" $ do
        let node = E.a [A.ping (Types.mkPing (Types.mkRawURL "https://track.example.com") NEL.:| [])] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<a ping=\"https://track.example.com\"></a>"
        RT.renderHTML  node @?= "<a ping=\"https://track.example.com\"></a>"

    , testCase "ping (multiple)" $ do
        let node = E.a [ A.ping
                           ( Types.mkPing (Types.mkRawURL "https://track1.example.com")
                             NEL.:|
                             [Types.mkPing (Types.mkRawURL "https://track2.example.com")]
                           )
                       ] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<a ping=\"https://track1.example.com https://track2.example.com\"></a>"
        RT.renderHTML  node @?= "<a ping=\"https://track1.example.com https://track2.example.com\"></a>"

    , testCase "referrerpolicy" $ do
        let node = E.a [A.referrerpolicy HTML.NoReferrer] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<a referrerpolicy=\"no-referrer\"></a>"
        RT.renderHTML  node @?= "<a referrerpolicy=\"no-referrer\"></a>"

    , testCase "target" $ do
        let node = E.a [A.target HTML.Blank] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<a target=\"_blank\"></a>"
        RT.renderHTML  node @?= "<a target=\"_blank\"></a>"
    ]

-- Image Attributes
--

imgAttributes :: TestTree
imgAttributes =
  testGroup "Image attributes"
    [ testCase "alt" $ do
        let node = E.img [A.alt "A photo"] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img alt=\"A photo\"/>"
        RT.renderHTML  node @?= "<img alt=\"A photo\"/>"

    , testCase "crossorigin (anonymous)" $ do
        let node = E.img [A.crossorigin HTML.Anonymous] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img crossorigin=\"anonymous\"/>"
        RT.renderHTML  node @?= "<img crossorigin=\"anonymous\"/>"

    , testCase "crossorigin (use-credentials)" $ do
        let node = E.img [A.crossorigin HTML.UseCredentials] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img crossorigin=\"use-credentials\"/>"
        RT.renderHTML  node @?= "<img crossorigin=\"use-credentials\"/>"

    , testCase "decoding (auto)" $ do
        let node = E.img [A.decoding HTML.DecodeAuto] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img decoding=\"auto\"/>"
        RT.renderHTML  node @?= "<img decoding=\"auto\"/>"

    , testCase "decoding (sync)" $ do
        let node = E.img [A.decoding HTML.DecodeSync] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img decoding=\"sync\"/>"
        RT.renderHTML  node @?= "<img decoding=\"sync\"/>"

    , testCase "decoding (async)" $ do
        let node = E.img [A.decoding HTML.DecodeAsync] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img decoding=\"async\"/>"
        RT.renderHTML  node @?= "<img decoding=\"async\"/>"

    , testCase "fetchpriority (high)" $ do
        let node = E.img [A.fetchpriority HTML.FetchHigh] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img fetchpriority=\"high\"/>"
        RT.renderHTML  node @?= "<img fetchpriority=\"high\"/>"

    , testCase "fetchpriority (low)" $ do
        let node = E.img [A.fetchpriority HTML.FetchLow] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img fetchpriority=\"low\"/>"
        RT.renderHTML  node @?= "<img fetchpriority=\"low\"/>"

    , testCase "fetchpriority (auto)" $ do
        let node = E.img [A.fetchpriority HTML.FetchAuto] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img fetchpriority=\"auto\"/>"
        RT.renderHTML  node @?= "<img fetchpriority=\"auto\"/>"

    , testCase "height" $ do
        let node = E.img [A.height (100 :: Positive)] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img height=\"100\"/>"
        RT.renderHTML  node @?= "<img height=\"100\"/>"

    , testCase "ismap" $ do
        let node = E.img [A.ismap] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img ismap/>"
        RT.renderHTML  node @?= "<img ismap/>"

    , testCase "loading (eager)" $ do
        let node = E.img [A.loading HTML.Eager] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img loading=\"eager\"/>"
        RT.renderHTML  node @?= "<img loading=\"eager\"/>"

    , testCase "loading (lazy)" $ do
        let node = E.img [A.loading HTML.Lazy] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img loading=\"lazy\"/>"
        RT.renderHTML  node @?= "<img loading=\"lazy\"/>"

    , testCase "src" $ do
        let node = E.img [A.src (Types.mkRawURL "image.png")] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img src=\"image.png\"/>"
        RT.renderHTML  node @?= "<img src=\"image.png\"/>"

    , testCase "width" $ do
        let node = E.img [A.width (200 :: Positive)] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img width=\"200\"/>"
        RT.renderHTML  node @?= "<img width=\"200\"/>"
    ]

-- Input Attributes
--

inputAttributes :: TestTree
inputAttributes =
  testGroup "Input attributes"
    [ testCase "accept" $ do
        let node = Safe.file [A.accept "image/*"] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"file\" accept=\"image/*\"/>"
        RT.renderHTML  node @?= "<input type=\"file\" accept=\"image/*\"/>"

    , testCase "checked (True)" $ do
        let node = Safe.checkbox [A.check True] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"checkbox\" checked/>"
        RT.renderHTML  node @?= "<input type=\"checkbox\" checked/>"

    , testCase "checked (False)" $ do
        let node = Safe.checkbox [A.check False] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"checkbox\"/>"
        RT.renderHTML  node @?= "<input type=\"checkbox\"/>"

    , testCase "disabled (True)" $ do
        let node = E.button [A.disable True] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button disabled></button>"
        RT.renderHTML  node @?= "<button disabled></button>"

    , testCase "disabled (False)" $ do
        let node = E.button [A.disable False] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button></button>"
        RT.renderHTML  node @?= "<button></button>"

    , testCase "dirname" $ do
        let node = E.textarea [A.dirname "comment.dir"] "" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<textarea dirname=\"comment.dir\"></textarea>"
        RT.renderHTML  node @?= "<textarea dirname=\"comment.dir\"></textarea>"

    , testCase "maxlength" $ do
        let node = E.textarea [A.maxlength (100 :: Natural)] "" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<textarea maxlength=\"100\"></textarea>"
        RT.renderHTML  node @?= "<textarea maxlength=\"100\"></textarea>"

    , testCase "minlength" $ do
        let node = E.textarea [A.minlength (10 :: Natural)] "" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<textarea minlength=\"10\"></textarea>"
        RT.renderHTML  node @?= "<textarea minlength=\"10\"></textarea>"

    , testCase "multiple" $ do
        let node = E.select [A.multiple] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<select multiple></select>"
        RT.renderHTML  node @?= "<select multiple></select>"

    , testCase "pattern" $ do
        let node = Safe.text [A.pattern "[0-9]{3}"] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"text\" pattern=\"[0-9]{3}\"/>"
        RT.renderHTML  node @?= "<input type=\"text\" pattern=\"[0-9]{3}\"/>"

    , testCase "placeholder" $ do
        let node = E.textarea [A.placeholder "Enter text..."] "" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<textarea placeholder=\"Enter text...\"></textarea>"
        RT.renderHTML  node @?= "<textarea placeholder=\"Enter text...\"></textarea>"

    , testCase "readonly" $ do
        let node = E.textarea [A.readonly] "" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<textarea readonly></textarea>"
        RT.renderHTML  node @?= "<textarea readonly></textarea>"

    , testCase "required" $ do
        let node = E.select [A.required] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<select required></select>"
        RT.renderHTML  node @?= "<select required></select>"

    , testCase "size" $ do
        let node = E.select [A.size (5 :: Positive)] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<select size=\"5\"></select>"
        RT.renderHTML  node @?= "<select size=\"5\"></select>"

    , testCase "step (any)" $ do
        let node = Safe.number [A.step HTML.Any] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"number\" step=\"any\"/>"
        RT.renderHTML  node @?= "<input type=\"number\" step=\"any\"/>"

    , testCase "step (value)" $ do
        let node = Safe.number [A.step (HTML.Step (HTML.numberFromIntegral (5 :: Int)))] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"number\" step=\"5\"/>"
        RT.renderHTML  node @?= "<input type=\"number\" step=\"5\"/>"
    ]

-- Table Attributes
--

tableAttributes :: TestTree
tableAttributes =
  testGroup "Table attributes"
    [ testCase "abbr" $ do
        let node = E.th [A.abbr "Name"] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<th abbr=\"Name\"></th>"
        RT.renderHTML  node @?= "<th abbr=\"Name\"></th>"

    , testCase "colspan" $ do
        let node = E.td [A.colspan (2 :: Positive)] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<td colspan=\"2\"></td>"
        RT.renderHTML  node @?= "<td colspan=\"2\"></td>"

    , testCase "headers (single)" $ do
        let node = E.td [A.headers [Types.Id (NET.singleton 'h')]] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<td headers=\"h\"></td>"
        RT.renderHTML  node @?= "<td headers=\"h\"></td>"

    , testCase "headers (multiple)" $ do
        let node = E.td [ A.headers
                            [ Types.Id (NET.singleton 'a')
                            , Types.Id (NET.singleton 'b')
                            ]
                        ] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<td headers=\"a b\"></td>"
        RT.renderHTML  node @?= "<td headers=\"a b\"></td>"

    , testCase "rowspan" $ do
        let node = E.td [A.rowspan (3 :: Positive)] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<td rowspan=\"3\"></td>"
        RT.renderHTML  node @?= "<td rowspan=\"3\"></td>"

    , testCase "scope (col)" $ do
        let node = E.th [A.scope HTML.Col] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<th scope=\"col\"></th>"
        RT.renderHTML  node @?= "<th scope=\"col\"></th>"

    , testCase "scope (row)" $ do
        let node = E.th [A.scope HTML.Row] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<th scope=\"row\"></th>"
        RT.renderHTML  node @?= "<th scope=\"row\"></th>"

    , testCase "scope (colgroup)" $ do
        let node = E.th [A.scope HTML.ColGroup] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<th scope=\"colgroup\"></th>"
        RT.renderHTML  node @?= "<th scope=\"colgroup\"></th>"

    , testCase "scope (rowgroup)" $ do
        let node = E.th [A.scope HTML.RowGroup] [] :: E.ChildHTML E.TableRow E.TableBody
        RBS.renderHTML node @?= "<th scope=\"rowgroup\"></th>"
        RT.renderHTML  node @?= "<th scope=\"rowgroup\"></th>"

    , testCase "span" $ do
        let node = E.col [A.span (2 :: Positive)] :: E.ChildHTML E.TableColumnGroup E.Table
        RBS.renderHTML node @?= "<col span=\"2\"/>"
        RT.renderHTML  node @?= "<col span=\"2\"/>"
    ]

-- Media Attributes (audio/video)
--

mediaAttributes :: TestTree
mediaAttributes =
  testGroup "Media attributes"
    [ testCase "autoplay" $ do
        let node = E.audio [A.autoplay] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio autoplay></audio>"
        RT.renderHTML  node @?= "<audio autoplay></audio>"

    , testCase "controls" $ do
        let node = E.audio [A.controls] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio controls></audio>"
        RT.renderHTML  node @?= "<audio controls></audio>"

    , testCase "controlslist (nodownload)" $ do
        let node = E.audio [A.controlslist HTML.NoDownload] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio controlslist=\"nodownload\"></audio>"
        RT.renderHTML  node @?= "<audio controlslist=\"nodownload\"></audio>"

    , testCase "controlslist (nofullscreen)" $ do
        let node = E.audio [A.controlslist HTML.NoFullscreen] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio controlslist=\"nofullscreen\"></audio>"
        RT.renderHTML  node @?= "<audio controlslist=\"nofullscreen\"></audio>"

    , testCase "controlslist (noremoteplayback)" $ do
        let node = E.audio [A.controlslist HTML.NoRemotePlayback] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio controlslist=\"noremoteplayback\"></audio>"
        RT.renderHTML  node @?= "<audio controlslist=\"noremoteplayback\"></audio>"

    , testCase "disableremoteplayback" $ do
        let node = E.audio [A.disableremoteplayback] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio disableremoteplayback></audio>"
        RT.renderHTML  node @?= "<audio disableremoteplayback></audio>"

    , testCase "loop" $ do
        let node = E.audio [A.loop] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio loop></audio>"
        RT.renderHTML  node @?= "<audio loop></audio>"

    , testCase "muted (True)" $ do
        let node = E.audio [A.mute True] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio muted></audio>"
        RT.renderHTML  node @?= "<audio muted></audio>"

    , testCase "muted (False)" $ do
        let node = E.audio [A.mute False] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio></audio>"
        RT.renderHTML  node @?= "<audio></audio>"

    , testCase "preload (none)" $ do
        let node = E.audio [A.preload HTML.PreloadNone] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio preload=\"none\"></audio>"
        RT.renderHTML  node @?= "<audio preload=\"none\"></audio>"

    , testCase "preload (metadata)" $ do
        let node = E.audio [A.preload HTML.PreloadMetadata] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio preload=\"metadata\"></audio>"
        RT.renderHTML  node @?= "<audio preload=\"metadata\"></audio>"

    , testCase "preload (auto)" $ do
        let node = E.audio [A.preload HTML.PreloadAuto] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<audio preload=\"auto\"></audio>"
        RT.renderHTML  node @?= "<audio preload=\"auto\"></audio>"

    , testCase "disablepictureinpicture" $ do
        let node = E.video [A.disablepictureinpicture] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<video disablepictureinpicture></video>"
        RT.renderHTML  node @?= "<video disablepictureinpicture></video>"

    , testCase "playsinline" $ do
        let node = E.video [A.playsinline] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<video playsinline></video>"
        RT.renderHTML  node @?= "<video playsinline></video>"

    , testCase "playInline (False)" $ do
        let node = E.video [A.playInline False] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<video></video>"
        RT.renderHTML  node @?= "<video></video>"

    , testCase "poster" $ do
        let node = E.video [A.poster (Types.mkRawURL "thumb.jpg")] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<video poster=\"thumb.jpg\"></video>"
        RT.renderHTML  node @?= "<video poster=\"thumb.jpg\"></video>"
    ]

-- Script and Link Attributes
--

scriptLinkAttributes :: TestTree
scriptLinkAttributes =
  testGroup "Script/Link attributes"
    [ testCase "async" $ do
        let node = E.script [A.async] Nothing :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<script async></script>"
        RT.renderHTML  node @?= "<script async></script>"

    , testCase "blocking" $ do
        let node = E.script [A.blocking HTML.Render] Nothing :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<script blocking=\"render\"></script>"
        RT.renderHTML  node @?= "<script blocking=\"render\"></script>"

    , testCase "charset" $ do
        let node = E.script [A.charset] Nothing :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<script charset=\"utf-8\"></script>"
        RT.renderHTML  node @?= "<script charset=\"utf-8\"></script>"

    , testCase "defer" $ do
        let node = E.script [A.defer] Nothing :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<script defer></script>"
        RT.renderHTML  node @?= "<script defer></script>"

    , testCase "integrity (sha256)" $ do
        let node = E.script [A.integrity HTML.SHA256 "abc123"] Nothing :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<script integrity=\"sha256-bKE9UspwyIPg8LsQHkJaiehiTeUdstI5JZOvaoQRgJA=\"></script>"
        RT.renderHTML  node @?= "<script integrity=\"sha256-bKE9UspwyIPg8LsQHkJaiehiTeUdstI5JZOvaoQRgJA=\"></script>"

    , testCase "nomodule (True)" $ do
        let node = E.script [A.nomodule True] Nothing :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<script nomodule></script>"
        RT.renderHTML  node @?= "<script nomodule></script>"

    , testCase "nomodule (False)" $ do
        let node = E.script [A.nomodule False] Nothing :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<script></script>"
        RT.renderHTML  node @?= "<script></script>"

    , testCase "as" $ do
        let node = E.link [A.as HTML.AsFetch] :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<link as=\"fetch\"/>"
        RT.renderHTML  node @?= "<link as=\"fetch\"/>"
    ]

-- Meta Attributes
--

metaAttributes :: TestTree
metaAttributes =
  testGroup "Meta attributes"
    [ testCase "content" $ do
        let node = E.meta [A.content "text/html; charset=UTF-8"] :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<meta content=\"text/html; charset=UTF-8\"/>"
        RT.renderHTML  node @?= "<meta content=\"text/html; charset=UTF-8\"/>"
    ]

-- Track Attributes
--

trackAttributes :: TestTree
trackAttributes =
  testGroup "Track attributes"
    [ testCase "default_" $ do
        let node = E.track [A.default_] :: E.ChildHTML E.Audio E.Body
        RBS.renderHTML node @?= "<track default/>"
        RT.renderHTML  node @?= "<track default/>"

    , testCase "kind (subtitles)" $ do
        let node = E.track [A.kind HTML.Subtitles] :: E.ChildHTML E.Audio E.Body
        RBS.renderHTML node @?= "<track kind=\"subtitles\"/>"
        RT.renderHTML  node @?= "<track kind=\"subtitles\"/>"

    , testCase "kind (captions)" $ do
        let node = E.track [A.kind HTML.Captions] :: E.ChildHTML E.Audio E.Body
        RBS.renderHTML node @?= "<track kind=\"captions\"/>"
        RT.renderHTML  node @?= "<track kind=\"captions\"/>"

    , testCase "kind (chapters)" $ do
        let node = E.track [A.kind HTML.Chapters] :: E.ChildHTML E.Audio E.Body
        RBS.renderHTML node @?= "<track kind=\"chapters\"/>"
        RT.renderHTML  node @?= "<track kind=\"chapters\"/>"

    , testCase "kind (metadata)" $ do
        let node = E.track [A.kind HTML.Metadata] :: E.ChildHTML E.Audio E.Body
        RBS.renderHTML node @?= "<track kind=\"metadata\"/>"
        RT.renderHTML  node @?= "<track kind=\"metadata\"/>"

    , testCase "srclang" $ do
        let node = E.track [A.srclang (Ogma.simpleBCP_47 Ogma.English)] :: E.ChildHTML E.Audio E.Body
        RBS.renderHTML node @?= "<track srclang=\"en\"/>"
        RT.renderHTML  node @?= "<track srclang=\"en\"/>"
    ]

-- Textarea Attributes
--

textareaAttributes :: TestTree
textareaAttributes =
  testGroup "Textarea attributes"
    [ testCase "cols" $ do
        let node = E.textarea [A.cols (40 :: Natural)] "" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<textarea cols=\"40\"></textarea>"
        RT.renderHTML  node @?= "<textarea cols=\"40\"></textarea>"

    , testCase "rows" $ do
        let node = E.textarea [A.rows (10 :: Natural)] "" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<textarea rows=\"10\"></textarea>"
        RT.renderHTML  node @?= "<textarea rows=\"10\"></textarea>"

    , testCase "wrap (hard)" $ do
        let node = E.textarea [A.wrap HTML.WrapHard] "" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<textarea wrap=\"hard\"></textarea>"
        RT.renderHTML  node @?= "<textarea wrap=\"hard\"></textarea>"

    , testCase "wrap (soft)" $ do
        let node = E.textarea [A.wrap HTML.WrapSoft] "" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<textarea wrap=\"soft\"></textarea>"
        RT.renderHTML  node @?= "<textarea wrap=\"soft\"></textarea>"

    , testCase "wrap (off)" $ do
        let node = E.textarea [A.wrap HTML.WrapOff] "" :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<textarea wrap=\"off\"></textarea>"
        RT.renderHTML  node @?= "<textarea wrap=\"off\"></textarea>"
    ]

-- Button Attributes
--

buttonAttributes :: TestTree
buttonAttributes =
  testGroup "Button attributes"
    [ testCase "command (show-modal)" $ do
        let node = E.button [A.command HTML.ShowModal] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button command=\"show-modal\"></button>"
        RT.renderHTML  node @?= "<button command=\"show-modal\"></button>"

    , testCase "command (close)" $ do
        let node = E.button [A.command HTML.Close] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button command=\"close\"></button>"
        RT.renderHTML  node @?= "<button command=\"close\"></button>"

    , testCase "command (toggle-popover)" $ do
        let node = E.button [A.command HTML.TogglePopover] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button command=\"toggle-popover\"></button>"
        RT.renderHTML  node @?= "<button command=\"toggle-popover\"></button>"

    , testCase "commandfor" $ do
        let node = E.button [A.commandfor (Types.Id (NET.singleton 'd'))] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button commandfor=\"d\"></button>"
        RT.renderHTML  node @?= "<button commandfor=\"d\"></button>"

    , testCase "form" $ do
        let node = E.button [A.form (Types.Id (NET.singleton 'f'))] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button form=\"f\"></button>"
        RT.renderHTML  node @?= "<button form=\"f\"></button>"

    , testCase "formaction" $ do
        let node = E.button [A.formaction (Types.mkRawURL "/submit")] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button formaction=\"/submit\"></button>"
        RT.renderHTML  node @?= "<button formaction=\"/submit\"></button>"

    , testCase "formenctype" $ do
        let node = E.button [A.formenctype "multipart/form-data"] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button formenctype=\"multipart/form-data\"></button>"
        RT.renderHTML  node @?= "<button formenctype=\"multipart/form-data\"></button>"

    , testCase "formmethod (GET)" $ do
        let node = E.button [A.formmethod Types.FormGET] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button formmethod=\"get\"></button>"
        RT.renderHTML  node @?= "<button formmethod=\"get\"></button>"

    , testCase "formnovalidate" $ do
        let node = E.button [A.formnovalidate] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button formnovalidate></button>"
        RT.renderHTML  node @?= "<button formnovalidate></button>"

    , testCase "formtarget" $ do
        let node = E.button [A.formtarget HTML.Blank] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button formtarget=\"_blank\"></button>"
        RT.renderHTML  node @?= "<button formtarget=\"_blank\"></button>"

    , testCase "popovertarget" $ do
        let node = E.button [A.popovertarget (Types.Id (NET.singleton 'p'))] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button popovertarget=\"p\"></button>"
        RT.renderHTML  node @?= "<button popovertarget=\"p\"></button>"

    , testCase "popovertargetaction (show)" $ do
        let node = E.button [A.popovertargetaction HTML.PopoverShow] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button popovertargetaction=\"show\"></button>"
        RT.renderHTML  node @?= "<button popovertargetaction=\"show\"></button>"

    , testCase "popovertargetaction (hide)" $ do
        let node = E.button [A.popovertargetaction HTML.PopoverHide] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button popovertargetaction=\"hide\"></button>"
        RT.renderHTML  node @?= "<button popovertargetaction=\"hide\"></button>"

    , testCase "popovertargetaction (toggle)" $ do
        let node = E.button [A.popovertargetaction HTML.PopoverToggle] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button popovertargetaction=\"toggle\"></button>"
        RT.renderHTML  node @?= "<button popovertargetaction=\"toggle\"></button>"
    ]

-- Meter Attributes
--

meterAttributes :: TestTree
meterAttributes =
  testGroup "Meter attributes"
    [ testCase "high" $ do
        let node = E.meter [A.high (HTML.numberFromIntegral (80 :: Int))] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<meter high=\"80\"></meter>"
        RT.renderHTML  node @?= "<meter high=\"80\"></meter>"

    , testCase "low" $ do
        let node = E.meter [A.low (HTML.numberFromIntegral (20 :: Int))] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<meter low=\"20\"></meter>"
        RT.renderHTML  node @?= "<meter low=\"20\"></meter>"

    , testCase "optimum" $ do
        let node = E.meter [A.optimum (HTML.numberFromIntegral (50 :: Int))] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<meter optimum=\"50\"></meter>"
        RT.renderHTML  node @?= "<meter optimum=\"50\"></meter>"
    ]

-- Miscellaneous Scoped Attributes
--

miscScopedAttributes :: TestTree
miscScopedAttributes =
  testGroup "Miscellaneous scoped attributes"
    [ testCase "coords (area)" $ do
        let node = E.area [A.coords (0 NEL.:| [0, 10, 10])] :: E.ChildHTML E.Paragraph E.Body
        RBS.renderHTML node @?= "<area coords=\"0,0,10,10\"/>"
        RT.renderHTML  node @?= "<area coords=\"0,0,10,10\"/>"

    , testCase "shape (rect)" $ do
        let node = E.area [A.shape HTML.Rect] :: E.ChildHTML E.Paragraph E.Body
        RBS.renderHTML node @?= "<area shape=\"rect\"/>"
        RT.renderHTML  node @?= "<area shape=\"rect\"/>"

    , testCase "shape (circle)" $ do
        let node = E.area [A.shape HTML.Circle] :: E.ChildHTML E.Paragraph E.Body
        RBS.renderHTML node @?= "<area shape=\"circle\"/>"
        RT.renderHTML  node @?= "<area shape=\"circle\"/>"

    , testCase "shape (poly)" $ do
        let node = E.area [A.shape HTML.Poly] :: E.ChildHTML E.Paragraph E.Body
        RBS.renderHTML node @?= "<area shape=\"poly\"/>"
        RT.renderHTML  node @?= "<area shape=\"poly\"/>"

    , testCase "shape (default)" $ do
        let node = E.area [A.shape HTML.Default] :: E.ChildHTML E.Paragraph E.Body
        RBS.renderHTML node @?= "<area shape=\"default\"/>"
        RT.renderHTML  node @?= "<area shape=\"default\"/>"

    , testCase "label (optgroup)" $ do
        let node = E.optgroup [A.label "Group A"] [] :: E.ChildHTML E.Select E.Body
        RBS.renderHTML node @?= "<optgroup label=\"Group A\"></optgroup>"
        RT.renderHTML  node @?= "<optgroup label=\"Group A\"></optgroup>"

    , testCase "selected (True)" $ do
        let node = E.option [A.select True] [] :: E.ChildHTML E.Select E.Body
        RBS.renderHTML node @?= "<option selected></option>"
        RT.renderHTML  node @?= "<option selected></option>"

    , testCase "selected (False)" $ do
        let node = E.option [A.select False] [] :: E.ChildHTML E.Select E.Body
        RBS.renderHTML node @?= "<option></option>"
        RT.renderHTML  node @?= "<option></option>"

    , testCase "open (details)" $ do
        let node = E.details [A.open] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<details open></details>"
        RT.renderHTML  node @?= "<details open></details>"

    , testCase "reversed (True)" $ do
        let node = E.ol [A.reverse True] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<ol reversed></ol>"
        RT.renderHTML  node @?= "<ol reversed></ol>"

    , testCase "reversed (False)" $ do
        let node = E.ol [A.reverse False] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<ol></ol>"
        RT.renderHTML  node @?= "<ol></ol>"

    , testCase "start" $ do
        let node = E.ol [A.start 3] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<ol start=\"3\"></ol>"
        RT.renderHTML  node @?= "<ol start=\"3\"></ol>"

    , testCase "elementtiming" $ do
        let node = divWith [A.elementtiming "hero-image"]
        RBS.renderHTML node @?= "<div elementtiming=\"hero-image\"></div>"
        RT.renderHTML  node @?= "<div elementtiming=\"hero-image\"></div>"

    , testCase "cite (blockquote)" $ do
        let node = E.blockquote [A.cite (Types.mkRawURL "https://example.com")] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<blockquote cite=\"https://example.com\"></blockquote>"
        RT.renderHTML  node @?= "<blockquote cite=\"https://example.com\"></blockquote>"
    ]

-- Additional Global Attributes
--

additionalGlobalAttributes :: TestTree
additionalGlobalAttributes =
  testGroup "Additional global attributes"
    [ testCase "role" $ do
        let node = divWith [A.role HTML.RoleButton]
        RBS.renderHTML node @?= "<div role=\"button\"></div>"
        RT.renderHTML  node @?= "<div role=\"button\"></div>"

    , testCase "slot" $ do
        let node = divWith [A.slot (Types.Name (NET.singleton 's'))]
        RBS.renderHTML node @?= "<div slot=\"s\"></div>"
        RT.renderHTML  node @?= "<div slot=\"s\"></div>"
    ]

-- Additional Scoped Attributes
--

additionalScopedAttributes :: TestTree
additionalScopedAttributes =
  testGroup "Additional scoped attributes"
    [ testCase "allow" $ do
        let node = E.iframe [A.allow [HTML.Fullscreen]] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<iframe allow=\"fullscreen\"></iframe>"
        RT.renderHTML  node @?= "<iframe allow=\"fullscreen\"></iframe>"

    , testCase "autocomplete (on)" $ do
        let node = Safe.text [A.autocomplete HTML.On] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"text\" autocomplete=\"on\"/>"
        RT.renderHTML  node @?= "<input type=\"text\" autocomplete=\"on\"/>"

    , testCase "autocomplete (off)" $ do
        let node = Safe.text [A.autocomplete HTML.Off] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"text\" autocomplete=\"off\"/>"
        RT.renderHTML  node @?= "<input type=\"text\" autocomplete=\"off\"/>"

    , testCase "capture (user)" $ do
        let node = Safe.file [A.capture (Just HTML.User)] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"file\" capture=\"user\"/>"
        RT.renderHTML  node @?= "<input type=\"file\" capture=\"user\"/>"

    , testCase "capture (environment)" $ do
        let node = Safe.file [A.capture (Just HTML.Environment)] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"file\" capture=\"environment\"/>"
        RT.renderHTML  node @?= "<input type=\"file\" capture=\"environment\"/>"

    , testCase "capture (default)" $ do
        let node = Safe.file [A.capture Nothing] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"file\" capture/>"
        RT.renderHTML  node @?= "<input type=\"file\" capture/>"

    , testCase "data_" $ do
        let node = E.object [A.data_ (Types.mkRawURL "file.swf")] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<object data=\"file.swf\"></object>"
        RT.renderHTML  node @?= "<object data=\"file.swf\"></object>"

    , testCase "datetime" $ do
        let node = E.time [A.datetime (Time.fromGregorian 2024 1 15)] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<time datetime=\"2024-01-15\"></time>"
        RT.renderHTML  node @?= "<time datetime=\"2024-01-15\"></time>"

    , testCase "for (label)" $ do
        let node = E.label [A.for (Types.Id (NET.singleton 'i'))] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<label for=\"i\"></label>"
        RT.renderHTML  node @?= "<label for=\"i\"></label>"

    , testCase "for (output)" $ do
        let node = E.output [A.for (Types.Id (NET.singleton 'a') NEL.:| [])] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<output for=\"a\"></output>"
        RT.renderHTML  node @?= "<output for=\"a\"></output>"

    , testCase "for (output multiple)" $ do
        let node = E.output [ A.for
                                ( Types.Id (NET.singleton 'a')
                                  NEL.:|
                                  [Types.Id (NET.singleton 'b')]
                                )
                            ] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<output for=\"a b\"></output>"
        RT.renderHTML  node @?= "<output for=\"a b\"></output>"

    , testCase "http-equiv" $ do
        let node = E.meta [A.httpEquiv HTML.ContentType] :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<meta http-equiv=\"content-type\"/>"
        RT.renderHTML  node @?= "<meta http-equiv=\"content-type\"/>"

    , testCase "imagesizes" $ do
        let node = E.link
                     [ A.imagesizes
                         ( HTML.Size
                             { HTML.sizeCondition = Nothing
                             , HTML.sizeLength = HTML.SizePx 800
                             }
                           NEL.:| []
                         )
                     ] :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<link imagesizes=\"800px\"/>"
        RT.renderHTML  node @?= "<link imagesizes=\"800px\"/>"

    , testCase "imagesrcset" $ do
        let node = E.link
                     [ A.imagesrcset
                         ( HTML.mkSrcsetCandidate
                             (Types.mkRawURL "img.png")
                             (HTML.SrcsetWidth 480)
                           NEL.:| []
                         )
                     ] :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<link imagesrcset=\"img.png 480w\"/>"
        RT.renderHTML  node @?= "<link imagesrcset=\"img.png 480w\"/>"

    , testCase "list" $ do
        let node = Safe.text [A.list (Types.Id (NET.singleton 'd'))] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"text\" list=\"d\"/>"
        RT.renderHTML  node @?= "<input type=\"text\" list=\"d\"/>"

    , testCase "max" $ do
        let node = Safe.number [A.max (HTML.numberFromIntegral (100 :: Int))] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"number\" max=\"100\"/>"
        RT.renderHTML  node @?= "<input type=\"number\" max=\"100\"/>"

    , testCase "media" $ do
        let node = E.link
                     [ A.media
                         ( HTML.MediaQuery
                             { HTML.mediaQueryModifier   = Nothing
                             , HTML.mediaQueryType       = Nothing
                             , HTML.mediaQueryConditions = [HTML.Width HTML.Max (HTML.MediaPx (HTML.numberFromIntegral (600 :: Int)))]
                             }
                           NEL.:| []
                         )
                     ] :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<link media=\"(max-width: 600px)\"/>"
        RT.renderHTML  node @?= "<link media=\"(max-width: 600px)\"/>"

    , testCase "min" $ do
        let node = Safe.number [A.min (HTML.numberFromIntegral (0 :: Int))] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"number\" min=\"0\"/>"
        RT.renderHTML  node @?= "<input type=\"number\" min=\"0\"/>"

    , testCase "name (input)" $ do
        let node = Safe.text [A.name (Types.Name (NET.singleton 'q'))] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<input type=\"text\" name=\"q\"/>"
        RT.renderHTML  node @?= "<input type=\"text\" name=\"q\"/>"

    , testCase "name (meta)" $ do
        let node = E.meta [A.name HTML.Author] :: E.ChildHTML E.Head E.Html
        RBS.renderHTML node @?= "<meta name=\"author\"/>"
        RT.renderHTML  node @?= "<meta name=\"author\"/>"

    , testCase "rel" $ do
        let node = E.a [A.rel HTML.Rel_NoFollow] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<a rel=\"nofollow\"></a>"
        RT.renderHTML  node @?= "<a rel=\"nofollow\"></a>"

    , testCase "sandbox (empty)" $ do
        let node = E.iframe [A.sandbox []] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<iframe sandbox></iframe>"
        RT.renderHTML  node @?= "<iframe sandbox></iframe>"

    , testCase "sandbox (allow-scripts)" $ do
        let node = E.iframe [A.sandbox [HTML.AllowScripts]] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<iframe sandbox=\"allow-scripts\"></iframe>"
        RT.renderHTML  node @?= "<iframe sandbox=\"allow-scripts\"></iframe>"

    , testCase "sandbox (multiple)" $ do
        let node = E.iframe [A.sandbox [HTML.AllowScripts, HTML.AllowSameOrigin]] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<iframe sandbox=\"allow-scripts allow-same-origin\"></iframe>"
        RT.renderHTML  node @?= "<iframe sandbox=\"allow-scripts allow-same-origin\"></iframe>"

    , testCase "shadowrootmode (open)" $ do
        let node = E.template [A.shadowrootmode HTML.Open] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<template shadowrootmode=\"open\"></template>"
        RT.renderHTML  node @?= "<template shadowrootmode=\"open\"></template>"

    , testCase "shadowrootmode (closed)" $ do
        let node = E.template [A.shadowrootmode HTML.Closed] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<template shadowrootmode=\"closed\"></template>"
        RT.renderHTML  node @?= "<template shadowrootmode=\"closed\"></template>"

    , testCase "shadowrootdelegatesfocus" $ do
        let node = E.template [A.shadowrootdelegatesfocus] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<template shadowrootdelegatesfocus></template>"
        RT.renderHTML  node @?= "<template shadowrootdelegatesfocus></template>"

    , testCase "shadowrootclonable" $ do
        let node = E.template [A.shadowrootclonable] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<template shadowrootclonable></template>"
        RT.renderHTML  node @?= "<template shadowrootclonable></template>"

    , testCase "sizes" $ do
        let node = E.img
                     [ A.sizes
                         ( HTML.Size
                             { HTML.sizeCondition = Nothing
                             , HTML.sizeLength = HTML.SizePx 800
                             }
                           NEL.:| []
                         )
                     ] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img sizes=\"800px\"/>"
        RT.renderHTML  node @?= "<img sizes=\"800px\"/>"

    , testCase "srcdoc" $ do
        let node = E.iframe [A.srcdoc (E.p [] [E.text "Hello"] :: E.ChildHTML E.Body E.Html)] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<iframe srcdoc=\"<p>Hello</p>\"></iframe>"
        RT.renderHTML  node @?= "<iframe srcdoc=\"<p>Hello</p>\"></iframe>"

    , testCase "srcset (single)" $ do
        let node = E.img
                     [ A.srcset
                         ( HTML.mkSrcsetCandidate
                             (Types.mkRawURL "img.png")
                             (HTML.SrcsetWidth 480)
                           NEL.:| []
                         )
                     ] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img srcset=\"img.png 480w\"/>"
        RT.renderHTML  node @?= "<img srcset=\"img.png 480w\"/>"

    , testCase "srcset (multiple)" $ do
        let node = E.img
                     [ A.srcset
                         ( HTML.mkSrcsetCandidate (Types.mkRawURL "img-480.png") (HTML.SrcsetWidth 480)
                           NEL.:|
                           [ HTML.mkSrcsetCandidate (Types.mkRawURL "img-800.png") (HTML.SrcsetWidth 800)
                           ]
                         )
                     ] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img srcset=\"img-480.png 480w, img-800.png 800w\"/>"
        RT.renderHTML  node @?= "<img srcset=\"img-480.png 480w, img-800.png 800w\"/>"

    , testCase "type_ (button)" $ do
        let node = E.button [A.type_ (HTML.mkRawTypeOption "submit")] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button type=\"submit\"></button>"
        RT.renderHTML  node @?= "<button type=\"submit\"></button>"

    , testCase "usemap" $ do
        let node = E.img [A.usemap (Types.Name (NET.singleton 'm'))] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<img usemap=\"#m\"/>"
        RT.renderHTML  node @?= "<img usemap=\"#m\"/>"

    , testCase "value (text)" $ do
        let node = E.button [A.value ("foo" :: T.Text)] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<button value=\"foo\"></button>"
        RT.renderHTML  node @?= "<button value=\"foo\"></button>"

    , testCase "value (integer)" $ do
        let node = E.li [A.value (5 :: Integer)] [] :: E.ChildHTML E.OrderedList E.Body
        RBS.renderHTML node @?= "<li value=\"5\"></li>"
        RT.renderHTML  node @?= "<li value=\"5\"></li>"

    , testCase "value (number)" $ do
        let node = E.meter [A.value (HTML.numberFromIntegral (50 :: Int))] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<meter value=\"50\"></meter>"
        RT.renderHTML  node @?= "<meter value=\"50\"></meter>"

    , testCase "xmlns" $ do
        let node = E.html [A.xmlns (Types.mkRawURL "http://www.w3.org/1999/xhtml")] [E.head [] [], E.body [] []]
        RBS.renderHTML node @?= "<!DOCTYPE html><html xmlns=\"http://www.w3.org/1999/xhtml\"><head></head><body></body></html>"
        RT.renderHTML  node @?= "<!DOCTYPE html><html xmlns=\"http://www.w3.org/1999/xhtml\"><head></head><body></body></html>"
    ]

-- ARIA Attributes
--

ariaAttributes :: TestTree
ariaAttributes =
  testGroup "ARIA attributes"
    [ testCase "aria-label" $ do
        let node = divWith [A.aria_label "Close"]
        RBS.renderHTML node @?= "<div aria-label=\"Close\"></div>"
        RT.renderHTML  node @?= "<div aria-label=\"Close\"></div>"

    , testCase "aria-hidden (True)" $ do
        let node = divWith [A.aria_hidden True]
        RBS.renderHTML node @?= "<div aria-hidden=\"true\"></div>"
        RT.renderHTML  node @?= "<div aria-hidden=\"true\"></div>"

    , testCase "aria-hidden (False)" $ do
        let node = divWith [A.aria_hidden False]
        RBS.renderHTML node @?= "<div aria-hidden=\"false\"></div>"
        RT.renderHTML  node @?= "<div aria-hidden=\"false\"></div>"

    , testCase "aria-expanded (True)" $ do
        let node = divWith [A.aria_expanded True]
        RBS.renderHTML node @?= "<div aria-expanded=\"true\"></div>"
        RT.renderHTML  node @?= "<div aria-expanded=\"true\"></div>"

    , testCase "aria-disabled (True)" $ do
        let node = divWith [A.aria_disabled True]
        RBS.renderHTML node @?= "<div aria-disabled=\"true\"></div>"
        RT.renderHTML  node @?= "<div aria-disabled=\"true\"></div>"

    , testCase "aria-atomic (True)" $ do
        let node = divWith [A.aria_atomic True]
        RBS.renderHTML node @?= "<div aria-atomic=\"true\"></div>"
        RT.renderHTML  node @?= "<div aria-atomic=\"true\"></div>"

    , testCase "aria-busy (True)" $ do
        let node = divWith [A.aria_busy True]
        RBS.renderHTML node @?= "<div aria-busy=\"true\"></div>"
        RT.renderHTML  node @?= "<div aria-busy=\"true\"></div>"

    , testCase "aria-colcount" $ do
        let node = divWith [A.aria_colcount (5 :: Natural)]
        RBS.renderHTML node @?= "<div aria-colcount=\"5\"></div>"
        RT.renderHTML  node @?= "<div aria-colcount=\"5\"></div>"

    , testCase "aria-colindex" $ do
        let node = divWith [A.aria_colindex (2 :: Positive)]
        RBS.renderHTML node @?= "<div aria-colindex=\"2\"></div>"
        RT.renderHTML  node @?= "<div aria-colindex=\"2\"></div>"

    , testCase "aria-colspan" $ do
        let node = divWith [A.aria_colspan (3 :: Positive)]
        RBS.renderHTML node @?= "<div aria-colspan=\"3\"></div>"
        RT.renderHTML  node @?= "<div aria-colspan=\"3\"></div>"

    , testCase "aria-controls (single)" $ do
        let node = divWith [A.aria_controls [Types.Id (NET.singleton 'x')]]
        RBS.renderHTML node @?= "<div aria-controls=\"x\"></div>"
        RT.renderHTML  node @?= "<div aria-controls=\"x\"></div>"

    , testCase "aria-controls (multiple)" $ do
        let node = divWith [ A.aria_controls
                               [ Types.Id (NET.singleton 'a')
                               , Types.Id (NET.singleton 'b')
                               ]
                           ]
        RBS.renderHTML node @?= "<div aria-controls=\"a b\"></div>"
        RT.renderHTML  node @?= "<div aria-controls=\"a b\"></div>"

    , testCase "aria-describedby" $ do
        let node = divWith [A.aria_describedby [Types.Id (NET.singleton 'd')]]
        RBS.renderHTML node @?= "<div aria-describedby=\"d\"></div>"
        RT.renderHTML  node @?= "<div aria-describedby=\"d\"></div>"

    , testCase "aria-description" $ do
        let node = divWith [A.aria_description "A longer description"]
        RBS.renderHTML node @?= "<div aria-description=\"A longer description\"></div>"
        RT.renderHTML  node @?= "<div aria-description=\"A longer description\"></div>"

    , testCase "aria-details" $ do
        let node = divWith [A.aria_details [Types.Id (NET.singleton 'd')]]
        RBS.renderHTML node @?= "<div aria-details=\"d\"></div>"
        RT.renderHTML  node @?= "<div aria-details=\"d\"></div>"

    , testCase "aria-keyshortcuts" $ do
        let node = divWith [A.aria_keyshortcuts "Alt+Shift+P"]
        RBS.renderHTML node @?= "<div aria-keyshortcuts=\"Alt+Shift+P\"></div>"
        RT.renderHTML  node @?= "<div aria-keyshortcuts=\"Alt+Shift+P\"></div>"

    , testCase "aria-level" $ do
        let node = divWith [A.aria_level (2 :: Positive)]
        RBS.renderHTML node @?= "<div aria-level=\"2\"></div>"
        RT.renderHTML  node @?= "<div aria-level=\"2\"></div>"

    , testCase "aria-modal (True)" $ do
        let node = divWith [A.aria_modal True]
        RBS.renderHTML node @?= "<div aria-modal=\"true\"></div>"
        RT.renderHTML  node @?= "<div aria-modal=\"true\"></div>"

    , testCase "aria-multiline (True)" $ do
        let node = divWith [A.aria_multiline True]
        RBS.renderHTML node @?= "<div aria-multiline=\"true\"></div>"
        RT.renderHTML  node @?= "<div aria-multiline=\"true\"></div>"

    , testCase "aria-multiselectable (True)" $ do
        let node = divWith [A.aria_multiselectable True]
        RBS.renderHTML node @?= "<div aria-multiselectable=\"true\"></div>"
        RT.renderHTML  node @?= "<div aria-multiselectable=\"true\"></div>"

    , testCase "aria-owns" $ do
        let node = divWith [A.aria_owns [Types.Id (NET.singleton 'o')]]
        RBS.renderHTML node @?= "<div aria-owns=\"o\"></div>"
        RT.renderHTML  node @?= "<div aria-owns=\"o\"></div>"

    , testCase "aria-placeholder" $ do
        let node = divWith [A.aria_placeholder "Enter a value"]
        RBS.renderHTML node @?= "<div aria-placeholder=\"Enter a value\"></div>"
        RT.renderHTML  node @?= "<div aria-placeholder=\"Enter a value\"></div>"

    , testCase "aria-posinset" $ do
        let node = divWith [A.aria_posinset (1 :: Positive)]
        RBS.renderHTML node @?= "<div aria-posinset=\"1\"></div>"
        RT.renderHTML  node @?= "<div aria-posinset=\"1\"></div>"

    , testCase "aria-readonly (True)" $ do
        let node = divWith [A.aria_readonly True]
        RBS.renderHTML node @?= "<div aria-readonly=\"true\"></div>"
        RT.renderHTML  node @?= "<div aria-readonly=\"true\"></div>"

    , testCase "aria-required (True)" $ do
        let node = divWith [A.aria_required True]
        RBS.renderHTML node @?= "<div aria-required=\"true\"></div>"
        RT.renderHTML  node @?= "<div aria-required=\"true\"></div>"

    , testCase "aria-rowcount" $ do
        let node = divWith [A.aria_rowcount (10 :: Integer)]
        RBS.renderHTML node @?= "<div aria-rowcount=\"10\"></div>"
        RT.renderHTML  node @?= "<div aria-rowcount=\"10\"></div>"

    , testCase "aria-rowindex" $ do
        let node = divWith [A.aria_rowindex (1 :: Positive)]
        RBS.renderHTML node @?= "<div aria-rowindex=\"1\"></div>"
        RT.renderHTML  node @?= "<div aria-rowindex=\"1\"></div>"

    , testCase "aria-rowspan" $ do
        let node = divWith [A.aria_rowspan (2 :: Natural)]
        RBS.renderHTML node @?= "<div aria-rowspan=\"2\"></div>"
        RT.renderHTML  node @?= "<div aria-rowspan=\"2\"></div>"

    , testCase "aria-selected (True)" $ do
        let node = divWith [A.aria_selected True]
        RBS.renderHTML node @?= "<div aria-selected=\"true\"></div>"
        RT.renderHTML  node @?= "<div aria-selected=\"true\"></div>"

    , testCase "aria-setsize" $ do
        let node = divWith [A.aria_setsize (5 :: Integer)]
        RBS.renderHTML node @?= "<div aria-setsize=\"5\"></div>"
        RT.renderHTML  node @?= "<div aria-setsize=\"5\"></div>"

    , testCase "aria-valuemax" $ do
        let node = divWith [A.aria_valuemax (HTML.numberFromIntegral (100 :: Int))]
        RBS.renderHTML node @?= "<div aria-valuemax=\"100\"></div>"
        RT.renderHTML  node @?= "<div aria-valuemax=\"100\"></div>"

    , testCase "aria-valuemin" $ do
        let node = divWith [A.aria_valuemin (HTML.numberFromIntegral (0 :: Int))]
        RBS.renderHTML node @?= "<div aria-valuemin=\"0\"></div>"
        RT.renderHTML  node @?= "<div aria-valuemin=\"0\"></div>"

    , testCase "aria-valuenow" $ do
        let node = divWith [A.aria_valuenow (HTML.numberFromIntegral (50 :: Int))]
        RBS.renderHTML node @?= "<div aria-valuenow=\"50\"></div>"
        RT.renderHTML  node @?= "<div aria-valuenow=\"50\"></div>"

    , testCase "aria-valuetext" $ do
        let node = divWith [A.aria_valuetext "50%"]
        RBS.renderHTML node @?= "<div aria-valuetext=\"50%\"></div>"
        RT.renderHTML  node @?= "<div aria-valuetext=\"50%\"></div>"

    , testCase "aria (raw)" $ do
        let node = divWith [A.aria (NET.new 'l' "ive") "polite"]
        RBS.renderHTML node @?= "<div aria-live=\"polite\"></div>"
        RT.renderHTML  node @?= "<div aria-live=\"polite\"></div>"
    ]

-- Event Attributes
--

eventAttributes :: TestTree
eventAttributes =
  testGroup "Event attributes"
    [ testCase "on click" $ do
        let node = divWith [A.on A.Click (RawJavaScript "alert()")]
        RBS.renderHTML node @?= "<div onclick=\"js:alert()\"></div>"
        RT.renderHTML  node @?= "<div onclick=\"js:alert()\"></div>"

    , testCase "on submit" $ do
        let node = E.form [A.on A.Submit (RawJavaScript "return false")] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<form onsubmit=\"js:return false\"></form>"
        RT.renderHTML  node @?= "<form onsubmit=\"js:return false\"></form>"

    , testCase "on mouseover" $ do
        let node = divWith [A.on A.MouseOver (RawJavaScript "this.style.color='red'")]
        RBS.renderHTML node @?= "<div onmouseover=\"js:this.style.color='red'\"></div>"
        RT.renderHTML  node @?= "<div onmouseover=\"js:this.style.color='red'\"></div>"
    ]

-- HTMX Attributes
--

htmxAttributes :: TestTree
htmxAttributes =
  testGroup "HTMX attributes"
    [ testCase "hx-get" $ do
        let node = divWith [A.hxGet (Types.get NoPathParams (R.make NoPathParams /- "items"))]
        RBS.renderHTML node @?= "<div hx-get=\"/items\"></div>"
        RT.renderHTML  node @?= "<div hx-get=\"/items\"></div>"

    , testCase "hx-post" $ do
        let node = divWith [A.hxPost (Types.post NoPathParams (R.make NoPathParams /- "items"))]
        RBS.renderHTML node @?= "<div hx-post=\"/items\"></div>"
        RT.renderHTML  node @?= "<div hx-post=\"/items\"></div>"

    , testCase "hx-delete" $ do
        let node = divWith [A.hxDelete (Types.delete NoPathParams (R.make NoPathParams /- "items"))]
        RBS.renderHTML node @?= "<div hx-delete=\"/items\"></div>"
        RT.renderHTML  node @?= "<div hx-delete=\"/items\"></div>"

    , testCase "hx-patch" $ do
        let node = divWith [A.hxPatch (Types.patch NoPathParams (R.make NoPathParams /- "items"))]
        RBS.renderHTML node @?= "<div hx-patch=\"/items\"></div>"
        RT.renderHTML  node @?= "<div hx-patch=\"/items\"></div>"

    , testCase "hx-put" $ do
        let node = divWith [A.hxPut (Types.put NoPathParams (R.make NoPathParams /- "items"))]
        RBS.renderHTML node @?= "<div hx-put=\"/items\"></div>"
        RT.renderHTML  node @?= "<div hx-put=\"/items\"></div>"

    , testCase "hx-on (html event)" $ do
        let node = divWith [A.hxOn HTML.ClickEvent "doThing()"]
        RBS.renderHTML node @?= "<div hx-on-click=\"doThing()\"></div>"
        RT.renderHTML  node @?= "<div hx-on-click=\"doThing()\"></div>"

    , testCase "hx-on (htmx event)" $ do
        let node = divWith [A.hxOn HTML.HtmxLoad "doThing()"]
        RBS.renderHTML node @?= "<div hx-on--load=\"doThing()\"></div>"
        RT.renderHTML  node @?= "<div hx-on--load=\"doThing()\"></div>"

    , testCase "hx-push-url (true)" $ do
        let node = divWith [A.hxPushURL True]
        RBS.renderHTML node @?= "<div hx-push-url=\"true\"></div>"
        RT.renderHTML  node @?= "<div hx-push-url=\"true\"></div>"

    , testCase "hx-push-url (false)" $ do
        let node = divWith [A.hxPushURL False]
        RBS.renderHTML node @?= "<div hx-push-url=\"false\"></div>"
        RT.renderHTML  node @?= "<div hx-push-url=\"false\"></div>"

    , testCase "hx-select" $ do
        let node = divWith [A.hxSelect (Types.Id (NET.singleton 'x'))]
        RBS.renderHTML node @?= "<div hx-select=\"#x\"></div>"
        RT.renderHTML  node @?= "<div hx-select=\"#x\"></div>"

    , testCase "hx-select-oob" $ do
        let node = divWith
                     [ A.hxSelectOOB
                         ( HTML.mkOutOfBandSelect
                             (HTML.mkQuerySelector (Types.Id (NET.singleton 'x')))
                           NEL.:| []
                         )
                     ]
        RBS.renderHTML node @?= "<div hx-select-oob=\"#x\"></div>"
        RT.renderHTML  node @?= "<div hx-select-oob=\"#x\"></div>"

    , testCase "hx-swap" $ do
        let node = divWith [A.hxSwap (HTML.swapInnerHTML HTML.NoModifier)]
        RBS.renderHTML node @?= "<div hx-swap=\"innerHTML\"></div>"
        RT.renderHTML  node @?= "<div hx-swap=\"innerHTML\"></div>"

    , testCase "hx-swap-oob (true)" $ do
        let node = divWith [A.hxSwapOOB (Nothing :: Maybe HTML.SwapStyle)]
        RBS.renderHTML node @?= "<div hx-swap-oob=\"true\"></div>"
        RT.renderHTML  node @?= "<div hx-swap-oob=\"true\"></div>"

    , testCase "hx-swap-oob (style)" $ do
        let node = divWith [A.hxSwapOOB (Just HTML.OuterHTML)]
        RBS.renderHTML node @?= "<div hx-swap-oob=\"outerHTML\"></div>"
        RT.renderHTML  node @?= "<div hx-swap-oob=\"outerHTML\"></div>"

    , testCase "hx-target (this)" $ do
        let node = divWith [A.hxTarget HTML.This]
        RBS.renderHTML node @?= "<div hx-target=\"this\"></div>"
        RT.renderHTML  node @?= "<div hx-target=\"this\"></div>"

    , testCase "hx-trigger" $ do
        let node = divWith
                     [ A.hxTrigger
                         ( HTML.mkTrigger
                             (HTML.mkTriggerEvent (HTML.mkEvent HTML.ClickEvent) Nothing [])
                           NEL.:| []
                         )
                     ]
        RBS.renderHTML node @?= "<div hx-trigger=\"click\"></div>"
        RT.renderHTML  node @?= "<div hx-trigger=\"click\"></div>"

    , testCase "hx-vals" $ do
        let node = divWith [A.hxVals (HTML.InlineJSON "{}")]
        RBS.renderHTML node @?= "<div hx-vals=\"{}\"></div>"
        RT.renderHTML  node @?= "<div hx-vals=\"{}\"></div>"

    , testCase "hx-boost (true)" $ do
        let node = divWith [A.hxBoost True]
        RBS.renderHTML node @?= "<div hx-boost=\"true\"></div>"
        RT.renderHTML  node @?= "<div hx-boost=\"true\"></div>"

    , testCase "hx-boost (false)" $ do
        let node = divWith [A.hxBoost False]
        RBS.renderHTML node @?= "<div hx-boost=\"false\"></div>"
        RT.renderHTML  node @?= "<div hx-boost=\"false\"></div>"

    , testCase "hx-confirm" $ do
        let node = divWith [A.hxConfirm "Are you sure?"]
        RBS.renderHTML node @?= "<div hx-confirm=\"Are you sure?\"></div>"
        RT.renderHTML  node @?= "<div hx-confirm=\"Are you sure?\"></div>"

    , testCase "hx-disable (true)" $ do
        let node = divWith [A.hxDisable True]
        RBS.renderHTML node @?= "<div hx-disable></div>"
        RT.renderHTML  node @?= "<div hx-disable></div>"

    , testCase "hx-disable (false)" $ do
        let node = divWith [A.hxDisable False]
        RBS.renderHTML node @?= "<div></div>"
        RT.renderHTML  node @?= "<div></div>"

    , testCase "hx-disabled-elt" $ do
        let node = divWith [A.hxDisabledElt (HTML.disableThis NEL.:| [])]
        RBS.renderHTML node @?= "<div hx-disabled-elt=\"this\"></div>"
        RT.renderHTML  node @?= "<div hx-disabled-elt=\"this\"></div>"

    , testCase "hx-disinherit (all)" $ do
        let node = divWith [A.hxDisinherit HTML.DisinheritAll]
        RBS.renderHTML node @?= "<div hx-disinherit=\"*\"></div>"
        RT.renderHTML  node @?= "<div hx-disinherit=\"*\"></div>"

    , testCase "hx-disinherit (list)" $ do
        let node = divWith [A.hxDisinherit (HTML.HxPushURL NEL.:| [HTML.HxPrompt])]
        RBS.renderHTML node @?= "<div hx-disinherit=\"hx-push-url hx-prompt\"></div>"
        RT.renderHTML  node @?= "<div hx-disinherit=\"hx-push-url hx-prompt\"></div>"

    , testCase "hx-encoding" $ do
        let node = divWith [A.hxEncoding]
        RBS.renderHTML node @?= "<div hx-encoding=\"multipart/form-data\"></div>"
        RT.renderHTML  node @?= "<div hx-encoding=\"multipart/form-data\"></div>"

    , testCase "hx-ext (single)" $ do
        let node = divWith [A.hxExt (HTML.extJsonEnc NEL.:| [])]
        RBS.renderHTML node @?= "<div hx-ext=\"json-enc\"></div>"
        RT.renderHTML  node @?= "<div hx-ext=\"json-enc\"></div>"

    , testCase "hx-ext (multiple with ignore)" $ do
        let node = divWith [A.hxExt (HTML.extJsonEnc NEL.:| [HTML.ignore HTML.extAjaxHeader])]
        RBS.renderHTML node @?= "<div hx-ext=\"json-enc,ignore:ajax-header\"></div>"
        RT.renderHTML  node @?= "<div hx-ext=\"json-enc,ignore:ajax-header\"></div>"

    , testCase "hx-headers" $ do
        let node = divWith [A.hxHeaders HTML.emptyRequestHeaders]
        RBS.renderHTML node @?= "<div hx-headers=\"{}\"></div>"
        RT.renderHTML  node @?= "<div hx-headers=\"{}\"></div>"

    , testCase "hx-history" $ do
        let node = divWith [A.hxHistory]
        RBS.renderHTML node @?= "<div hx-history=\"false\"></div>"
        RT.renderHTML  node @?= "<div hx-history=\"false\"></div>"

    , testCase "hx-history-elt" $ do
        let node = divWith [A.hxHistoryElt]
        RBS.renderHTML node @?= "<div hx-history></div>"
        RT.renderHTML  node @?= "<div hx-history></div>"

    , testCase "hx-include" $ do
        let node = divWith [A.hxInclude HTML.includeThis]
        RBS.renderHTML node @?= "<div hx-include=\"this\"></div>"
        RT.renderHTML  node @?= "<div hx-include=\"this\"></div>"

    , testCase "hx-indicator" $ do
        let node = divWith [A.hxIndicator (HTML.indicateSelector (Types.Id (NET.singleton 'x')))]
        RBS.renderHTML node @?= "<div hx-indicator=\"#x\"></div>"
        RT.renderHTML  node @?= "<div hx-indicator=\"#x\"></div>"

    , testCase "hx-params (all)" $ do
        let node = divWith [A.hxParams HTML.AllParams]
        RBS.renderHTML node @?= "<div hx-params=\"*\"></div>"
        RT.renderHTML  node @?= "<div hx-params=\"*\"></div>"

    , testCase "hx-params (none)" $ do
        let node = divWith [A.hxParams HTML.NoParams]
        RBS.renderHTML node @?= "<div hx-params=\"none\"></div>"
        RT.renderHTML  node @?= "<div hx-params=\"none\"></div>"

    , testCase "hx-preserve (true)" $ do
        let node = divWith [A.hxPreserve True]
        RBS.renderHTML node @?= "<div hx-preserve></div>"
        RT.renderHTML  node @?= "<div hx-preserve></div>"

    , testCase "hx-preserve (false)" $ do
        let node = divWith [A.hxPreserve False]
        RBS.renderHTML node @?= "<div></div>"
        RT.renderHTML  node @?= "<div></div>"

    , testCase "hx-prompt" $ do
        let node = divWith [A.hxPrompt "Enter a value"]
        RBS.renderHTML node @?= "<div hx-prompt=\"Enter a value\"></div>"
        RT.renderHTML  node @?= "<div hx-prompt=\"Enter a value\"></div>"

    , testCase "hx-replace-url" $ do
        let node = divWith [A.hxReplaceURL True]
        RBS.renderHTML node @?= "<div hx-replace-url=\"true\"></div>"
        RT.renderHTML  node @?= "<div hx-replace-url=\"true\"></div>"

    , testCase "hx-validate" $ do
        let node = E.form [A.hxValidate] [] :: E.ChildHTML E.Body E.Html
        RBS.renderHTML node @?= "<form hx-validate></form>"
        RT.renderHTML  node @?= "<form hx-validate></form>"
    ]

-- Hyperscript Attributes
--

hyperscriptAttributes :: TestTree
hyperscriptAttributes =
  testGroup "Hyperscript attributes"
    [ testCase "hyperscript (_)" $ do
        let node = divWith [A.hyperscript (HTML.HyperScript "on click toggle .active")]
        RBS.renderHTML node @?= "<div _=\"on click toggle .active\"></div>"
        RT.renderHTML  node @?= "<div _=\"on click toggle .active\"></div>"
    ]
