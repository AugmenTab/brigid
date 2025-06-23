{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hedgehog qualified as HH
import Hedgehog.Internal.Gen (evalGen)
import Hedgehog.Internal.Seed (from)
import Hedgehog.Internal.Tree (nodeValue, runTree)
import Hedgehog.Range (Size (..))
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import Test.Tasty qualified as Tasty
import Test.Tasty.Bench.Fit qualified as Fit
import Test.Tasty.HUnit qualified as TastyHU
import Test.Tasty.Hedgehog qualified as TastyHH

import Brigid.HTML.Generation qualified as Gen
import Brigid.HTML.Generation.Attributes qualified as GA
import Brigid.HTML.Generation.Elements qualified as GE
import Brigid.HTML.Render.ByteString (renderHTML)

main :: IO ()
main = do
  Tasty.defaultMain $
    Tasty.testGroup "Brigid tests"
      [ Tasty.testGroup "Attribute generation doesn't produce exceptions" $
          uncurry mkAttributeTestCase <$> allAttributeGenerators
      , Tasty.testGroup "Generated DOM elements are spec compliant" $
          mkElementTestCase <$> allElements
      , TastyHH.testProperty
          "Large generated DOM is spec compliant"
          largeDOMTest
      , Tasty.testGroup
          "Core Brigid operations are linear complexity"
          complexityTests
      ]

mkAttributeTestCase :: T.Text -> HH.Gen GA.Attribute -> Tasty.TestTree
mkAttributeTestCase attrName gen =
  TastyHH.testProperty (T.unpack attrName) $
    HH.property $ do
      attr <- HH.forAll gen
      HH.evalEitherM . liftIO $ do
        result <- try $ evaluate attr

        pure $
          case result of
            Left (err :: SomeException) -> Left $ show err
            Right _result -> Right ()

allAttributeGenerators :: HH.MonadGen m => [(T.Text, m GA.Attribute)]
allAttributeGenerators =
  [ ("Abbreviation", Gen.abbr)
  , ("Accept", Gen.accept)
  , ("AcceptCharset", Gen.acceptCharset)
  , ("AccessKey", Gen.accesskey)
  , ("Action", Gen.action)
  , ("Allow", Gen.allow)
  , ("Alt", Gen.alt)
  , ("As", Gen.as)
  , ("Async", Gen.async)
  , ("Autocapitalize", Gen.autocapitalize)
  , ("Autocomplete", Gen.autocomplete)
  , ("Autocorrect", Gen.autocorrect)
  , ("Autofocus", Gen.autofocus)
  , ("Autoplay", Gen.autoplay)
  , ("Blocking", Gen.blocking)
  , ("Capture", Gen.capture)
  , ("Charset", Gen.charset)
  , ("Checked", Gen.checked)
  , ("Cite", Gen.cite)
  , ("Class", Gen.class_)
  , ("Cols", Gen.cols)
  , ("Colspan", Gen.colspan)
  , ("Command", Gen.command)
  , ("CommandFor", Gen.commandfor)
  , ("Content", Gen.content)
  , ("ContentEditable", Gen.contenteditable)
  , ("Controls", Gen.controls)
  , ("ControlsList", Gen.controlslist)
  , ("Coords", Gen.coords)
  , ("CrossOrigin", Gen.crossorigin)
  , ("CustomData", Gen.customData)
  , ("Data", Gen.data_)
  , ("Datetime", Gen.datetime)
  , ("Decoding", Gen.decoding)
  , ("Default", Gen.default_)
  , ("Defer", Gen.defer)
  , ("Dir", Gen.dir)
  , ("Dirname", Gen.dirname)
  , ("DisablePictureInPicture", Gen.disablepictureinpicture)
  , ("DisableRemotePlayback", Gen.disableremoteplayback)
  , ("Disabled", Gen.disabled)
  , ("Download", Gen.download)
  , ("Draggable", Gen.draggable)
  , ("ElementTiming", Gen.elementtiming)
  , ("Enctype", Gen.enctype)
  , ("EnterKeyHint", Gen.enterkeyhint)
  , ("ExportParts", Gen.exportparts)
  , ("FetchPriority", Gen.fetchpriority)
  , ("ForLabel", Gen.forLabel)
  , ("ForOutput", Gen.forOutput)
  , ("Form", Gen.form)
  , ("FormAction", Gen.formaction)
  , ("FormEnctype", Gen.formenctype)
  , ("FormMethod", Gen.formmethod)
  , ("FormNoValidate", Gen.formnovalidate)
  , ("FormTarget", Gen.formtarget)
  , ("Headers", Gen.headers)
  , ("Height", Gen.height)
  , ("Hidden", Gen.hidden)
  , ("High", Gen.high)
  , ("Href", Gen.href)
  , ("HrefLang", Gen.hreflang)
  , ("HttpEquiv", Gen.httpEquiv)
  , ("Id", Gen.id)
  , ("ImageSizes", Gen.imagesizes)
  , ("ImageSrcset", Gen.imagesrcset)
  , ("Inert", Gen.inert)
  , ("InputMode", Gen.inputmode)
  , ("Integrity", Gen.integrity)
  , ("Is", Gen.is)
  , ("IsMap", Gen.ismap)
  , ("ItemId", Gen.itemid)
  , ("ItemProp", Gen.itemprop)
  , ("ItemRef", Gen.itemref)
  , ("ItemScope", Gen.itemscope)
  , ("ItemType", Gen.itemtype)
  , ("Kind", Gen.kind)
  , ("Label", Gen.label)
  , ("Lang", Gen.lang)
  , ("List", Gen.list)
  , ("Loading", Gen.loading)
  , ("Loop", Gen.loop)
  , ("Low", Gen.low)
  , ("Max", Gen.max)
  , ("MaxLength", Gen.maxlength)
  , ("Media", Gen.media)
  , ("Method", Gen.method)
  , ("Min", Gen.min)
  , ("MinLength", Gen.minlength)
  , ("Multiple", Gen.multiple)
  , ("Muted", Gen.muted)
  , ("Name", Gen.name)
  , ("NameMeta", Gen.nameMeta)
  , ("NoModule", Gen.nomodule)
  , ("NoValidate", Gen.novalidate)
  , ("Nonce", Gen.nonce)
  , ("Open", Gen.open)
  , ("Optimum", Gen.optimum)
  , ("Part", Gen.part)
  , ("Pattern", Gen.pattern)
  , ("Ping", Gen.ping)
  , ("Placeholder", Gen.placeholder)
  , ("PlaysInline", Gen.playsinline)
  , ("Popover", Gen.popover)
  , ("PopoverTarget", Gen.popovertarget)
  , ("PopoverTargetAction", Gen.popovertargetaction)
  , ("Poster", Gen.poster)
  , ("Preload", Gen.preload)
  , ("ReadOnly", Gen.readonly)
  , ("ReferrerPolicy", Gen.referrerpolicy)
  , ("Rel", Gen.rel)
  , ("Required", Gen.required)
  , ("Reversed", Gen.reversed)
  , ("Role", Gen.role)
  , ("Rows", Gen.rows)
  , ("Rowspan", Gen.rowspan)
  , ("Sandbox", Gen.sandbox)
  , ("Scope", Gen.scope)
  , ("Selected", Gen.selected)
  , ("ShadowRootClonable", Gen.shadowrootclonable)
  , ("ShadowRootDelegatesFocus", Gen.shadowrootdelegatesfocus)
  , ("ShadowRootMode", Gen.shadowrootmode)
  , ("Shape", Gen.shape)
  , ("Size", Gen.size)
  , ("Sizes", Gen.sizes)
  , ("Slot", Gen.slot)
  , ("Span", Gen.span)
  , ("Spellcheck", Gen.spellcheck)
  , ("Src", Gen.src)
  , ("SrcDoc", Gen.srcdoc)
  , ("SrcLang", Gen.srclang)
  , ("SrcSet", Gen.srcset)
  , ("Start", Gen.start)
  , ("Step", Gen.step)
  , ("Style", Gen.style)
  , ("TabIndex", Gen.tabindex)
  , ("Target", Gen.target)
  , ("Title", Gen.title)
  , ("Translate", Gen.translate)
  , ("Type", Gen.type_)
  , ("UseMap", Gen.usemap)
  , ("Value", Gen.value)
  , ("ValueInteger", Gen.valueInteger)
  , ("ValueNumber", Gen.valueNumber)
  , ("Width", Gen.width)
  , ("Wrap", Gen.wrap)
  , ("WritingSuggestions", Gen.writingsuggestions)
  , ("XMLNS", Gen.xmlns)
  ]

mkElementTestCase :: GE.ElementType -> Tasty.TestTree
mkElementTestCase e =
  TastyHH.testProperty (show e) $
    HH.property $ do
      dom <-
        HH.forAll $
          Gen.generateDOM $
            Gen.GeneratorParams
              { Gen.startingElement = e
              , Gen.maximumTotalNodes = 25
              , Gen.maximumDepth = 3
              , Gen.childrenPerNode = Gen.mkRange 1 20
              , Gen.attributesPerNode = Gen.mkRange 1 10
              }

      case Gen.toBrigid dom of
        Left err -> fail $ unlines err
        Right _brigid -> pure ()

allElements :: [GE.ElementType]
allElements =
  [ minBound..maxBound ]

largeDOMTest :: HH.Property
largeDOMTest =
  HH.withTests 1 $
    HH.property $ do
      dom <- HH.forAll . Gen.generateDOM $ mkParams 100000

      case Gen.toBrigid dom of
        Left err -> fail $ unlines err
        Right _brigid -> pure ()

mkParams :: Int -> Gen.GeneratorParams
mkParams nodes =
  Gen.GeneratorParams
    { Gen.startingElement = GE.Html
    , Gen.maximumTotalNodes = nodes
    , Gen.maximumDepth = 11
    , Gen.childrenPerNode = Gen.mkRange 1 20
    , Gen.attributesPerNode = Gen.mkRange 1 20
    }

complexityTests :: [Tasty.TestTree]
complexityTests =
  let
    size = Size 30
    seed = from 0
  in
    [ TastyHU.testCase "Brigid construction and rendering is linear" $ do
        putStrLn "Before gens"
        doms <-
          forM [3000] $ \testCase ->
            case evalGen size seed (Gen.generateDOM $ mkParams testCase) of
              Just tree -> do
                pure . nodeValue $ runTree tree

              Nothing ->
                error "Structure generation failed."

        putStrLn "Before conversions"
        brigids <-
          forM doms $ \dom ->
            time $
              either
                (TastyHU.assertFailure . unlines)
                (evaluate . BS.length . renderHTML)
                (Gen.toBrigid dom)
        putStrLn "After conversions"

        let
          measurements =
            Map.fromList
              $ fmap (first (fromIntegral . Gen.totalNodes))
              $ zip doms brigids

        putStrLn
          . ("Measurements: " <>)
          . show
          . Map.toList
          $ measurements

        let
          complexity =
            Fit.guessComplexity measurements

        TastyHU.assertBool "Construction and rendering is not linear" $
          Fit.isLinear complexity
    ]

time :: IO a -> IO Fit.Measurement
time action = do
  start <- getTime Monotonic
  _ <- action
  end <- getTime Monotonic

  let
    millis = fromIntegral (toNanoSecs $ diffTimeSpec end start) / 1000

  putStrLn $ "Measurement: " <> show millis <> " millis"

  if millis <= 0
    then fail $ "Invalid measurement duration: " <> show millis
    else pure $ Fit.Measurement millis 1
