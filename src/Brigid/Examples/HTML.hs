module Brigid.Examples.HTML
  ( documentExample
  , example
  , safeScriptExample
  , htmxExample
  , idQuerySelectorExample
  , classQuerySelectorExample
  , elementQuerySelectorExample
  ) where

import Prelude hiding (head)
import Beeline.HTTP.Client (NoPathParams (NoPathParams))
import Beeline.Routing ((/-), (/+))
import Beeline.Routing qualified as R
import Data.Coerce (coerce)
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NEL
import Data.NonEmptyText qualified as NET
import Data.Ratio ((%))
import Data.Text qualified as T
import Data.Time qualified as Time
import Fleece.Core ((#+))
import Fleece.Core qualified as FC
import Ogma qualified

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.Safe qualified as Safe
import Brigid.HTML.Elements.SVG qualified as SVG
import Brigid.HTML.Entities qualified as Entity
import Brigid.HTML.HTMX.Config qualified as HTMX
import Brigid.HTML.Types qualified as HTML
import Brigid.Types qualified as B

documentExample :: E.HTML
documentExample =
  E.html [ A.xmlns fakeJavaScriptLink ]
    [ E.head []
        [ E.script [ A.crossorigin HTML.Anonymous ] $
            NET.fromText "This is a test!"
        , Safe.meta Safe.Charset
        , Safe.meta $ Safe.HttpEquiv Safe.Pragma
        , Safe.meta
            . Safe.Name
            . Safe.ColorScheme
            . Safe.Palettes
            $ NEL.singleton Safe.Dark
        , Safe.meta . Safe.Name $ Safe.Referrer HTML.NoReferrer
        , E.meta [ A.name HTML.Viewport
                 , A.content "width=device-width, initial-scale=1"
                 ]
        , E.link
            [ A.rel HTML.Rel_Stylesheet
            , A.href . exampleURL $ GetCustomer 1
            , A.as HTML.AsStyle
            , A.blocking HTML.Render
            ]
        , HTMX.setConfig $
            HTMX.defaultConfig
              { HTMX.refreshOnHistoryMiss = Just True
              }
        , Safe.baseWithTarget HTML.Self
            [ A.id . B.Id $ NET.new 's' "afe-base"
            ]
        -- , E.link [ A.href . B.Id $ NET.new 'b' "ad-link" ] -- This fails, because Id is not a valid href type for link.
        -- , E.script [ A.src pingURL ] Nothing -- This fails, because RelativeURL Post is not a valid URL type for the src attribute.
        ]
    , E.body [ A.hxBoost True
             , A.customAttribute "anotherCoolAttr" "anotherCoolValue"
             , A.customAttribute "myCoolAttribute" "myCoolValue"
             , A.customAttribute "anotherCoolAttr" "anotherCoolValue" -- This duplicate is removed when rendered - attributes at the front of the list are favored.
          -- , A.crossorigin HTML.Anonymous -- This fails, because crossorigin is not a valid attribute for body.
             ]
        [ E.header []
            [ E.text "Header with entities: "
            , E.customHTML "custom-tag" [] $ Left B.WithTag
            , Entity.latinCapitalLetterTWithCedilla
            , Entity.latinCapitalLetterEWithDiaeresis
            , Entity.latinCapitalLetterSWithCaron
            , Entity.latinCapitalLetterTWithStroke
         -- , E.footer [] [] -- This fails, because marginals are removed from flow content for valid children of footer.
            ]
        , example
        , htmxExample
        ]
    ]

newtype GetCustomer =
  GetCustomer
    { getCustomerId :: Int
    }

exampleURL :: GetCustomer -> B.RelativeURL B.Get
exampleURL route =
  B.get route exampleRoute

exampleRoute :: R.Router r => R.Builder r GetCustomer GetCustomer
exampleRoute =
  R.make GetCustomer
    /- "customers"
    /+ R.Param (R.coerceParam $ R.intParam "customerId") getCustomerId

divId :: B.Id
divId = B.Id $ NET.new 'd' "iv1"

fakeJavaScriptLink :: B.RawURL
fakeJavaScriptLink =
  B.mkRawURL "my/endpoint/file.js"

exampleDate :: Time.Day
exampleDate =
  Time.fromGregorian 2024 12 16

data Ping = Ping

pingURL :: B.RelativeURL B.Post
pingURL =
  B.post Ping $
    R.make Ping
      /- "customers"
      /- "log_access"

formId :: B.Id
formId = B.Id $ NET.new 'm' "y_form"

numberId :: B.Id
numberId = B.Id $ NET.new 'n' "umber"

example :: E.ChildHTML E.Body grandparent
example =
  E.div [ A.id divId
        , A.styles [ "color:blue", "font-size:2em" ]
        , A.writingsuggestions True
        ]
    [ E.noElement
    , E.customHTML "custom-tag" [] $ Left B.WithTag
    , E.blockquote [ A.cite . exampleURL $ GetCustomer 100 ]
        [
        ]
    , E.comment "First comment"
    , E.p []
        [ E.text "On "
        , E.time [ A.datetime exampleDate ]
            [ E.text "the day this was written"
            ]
        , E.text ", we learned about using week numbers to represent "
        , E.time [ A.datetimeWithFormat "%G-W%V-%u" exampleDate ]
            [ E.text "the same date"
            ]
        , E.text "."
        ]
    , E.form [ A.id formId
             , A.hxValidate
             , A.acceptCharset
             , A.validate False
             , A.method B.FormPOST
             ]
        [ E.button [ A.hyperscript sampleHyperScript ]
            [ E.text "log 'Do HyperScript'" ]
        , E.div [] []
        , E.label [ A.for numberId ]
            [ E.text "Number:"
            ]
        , E.input [ A.id numberId
                  , A.type_ HTML.InputNumber
                  , A.list divId
                  , A.form formId
                  , A.autocomplete $ HTML.section "test" HTML.Username
                  ]
        , E.input [ A.type_ HTML.InputFile
                  , A.capture Nothing
                  , A.multiple
                  , A.autocomplete HTML.Off
                  ]
        , E.input [ A.type_ HTML.InputFile
                  , A.capture $ Just HTML.User
                  ]
        , E.input [ A.type_ HTML.InputText
                  , A.size 25
                  , A.pattern "^[a-zA-Z]{3,10}$"
                  ]
        , Safe.image [ A.alt "This is a picture of numbers."
                  -- , A.maxlength 100 -- This fails because length is not a valid attribute for image input.
                     ]
        , Safe.checkbox [ A.checked ]
        , Safe.tel [ A.placeholder "Enter phone number"
                   , A.required
                   , A.dirname "telephone"
                   , A.readonly
                   ]
        , Safe.number [ A.value $ HTML.numberFromFractional (0.75 :: Double) 2
                      ]
        , Safe.range [ A.value $ HTML.numberFromReal (1 % 100 :: Rational) 2 ]
        , Safe.range [ A.value $ HTML.numberFromReal (1 :: Rational) 0 ]
     -- , Safe.checkbox [ A.value fakeJavaScriptLink ] -- This fails because RawURL is not a valid value for InputCheckbox.
     -- , E.input [ A.hxValidate ] -- This fails, because hx-validate is only valid on form elements.
     -- , E.form [] [] -- This fails, because `form` is removed from flow content for valid children of form.
        , Safe.submit [ A.formmethod B.FormGET ]
        ]
    , E.textarea [ A.cols 80
                 , A.rows 5
                 , A.writingsuggestions False
                 , A.wrap HTML.WrapHard
                 ]
        "Here's some text!"
    , E.div [ A.tabindex HTML.NotReachable ]
        [ E.output [ A.for $ divId :| [ numberId ] ]
            [ svgExample
            ]
        , E.p [ {- A.width 100, -} A.unsafeTabIndex 4 ]
            [ E.noElement
            , E.meter [ A.min $ HTML.numberFromIntegral (0 :: Int)
                      , A.low $ HTML.numberFromReal (1 % 4 :: Rational) 2
                      , A.optimum $ HTML.numberFromFractional (1 / 2 :: Double) 1
                      , A.high $ HTML.numberFromReal (3 % 4 :: Rational) 2
                      , A.max $ HTML.numberFromIntegral (1 :: Int)
                      ]
                []
            , E.progress [ A.min $ HTML.numberFromIntegral (0 :: Int)
                         , A.max $ HTML.numberFromIntegral (100 :: Int)
                         ]
                []
            , E.comment "Second comment"
            , E.customHTML
                "my-custom-element"
                [ A.id . B.Id $ NET.new 'm' "y-custom-elem-id" ]
                  ( Right
                      [ E.script [ A.src fakeJavaScriptLink
                                 , A.customData "my-custom-elem-attr" "test"
                                 ]
                          Nothing
                      ]
                  )
            , E.area [ A.coords $ 10 :| [ 58 ]
                     , A.target HTML.Blank
                     , A.href fakeJavaScriptLink
                     , A.hreflang irish
                     ]
            , Safe.area $
                Safe.Area
                  { Safe.areaShape = Just $ Safe.Circle (100, 580) 25
                  , Safe.areaHrefAttributes  =
                      Just $
                        Safe.mkHrefAttributes
                          (exampleURL $ GetCustomer 151)
                          (Just "Your browser does not support this element.")
                          (Just HTML.Rel_Bookmark)
                          (Just HTML.Parent)
                  , Safe.areaOtherAttributes =
                      [ A.id . B.Id $ NET.new 's' "afe-area" ]
                  }
            ]
        , safeScriptExample
        , transparencyExample
        , tableWithBodyExample
        , E.ul []
            [ E.li [ A.classes
                       [ "class-1"
                       , "class-2"
                       , "class-3"
                       ]
                   ]
                [ E.div [] $
                    tableWithRowExample
                      : E.noElement
                      : E.comment "Third comment"
                      : listExample
                ]
            , E.noElement
            , E.li []
                [ Safe.ruby [] "明日" "Ashita"
                ]
            , E.li []
                [ Safe.ruby [ A.hidden ] "忍者" "Ninja"
                ]
            , E.li []
                [ E.a [ A.href divId
                      , A.ping . NEL.singleton $ B.mkPing pingURL
                      ]
                    [ E.text "Back to top"
                    ]
                ]
            , E.comment "Fourth comment"
         -- , E.div [] [] -- This fails, because div isn't allowed in ul.
            ]
        ]
    ]

sampleHyperScript :: HTML.HyperScript
sampleHyperScript =
  HTML.HyperScript
    . T.unwords
    $ [ "on"
      , HTML.eventToText $ HTML.mkEvent HTML.ClickEvent
      , "put 'hello' into the"
      , HTML.querySelectorToText idQuerySelectorExample
      ]

svgExample :: E.ChildHTML E.Output grandparent
svgExample =
  SVG.svg []
    [ SVG.g []
        [ SVG.g []
            [ SVG.path []
                [
                ]
            ]
        ]
    , SVG.g []
        [ SVG.g []
            [ SVG.path []
                [
                ]
            ]
        ]
    ]

listExample :: [E.ChildHTML E.Division grandparent]
listExample =
  let testDiv =
        E.div []
          . L.intersperse (E.text " ")
          . (E.text "First text" :)
          . (<> [ E.text "Last text" ])
          $ [ E.text "Middle text" ]

   in [ testDiv
      , E.button [ A.disabled ]
          [ E.rawHTML "{{ Fake mustache code here! }}"
          ]
      , E.p [] $ E.wbrs "This is some paragraph text."
      , E.img [ A.draggable False
              , A.alt "This describes the image."
              , A.decoding HTML.DecodeAsync
              , A.usemap $ coerce divId
              , A.fetchpriority HTML.FetchHigh
              ]
   -- , Safe.imgMap [] -- This fails because `imgMap` must be used inside an Anchor tag.
      ]

safeScriptExample :: E.ChildHTML E.Division grandparent
safeScriptExample =
  Safe.script $
    (Safe.defaultExternalScript fakeJavaScriptLink)
      { Safe.externalScriptCrossorigin     = Just HTML.Anonymous
      , Safe.externalScriptLoadingBehavior = Safe.Async
      }

english :: Ogma.BCP_47
english = Ogma.simpleBCP_47 Ogma.English

irish :: Ogma.BCP_47
irish = Ogma.simpleBCP_47 Ogma.Irish

transparencyExample :: E.ChildHTML E.Division grandparent
transparencyExample =
  E.a [ A.href fakeJavaScriptLink
      , A.download $ NET.fromText "file_name"
      ]
    [ Safe.imgMap []
 -- , E.a [] [] -- This fails, because a is excluded from the transparent content that a holds.
 -- , E.li [] [] -- This fails, because li isn't a valid child for the grandparent div, and a is transparent.
    , E.div []
        [ E.ol [ A.reversed
               , A.type_ HTML.UppercaseLatinLetters
               , A.start 3
               ]
            [ E.li [] [ E.text "1" ]
            , E.li [] [ E.text "2" ]
            , E.li [] [ E.text "3" ]
            , E.li [] [ E.text "4" ]
            , E.li [] [ E.text "5" ]
            ]
        ]
    , E.audio [ A.controls
              , A.controlslist HTML.NoDownload
              , A.mute False
              , A.disableremoteplayback
              ]
        []
    , E.video [ A.muted
              , A.loop
              , A.preload HTML.PreloadMetadata
              , A.disablepictureinpicture
              , A.playsinline
              , A.poster fakeJavaScriptLink
              ]
        [ E.track [ A.default_
                  , A.label
                      . Ogma.languageToNameText
                      $ Ogma.bcp_47Language english
                  , A.srclang english
                  ]
        , E.track [ A.label
                      . Ogma.languageToNameText
                      $ Ogma.bcp_47Language irish
                  , A.srclang irish
                  , A.kind HTML.Metadata
                  ]
        ]
    ]

tableWithBodyExample :: E.ChildHTML E.Division grandparent
tableWithBodyExample =
  tableExample
    . Left
    $ [ Safe.tbody []
          [ Safe.tr []
              [ E.td [] [ E.text "1" ]
              , E.td [] [ E.text "2" ]
              , E.td [] [ E.text "3", E.noElement ]
              , E.td [] [ E.text "4" ]
              , E.td [ A.rowspan 2 ] [ E.text "5" ]
              ]
          ]
      ]

tableWithRowExample :: E.ChildHTML E.Division grandparent
tableWithRowExample =
  tableExample
    . Right
    . L.replicate 5
    $ Safe.tr []
        [ E.td [] [ E.text "1" ]
        , E.td [] [ E.text "2" ]
        , E.td [] [ E.text "3" ]
        , E.td [] [ E.text "4" ]
        , E.td [] [ E.text "5" ]
        , E.td [] [ E.text "6", E.noElement ]
        ]

tableExample :: Either [Safe.TableBody] [Safe.TableRow E.Table E.Division]
             -> E.ChildHTML E.Division grandparent
tableExample content =
  let caption =
        Just $
          Safe.caption []
            [ E.text "This is the table caption."
            ]

      colgroups =
        L.replicate 2
          . Safe.colgroup [ A.span 1 ]
          . L.replicate 3
          $ E.col []

      head =
        Just $
          Safe.thead []
            [ Safe.tr []
                [ E.th [ A.scope HTML.Col, A.abbr "Example" ] [ E.text "1" ]
                , E.th [ A.scope HTML.Col ] [ E.text "2", E.noElement ]
                , E.th [ A.scope HTML.Col ] [ E.text "3" ]
                , E.th [ A.scope HTML.Col ] [ E.text "4" ]
                , E.th [ A.scope HTML.Col ]
                    [ E.text "5"
                    , E.comment "Fifth comment"
                    ]
                , E.th [ A.scope HTML.Col, A.colspan 0 ] [ E.text "6" ]
                ]
            ]

      foot =
        Just $
          Safe.tfoot []
            [ Safe.tr []
                [ E.td [] [ E.comment "Sixth comment", E.text "1" ]
                , E.td [] [ E.text "2" ]
                , E.td [] [ E.text "3" ]
                , E.td [] [ E.noElement, E.text "4" ]
                , E.td [] [ E.text "5" ]
                , E.td [] [ E.text "6", E.noElement ]
                ]
            ]

   in Safe.table
        [ A.id . B.Id $ NET.new 'b' "ody-table" ]
        caption
        colgroups
        head
        content
        foot

newtype DeleteCustomer =
  DeleteCustomer
    { deleteCustomerId :: Int
    }

deleteCustomer :: DeleteCustomer -> B.RelativeURL B.Delete
deleteCustomer route =
  B.delete route $
    R.make DeleteCustomer
      /- "customers"
      /+ R.Param (R.coerceParam $ R.intParam customerIdParam) deleteCustomerId

customerIdParam :: T.Text
customerIdParam = "customerId"

-- This example demonstrates HTMX content.
htmxExample :: E.ChildHTML E.Body grandparent
htmxExample =
  E.div []
    [ E.button [ A.htmx . exampleURL $ GetCustomer 2
               , A.hxPushURL True
               , A.hxExt $ HTML.extJsonEnc :| [ HTML.ignore HTML.extAjaxHeader ]
               , A.hxSelect divId
               , A.hxParams HTML.AllParams
               , A.hxOn HTML.ClickEvent "alert(\"Hello!\")"
               , A.hxTrigger
                   . NEL.singleton
                   . HTML.mkTrigger
                   . HTML.mkTriggerEvent (HTML.mkEvent HTML.KeyUpEvent) Nothing
                   $ [ HTML.triggerChanged
                     , HTML.triggerDelay 1 HTML.Milliseconds
                     ]
               ]
        [ E.text "Implicit Get"
        , E.span [ A.hxDisinherit $ HTML.HxPushURL :| [] ]
            [ E.comment "Disinherit one"
            ]
        ]
    , E.iframe [ A.sandbox [ HTML.AllowSameOrigin ]
               , A.srcdoc tableWithBodyExample
               ]
    , E.object [ A.data_ . exampleURL $ GetCustomer 200
               ]
        [
        ]
    , E.button [ A.htmx . exampleURL $ GetCustomer 3
               , A.hxBoost False
               , A.hxPrompt $
                   "Customer 3 doesn't like to be disturbed."
                     <> " Really think about what you're doing here."
               , A.hxPushURL
                   . B.get NoPathParams
                   $ R.make NoPathParams /- "time_out"
               , A.hxParams $ HTML.Not [ customerIdParam ]
               , A.hxOn HTML.HtmxLoad "alert(\"Loaded!\")"
               , A.hxTarget idQuerySelectorExample
               , A.hxTrigger
                   . NEL.singleton
                   . HTML.mkTrigger
                   . HTML.every 2 HTML.Seconds
                   . Just
                   $ HTML.TriggerFilter "someConditional"
               ]
        [ E.text "Explicit Get"
        , E.span [ A.hxDisinherit $ HTML.HxPushURL :| [ HTML.HxPrompt ] ]
            [ E.comment "Disinherit two"
            ]
        ]
    , E.button [ A.htmx . deleteCustomer $ DeleteCustomer 4
               , A.hxConfirm $
                   "Customer 4 is essential to our future."
                     <> " Are you sure you want to do this?"
               , A.hxReplaceURL
                   . B.get NoPathParams
                   $ R.make NoPathParams /- "doom_page"
               , A.hxDisable $ 'a' < 'T'
               ]
        [ E.text "Delete Customer 4"
        , E.span [ A.hxDisinherit HTML.DisinheritAll ]
            [ E.comment "Disinherit all"
            ]
        ]
    , E.button [ A.id . B.Id $ NET.new 'w' "orthless"
               , A.hxDisabled
               ]
        [ E.text "I Do Nothing"
        ]
    , E.button [ A.htmx . exampleURL $ GetCustomer 5
               , A.hxVals $ HTML.mkInlineJSON thingSchema exampleThing
               , A.hxTarget $ HTML.htmx_closest myClass
               , A.hxSwap
                   . HTML.swapAfterbegin
                   . Just
                   . HTML.scroll HTML.SwapTop
                   $ Just idQuerySelectorExample
               , A.hxTrigger $
                   HTML.mkTrigger (HTML.every 1 HTML.Minutes Nothing)
                     :| [ HTML.mkTrigger $
                            HTML.mkTriggerEvent
                              (HTML.intersectRoot myClass)
                              (Nothing)
                              [ HTML.triggerThrottle 1 HTML.Hours
                              , HTML.triggerQueue HTML.QueueAll
                              ]
                        , HTML.customTrigger "my-custom-event"
                        ]
               ]
        [ E.text "Vals Test"
        ]
    ]

myClass :: HTML.Class
myClass = HTML.Class "myClass"

idQuerySelectorExample :: HTML.QuerySelector
idQuerySelectorExample =
  HTML.mkQuerySelector . B.Id $ NET.new 'm' "yId"

classQuerySelectorExample :: HTML.QuerySelector
classQuerySelectorExample =
  HTML.mkQuerySelector myClass

elementQuerySelectorExample :: HTML.QuerySelector
elementQuerySelectorExample =
  HTML.mkQuerySelector $
    HTML.tag_table
      Nothing
      [ HTML.toClassSelector myClass ]
      ( Just $
          HTML.tag_tbody
            Nothing
            []
            ( Just $
                HTML.tag_td
                  (Just $ HTML.attr_disabled)
                  [ HTML.not . HTML.toClassSelector $ HTML.Class "main" ]
                  ( Just $
                      HTML.tag_a
                        (Just . HTML.attr_href . exampleURL $ GetCustomer 1)
                        []
                        Nothing
                  )
            )
      )

data Thing =
  Thing
    { thingInt  :: Int
    , thingBool :: Bool
    }

thingSchema :: FC.Fleece schema => schema Thing
thingSchema =
  FC.object $
    FC.constructor Thing
      #+ FC.required "thing_int" thingInt FC.int
      #+ FC.required "thing_bool" thingBool FC.boolean

exampleThing :: Thing
exampleThing =
  Thing
    { thingInt  = 1500
    , thingBool = False
    }
