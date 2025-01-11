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
import Beeline.HTTP.Client qualified as B
import Beeline.Routing ((/-), (/+))
import Beeline.Routing qualified as R
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NEL
import Data.Maybe (fromMaybe)
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
import Brigid.HTML.Entities qualified as Entity
import Brigid.HTML.HTMX.Config qualified as HTMX
import Brigid.HTML.Types qualified as HTML

documentExample :: E.Document
documentExample =
  E.html [ A.xmlns fakeJavaScriptLink ]
    [ E.head []
        [ E.script [ A.crossorigin HTML.Anonymous ] $
            NET.fromText "This is a test!"
        , Safe.meta Safe.Charset
        , Safe.meta
            . Safe.Name
            . Safe.ColorScheme
            . Safe.Palettes
            $ NEL.singleton Safe.Dark
        , Safe.meta . Safe.Name $ Safe.Referrer HTML.NoReferrer
        , E.meta [ A.name "viewport"
                 , A.content "width=device-width, initial-scale=1"
                 ]
        , E.link
            [ A.rel HTML.Rel_Stylesheet
            , A.href . exampleURL $ GetCustomer 1
            ]
        , HTMX.setConfig $
            HTMX.defaultConfig
              { HTMX.refreshOnHistoryMiss = Just True
              }
        -- , E.link [ A.href $ HTML.Id "bad-link" ] -- This fails, because Id is not a valid href type for link.
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

exampleURL :: GetCustomer -> HTML.RelativeURL HTML.Get
exampleURL route =
  HTML.get route exampleRoute

exampleRoute :: R.Router r => R.Builder r GetCustomer GetCustomer
exampleRoute =
  R.make GetCustomer
    /- "customers"
    /+ R.Param (R.coerceParam $ R.intParam "customerId") getCustomerId

divId :: HTML.Id
divId = HTML.Id "div1"

fakeJavaScriptLink :: HTML.RawURL
fakeJavaScriptLink =
  HTML.mkRawURL "my/endpoint/file.js"

exampleDate :: Time.Day
exampleDate =
  Time.fromGregorian 2024 12 16

data Ping = Ping

pingURL :: HTML.RelativeURL HTML.Post
pingURL =
  HTML.post Ping $
    R.make Ping
      /- "customers"
      /- "log_access"

example :: E.ChildHTML E.Body grandparent
example =
  E.div [ A.id divId
        , A.styles [ "color:blue", "font-size:2em" ]
        , A.writingsuggestions True
        ]
    [ E.noElement
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
    , E.form [ A.hxValidate
             , A.acceptCharset
             , A.validate False
             , A.method HTML.FormPOST
             ]
        [ E.button [ A.hyperscript sampleHyperScript ]
            [ E.text "Do HyperScript" ]
        , E.div [] []
        , E.input [ A.type_ HTML.InputNumber, A.list divId ]
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
        , Safe.submit [ A.formmethod HTML.FormGET ]
        ]
    , E.textarea [ A.cols 80
                 , A.rows 5
                 , A.writingsuggestions False
                 , A.wrap HTML.Hard
                 ]
        [
        ]
    , E.div [ A.tabindex HTML.NotReachable ]
        [ E.p [ {- A.width 100, -} A.unsafeTabIndex 4 ]
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
                [ A.id $ HTML.Id "my-custom-elem-id" ]
                  ( Right
                      [ E.script [ A.src fakeJavaScriptLink
                                 , A.data_ "my-custom-elem-attr" "test"
                                 ]
                          Nothing
                      ]
                  )
            , E.area [ A.coords $ 10 :| [ 58 ], A.target HTML.Blank ]
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
                  , Safe.areaOtherAttributes = [ A.id $ HTML.Id "safe-area" ]
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
                      , A.ping . NEL.singleton $ HTML.mkPing pingURL
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
                      . fromMaybe (Ogma.bcp_47LanguageToText english)
                      $ Ogma.bcp_47EndonymToText english
                  , A.srclang english
                  ]
        , E.track [ A.label
                      . fromMaybe (Ogma.bcp_47LanguageToText irish)
                      $ Ogma.bcp_47EndonymToText irish
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
          . Safe.colgroup []
          . L.replicate 3
          $ E.col []

      head =
        Just $
          Safe.thead []
            [ Safe.tr []
                [ E.th [] [ E.text "1" ]
                , E.th [] [ E.text "2", E.noElement ]
                , E.th [] [ E.text "3" ]
                , E.th [] [ E.text "4" ]
                , E.th [] [ E.text "5", E.comment "Fifth comment" ]
                , E.th [ A.colspan 0 ] [ E.text "6" ]
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
        [ A.id $ HTML.Id "body-table" ]
        caption
        colgroups
        head
        content
        foot

newtype DeleteCustomer =
  DeleteCustomer
    { deleteCustomerId :: Int
    }

deleteCustomer :: DeleteCustomer -> HTML.RelativeURL HTML.Delete
deleteCustomer route =
  HTML.delete route $
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
                     , HTML.triggerDelay 1
                     ]
               ]
        [ E.text "Implicit Get"
        , E.span [ A.hxDisinherit $ HTML.HxPushURL :| [] ]
            [ E.comment "Disinherit one"
            ]
        ]
    , E.button [ A.hxGet . exampleURL $ GetCustomer 3
               , A.hxBoost False
               , A.hxPrompt $
                   "Customer 3 doesn't like to be disturbed."
                     <> " Really think about what you're doing here."
               , A.hxPushURL
                   . HTML.get B.NoPathParams
                   $ R.make B.NoPathParams /- "time_out"
               , A.hxParams $ HTML.Not [ customerIdParam ]
               , A.hxOn HTML.HtmxLoad "alert(\"Loaded!\")"
               , A.hxTarget idQuerySelectorExample
               , A.hxTrigger
                   . NEL.singleton
                   . HTML.mkTrigger
                   . HTML.every 2
                   . Just
                   $ HTML.TriggerFilter "someConditional"
               ]
        [ E.text "Explicit Get"
        , E.span [ A.hxDisinherit $ HTML.HxPushURL :| [ HTML.HxPrompt ] ]
            [ E.comment "Disinherit two"
            ]
        ]
    , E.button [ A.hxDelete . deleteCustomer $ DeleteCustomer 4
               , A.hxConfirm $
                   "Customer 4 is essential to our future."
                     <> " Are you sure you want to do this?"
               , A.hxReplaceURL
                   . HTML.get B.NoPathParams
                   $ R.make B.NoPathParams /- "doom_page"
               , A.hxDisable $ 'a' < 'T'
               ]
        [ E.text "Delete Customer 4"
        , E.span [ A.hxDisinherit HTML.DisinheritAll ]
            [ E.comment "Disinherit all"
            ]
        ]
    , E.button [ A.id $ HTML.Id "worthless"
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
                   HTML.mkTrigger (HTML.every 1 Nothing)
                     :| [ HTML.mkTrigger $
                            HTML.mkTriggerEvent
                              (HTML.intersectRoot myClass)
                              (Nothing)
                              [ HTML.triggerThrottle 1
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
  HTML.mkQuerySelector $ HTML.Id "myId"

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
