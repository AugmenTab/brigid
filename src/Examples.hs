module Examples
  ( documentExample
  , example
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
import Data.Text qualified as T

import HTML.Attributes qualified as A
import HTML.Elements qualified as E
import HTML.Elements.AddAttribute (addDivisionAttribute)
import HTML.Elements.Ruby qualified as Ruby
import HTML.Elements.Table qualified as Table
import HTML.Types qualified as HTML

documentExample :: E.Document
documentExample =
  E.html []
    [ E.head []
        [ E.script [ A.crossorigin HTML.Anonymous ] "This is a test!"
        , E.link
            [ A.href . exampleURL $ GetCustomer 1
            ]
     -- , E.link [ A.href $ HTML.idFromText "bad-link" ] -- This fails, because Id is not a valid href type for link.
        ]
    , E.body [ A.hxBoost True
             , A.customAttribute "myCoolAttribute" "myCoolValue"
             , A.customAttribute "anotherCoolAttr" "anotherCoolValue"
          -- , A.crossorigin HTML.Anonymous -- This fails, because crossorigin is not a valid attribute for body.
             ]
        [ E.header []
            [ E.text "This is the header"
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
  HTML.get route $
    R.make GetCustomer
      /- "customers"
      /+ R.Param (R.coerceParam $ R.intParam "customerId") getCustomerId

divId :: HTML.Id
divId = HTML.Id "div1"

example :: E.ChildHTML E.Body grandparent
example =
  E.div [ A.id divId
        , A.styles [ "color:blue", "font-size:2em" ]
        ]
    [ E.noElement
    , E.comment "First comment"
    , E.form [ A.hxValidate ]
        [ E.button [] []
        , E.input []
     -- , E.input [ A.hxValidate ] -- This fails, because hx-validate is only valid on form elements.
     -- , E.form [] [] -- This fails, because `form` is removed from flow content for valid children of form.
        ]
    , E.div [ A.tabindex HTML.NotReachable ]
        [ E.p [ {- A.width 100, -} A.unsafeTabIndex 4 ]
            [ E.noElement
            , E.comment "Second comment"
            , E.customHTML
                "my-custom-element"
                [ A.id $ HTML.Id "my-custom-elem-id" ]
                  ( Right
                      [ E.script
                          [ A.customData "my-custom-elem-attr" "test" ]
                          T.empty
                      ]
                  )
            ]
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
                [ Ruby.ruby [] "明日" "Ashita"
                ]
            , E.li []
                [ Ruby.ruby [ A.hidden ] "忍者" "Ninja"
                ]
            , E.li []
                [ E.a [ A.href divId ]
                    [ E.text "Back to top"
                    ]
                ]
            , E.comment "Fourth comment"
         -- , E.div [] [] -- This fails, because div isn't allowed in ul.
            ]
        ]
    ]

listExample :: [E.ChildHTML E.Division grandparent]
listExample =
  let testDiv =
        flip addDivisionAttribute (A.id $ HTML.Id "added-later")
          . E.div []
          . L.intersperse (E.text " ")
          . (E.text "First text" :)
          . (<> [ E.text "Last text" ])
          $ [ E.text "Middle text" ]

   in [ testDiv
      , E.button [ A.disabled ]
          [ E.rawHTML "{{ Fake mustache code here! }}"
          ]
      , E.p [] [ E.text "This is some paragraph text." ]
      , E.img [ A.draggable False ]
      ]

transparencyExample :: E.ChildHTML E.Division grandparent
transparencyExample =
  E.a []
    [ E.img []
 -- , E.a [] [] -- This fails, because a is excluded from the transparent content that a holds.
 -- , E.li [] [] -- This fails, because li isn't a valid child for the grandparent div, and a is transparent.
    , E.div [] []
    , E.audio [] []
    ]

tableWithBodyExample :: E.ChildHTML E.Division grandparent
tableWithBodyExample =
  tableExample
    . Left
    $ [ Table.body []
          [ Table.row []
              [ E.td [] [ E.text "1" ]
              , E.td [] [ E.text "2" ]
              , E.td [] [ E.text "3" , E.noElement ]
              , E.td [] [ E.text "4" ]
              , E.td [] [ E.text "5" ]
              , E.td [] [ E.text "6" ]
              ]
          ]
      ]

tableWithRowExample :: E.ChildHTML E.Division grandparent
tableWithRowExample =
  tableExample
    . Right
    . L.replicate 5
    $ Table.row []
        [ E.td [] [ E.text "1" ]
        , E.td [] [ E.text "2" ]
        , E.td [] [ E.text "3" ]
        , E.td [] [ E.text "4" ]
        , E.td [] [ E.text "5" ]
        , E.td [] [ E.text "6" , E.noElement ]
        ]

tableExample :: Either [Table.Body] [Table.Row E.Table E.Division]
             -> E.ChildHTML E.Division grandparent
tableExample content =
  let caption =
        Just $
          Table.caption []
            [ E.text "This is the table caption."
            ]

      colgroups =
        L.replicate 2
          . Table.colgroup []
          . L.replicate 3
          $ E.col []

      head =
        Just $
          Table.head []
            [ Table.row []
                [ E.th [] [ E.text "1" ]
                , E.th [] [ E.text "2" , E.noElement ]
                , E.th [] [ E.text "3" ]
                , E.th [] [ E.text "4" ]
                , E.th [] [ E.text "5" , E.comment "Fifth comment" ]
                , E.th [] [ E.text "6" ]
                ]
            ]

      foot =
        Just $
          Table.foot []
            [ Table.row []
                [ E.td [] [ E.comment "Sixth comment", E.text "1" ]
                , E.td [] [ E.text "2" ]
                , E.td [] [ E.text "3" ]
                , E.td [] [ E.noElement, E.text "4" ]
                , E.td [] [ E.text "5" ]
                , E.td [] [ E.text "6" , E.noElement ]
                ]
            ]

   in Table.table
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
      /+ R.Param (R.coerceParam $ R.intParam "customerId") deleteCustomerId

-- This example demonstrates HTMX content.
htmxExample :: E.ChildHTML E.Body grandparent
htmxExample =
  E.div []
    [ E.button [ A.htmx . exampleURL $ GetCustomer 2
               , A.hxPushURL True
               , A.hxExt $ HTML.extJsonEnc :| [ HTML.ignore HTML.extAjaxHeader ]
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
    HTML.table
      Nothing
      [ HTML.toClassSelector myClass ]
      ( Just $
          HTML.tbody
            Nothing
            []
            ( Just $
                HTML.td
                  (Just $ HTML.disabled)
                  [ HTML.not . HTML.toClassSelector $ HTML.Class "main" ]
                  Nothing
            )
      )
