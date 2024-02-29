module Examples
  ( documentExample
  , example
  ) where

import Prelude hiding (head)
import Data.List qualified as L
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
        ]
    , E.body [ A.customAttribute "myCoolAttribute" "myCoolValue"
             , A.customAttribute "anotherCoolAttr" "anotherCoolValue"
          -- , A.crossorigin HTML.Anonymous -- This fails, because crossorigin is not a valid attribute for body.
             ]
        [ E.header []
            [ E.text "This is the header"
         -- , E.footer [] [] -- This fails, because marginals are removed from flow content for valid children of footer.
            ]
        , example
        ]
    ]

example :: E.ChildHTML E.Body grandparent
example =
  E.div [ A.id $ HTML.idFromText "div1"
        , A.styles [ "color:blue", "font-size:2em" ]
        ]
    [ E.noElement
    , E.comment "First comment"
    , E.form []
        [ E.button [] []
        , E.input []
     -- , E.form [] [] -- This fails, because `form` is removed from flow content for valid children of form.
        ]
    , E.div [ A.tabindex HTML.NotReachable ]
        [ E.p [ {- A.width 100, -} A.unsafeTabIndex 4 ]
            [ E.noElement
            , E.comment "Second comment"
            , E.customHTML
                "my-custom-element"
                [ A.id $ HTML.idFromText "my-custom-elem-id" ]
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
            , E.comment "Fourth comment"
         -- , E.div [] [] -- This fails, because div isn't allowed in ul.
            ]
        ]
    ]

listExample :: [E.ChildHTML E.Division grandparent]
listExample =
  let testDiv =
        flip addDivisionAttribute (A.id $ HTML.idFromText "added-later")
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
        [ A.id $ HTML.idFromText "body-table" ]
        caption
        colgroups
        head
        content
        foot
