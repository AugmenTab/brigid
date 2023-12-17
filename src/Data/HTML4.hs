module Data.HTML4
  ( documentExample
  , example
  ) where

-- import Data.Text qualified as T

import Data.HTML4.Attributes qualified as A
import Data.HTML4.Elements qualified as E
import Data.HTML4.Types qualified as HTML

documentExample :: E.Document
documentExample =
  E.html []
    [ E.body []
        [ example
        ]
    ]

example :: E.HTML E.Division parent
example =
  E.div [ A.id "div1"
        , A.styles [ "color:blue", "font-size:2em" ]
        ]
    [ E.div [ A.tabindex HTML.NotReachable ]
        [ E.p [ A.width 100, A.unsafeTabIndex 4 ]
            []
        , E.ul [ A.hidden ]
            [ E.li [ A.classes
                       [ "class-1"
                       , "class-2"
                       , "class-3"
                       ]
                   ]
                [ E.div [] $
                    listExample
                ]
         -- , E.div [] [] -- This fails, because Div produces Flow, not ListItem.
            ]
        ]
    ]

listExample :: [E.ChildHTML E.Division]
listExample =
  [ E.div [] []
  , E.p [] [ E.text "This is some paragraph text." ]
  , E.img [ A.draggable False ]
  ]
