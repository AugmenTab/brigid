module Data.HTML4
  ( documentExample
  , example
  ) where

-- import Data.Text qualified as T

import Data.HTML4.Attributes qualified as A
import Data.HTML4.Elements qualified as E

documentExample :: E.Document
documentExample =
  E.html []
    [ E.body []
        [ example
        ]
    ]

example :: E.HTML E.Division parent
example =
  E.div [ A.id "div1", A.class_ "myCoolClass" ]
    [ E.div []
        [ E.p [ A.width 100 ]
            []
        , E.ul []
            [ E.li []
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
  , E.p [] []
  , E.img []
  ]
