module Data.HTML4
  ( example
  ) where

-- import Data.Text qualified as T

import Data.HTML4.Attributes qualified as A
import Data.HTML4.Elements qualified as E
import Data.HTML4.Elements.Tags qualified as Tags

example :: E.HTML Tags.Division
example =
  E.div [ A.id "div1" ]
    [ E.div []
        [ E.p [ {- Width 100 -} ]
            [ E.a []
                [ E.ul []
                    [ E.li [] []
                 -- , E.div [] [] -- This fails, because Div produces Flow, not ListItem.
                    ]
                ]
            ]
        ]
    ]
