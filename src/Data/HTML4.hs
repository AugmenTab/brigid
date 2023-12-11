module Data.HTML4
  ( example
  ) where

-- import Data.Text qualified as T

import Data.HTML4.Elements.Tags qualified as Tags
import Data.HTML4.Internal

example :: HTML Tags.Division
example =
  Div [ Id "div1" ]
    [ Div []
        [ P [ {- Width 100 -} ]
            [ A []
                [ Ul []
                    [ Li [] []
                 -- , Div [] [] -- This fails, because Div produces Flow, not ListItem.
                    ]
                ]
            ]
        ]
    ]
