module Brigid.Examples.HXML
  ( documentExample
  ) where

import Brigid.HXML.Elements qualified as E

documentExample :: E.HXML
documentExample =
  E.doc
    [ E.screen
        [ E.styles
            [ E.style []
            , E.style []
            , E.style []
            ]
        , E.body
            [ E.text
            , E.text
            ]
        ]
    ]
