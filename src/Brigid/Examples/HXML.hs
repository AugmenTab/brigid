module Brigid.Examples.HXML
  ( documentExample
  ) where

import Brigid.HXML.Attributes qualified as A
import Brigid.HXML.Elements qualified as E
import Brigid.HXML.Types qualified as HXML

hyperviewNamespace :: HXML.RawURL
hyperviewNamespace =
  HXML.mkRawURL "https://hyperview.org/hyperview"

documentExample :: E.HXML
documentExample =
  E.doc [ A.xmlns hyperviewNamespace ]
    [ E.screen []
        [ E.styles []
            [ E.style [] []
            , E.style [] []
            , E.style [] []
            ]
        , E.body []
            [ E.text []
            , E.text []
            ]
        ]
    ]
