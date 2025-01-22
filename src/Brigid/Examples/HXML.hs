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
    [ E.screen [ A.id $ HXML.Id "my-screen" ]
        [ E.styles []
            [ E.style [] []
            , E.style [] []
            , E.style [] []
            ]
        , E.header [ A.hide True
                   , A.safeArea False
                   , A.style "hidden"
                   ]
            [
            ]
        , E.body [ A.scroll True
                 , A.scrollOrientation HXML.Horizontal
                 , A.showsScrollIndicator True
                 ]
            [ E.text []
            , E.text []
            ]
        ]
    ]
