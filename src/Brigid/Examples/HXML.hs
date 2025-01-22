module Brigid.Examples.HXML
  ( documentExample
  ) where

import Brigid.HXML.Attributes qualified as A
import Brigid.HXML.Elements qualified as E
import Brigid.HXML.Types qualified as HXML

documentExample :: E.HXML
documentExample =
  E.doc [ A.xmlns hyperviewNamespace ]
    [ E.screen [ A.id $ HXML.Id "my-screen" ]
        [ E.styles []
            [ E.style [ -- TODO: A.id myStyleId
                      ]
                [
                ]
            , E.style [] []
            , E.style [] []
            ]
        , E.header [ A.hide True
                   , A.safeArea False
                   , A.style myStyleId
                   ]
            [
            ]
        , E.body [ A.scroll True
                 , A.scrollOrientation HXML.Horizontal
                 , A.showsScrollIndicator True
                 ]
            [ E.view [ A.avoidKeyboard False
                     , A.contentContainerStyle [ myStyleId ]
                     , A.keyboardDismissMode HXML.OnDrag
                     , A.scrollToInputOffset 50
                     , A.sticky True
                     ]
                [ E.text []
                ]
            ]
        ]
    ]

hyperviewNamespace :: HXML.RawURL
hyperviewNamespace =
  HXML.mkRawURL "https://hyperview.org/hyperview"

myStyleId :: HXML.Id
myStyleId = HXML.Id "MyStyle"
