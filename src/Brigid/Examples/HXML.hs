module Brigid.Examples.HXML
  ( documentExample
  ) where

import Brigid.HXML.Attributes qualified as A
import Brigid.HXML.Elements qualified as E
import Brigid.HXML.Entities qualified as Entity
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
                [ E.list [ A.itemHeight 250
                         , A.keyboardShouldPersistTaps HXML.Handled
                         ]
                    [ E.item []
                        [ E.text [ A.adjustsFontSizeToFit True
                                 , A.numberOfLines 100
                                 , A.preformatted False
                                 , A.selectable True
                                 ]
                            [ E.content "Here's"
                            , Entity.noBreakSpace
                            , E.content "some"
                            , Entity.noBreakSpace
                            , E.content "text!"
                            ]
                        ]
                    ]
                , E.image [ A.source hyperviewNamespace ]
                ]
            ]
        ]
    ]

hyperviewNamespace :: HXML.RawURL
hyperviewNamespace =
  HXML.mkRawURL "https://hyperview.org/hyperview"

myStyleId :: HXML.Id
myStyleId = HXML.Id "MyStyle"
