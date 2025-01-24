module Brigid.Examples.HXML
  ( documentExample
  ) where

import Data.Coerce (coerce)

import Brigid.Examples.HTML qualified as HTML
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
            [ E.spinner [ case HXML.hexColorFromText "#FFFFFF" of
                            Left  _err  -> A.noAttribute
                            Right color -> A.color color
                        ]
            , E.view [ A.avoidKeyboard False
                     , A.contentContainerStyle [ myStyleId ]
                     , A.keyboardDismissMode HXML.OnDrag
                     , A.scrollToInputOffset 50
                     , A.sticky True
                     ]
                [ E.list [ A.itemHeight 250
                         , A.keyboardShouldPersistTaps HXML.Handled
                         ]
                    [ E.items []
                        [ E.item [ A.key $ coerce myStyleId ]
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
                    ]
                , E.image [ A.source hyperviewNamespace ]
                , E.sectionList [ A.stickySectionTitles True
                                ]
                    [ E.section []
                        [ E.sectionTitle []
                            [ E.text []
                                [ E.content "My Section"
                                ]
                            ]
                        ]
                    ]
                , E.webView [ A.activityIndicatorColor $ HXML.ColorName "blue"
                            , A.html HTML.documentExample
                            , A.injectedJavaScript
                                . HXML.RawJavaScript
                                $ "alert('Hello Hyperview user!')"
                            , A.showLoadingIndicator HXML.DocumentOnly
                            , A.url hyperviewNamespace
                            ]
                ]
            ]
        ]
    ]

hyperviewNamespace :: HXML.RawURL
hyperviewNamespace =
  HXML.mkRawURL "https://hyperview.org/hyperview"

myStyleId :: HXML.Id
myStyleId = HXML.Id "MyStyle"
