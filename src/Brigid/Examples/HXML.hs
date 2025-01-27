module Brigid.Examples.HXML
  ( navigatorExample
  , documentExample
  ) where

import Data.Coerce (coerce)

import Brigid.Examples.HTML qualified as HTML
import Brigid.HXML.Attributes qualified as A
import Brigid.HXML.Elements qualified as E
import Brigid.HXML.Entities qualified as Entity
import Brigid.HXML.Types qualified as HXML

navigatorExample :: E.HXML
navigatorExample =
  E.doc [ A.xmlns hyperviewNamespace ]
    [ E.navigator [ A.merge True
                  , A.modal False
                  , A.type_ HXML.Stack
                  ]
        [ E.navRoute []
        ]
    ]

documentExample :: E.HXML
documentExample =
  E.doc [ A.xmlns hyperviewNamespace ]
    [ E.screen [ A.id $ HXML.Id "my-screen" ]
        [ E.styles
            [ E.style [ -- TODO: A.id myStyleId
                      ]
                [
                ]
            , E.style [] []
            , E.style []
                [ E.modifier [ A.focused True
                             , A.pressed True
                             , A.selected True
                             ]
                    [ E.style [] []
                    ]
                ]
            ]
        , E.header [ A.hide True
                   , A.safeArea False
                   , A.style [ myStyleId ]
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
                , E.form []
                    [ E.textField [ A.name $ HXML.Name "random-text"
                                  , A.value "Existing value"
                                  , A.placeholder "Type something..."
                                  , A.placeholderTextColor blue
                                  , A.selectionColor red
                                  , A.selectionHandleColor green
                                  , A.cursorColor yellow
                                  , A.multiline True
                                  , A.keyboardType HXML.NumbersAndPunctuation
                                  , case HXML.mkMask "AA (999) 999-9999 S*S*" of
                                      Left  _err -> A.noAttribute
                                      Right mask -> A.mask mask
                                  , A.autoFocus False
                                  , A.secureText True
                                  ]
                    , E.selectSingle [ A.allowDeselect True
                                     ]
                        [ E.option []
                            [ E.text []
                                [ E.content "Option 1"
                                ]
                            ]
                        , E.option []
                            [ E.text []
                                [ E.content "Option 2"
                                ]
                            ]
                        , E.option []
                            [ E.text []
                                [ E.content "Option 3"
                                ]
                            ]
                        ]
                    , E.pickerField [ A.cancelLabel "Cancel"
                                    , A.doneLabel "Done"
                                    , A.fieldStyle [ myStyleId ]
                                    , A.fieldTextStyle [ myStyleId ]
                                    , A.modalStyle [ myStyleId ]
                                    , A.modalTextStyle [ myStyleId ]
                                    ]
                        [ E.pickerItem [ A.label "Choice 1"
                                       , A.value "1"
                                       ]
                        , E.pickerItem [ A.label "Choice 2"
                                       , A.value "2"
                                       ]
                        ]
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

blue :: HXML.ColorName
blue = HXML.ColorName "blue"

red :: HXML.ColorName
red = HXML.ColorName "red"

green :: HXML.ColorName
green = HXML.ColorName "green"

yellow :: HXML.ColorName
yellow = HXML.ColorName "yellow"
