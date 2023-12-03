{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.HTML
  ( example
  ) where

import qualified Data.Text as T

import qualified Data.HTML.Elements as H
import qualified Data.HTML.Attributes as A

example :: H.HTML H.Division parent
example =
  -- This attribute assignment should be failing, but isn't.
  H.div [ A.id $ T.pack "div-id" ]
    [ H.img []
    , H.span []
        [ H.div []
            [ H.b []
                [ H.h1 []
                    -- These should both be failing, but neither is.
                    [ H.a [] []
                    , H.h1 [] []
                    ]
                ]
            ]
        ]
 -- This should be compiling, but isn't.
 -- , H.div []
 --     example2
    -- This should be failing, but it isn't.
    , H.a [ A.id $ T.pack "anchor-id" ]
        [ example1
        ]
    ]

example1 :: H.HTML H.H1 parent
example1 = H.h1 [] [ H.img [] ]

example2 :: [H.HTML H.Division H.Division]
example2 =
  [ H.div [] []
  , H.div [] []
  , H.div [] []
  ]
