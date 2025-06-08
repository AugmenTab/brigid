{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brigid.HTML.Generation.Internal.Analysis
  ( totalNodes
  , maxDepth
  , prettyPrint
  ) where

import Data.List.NonEmpty qualified as NEL
import Data.List.NonEmpty.Extra (maximum1)
import Data.NonEmptyText qualified as NET

import Brigid.HTML.Generation.Internal.Types qualified as GT

newtype Count =
  Count
    { unCount :: Int
    } deriving (Eq, Num, Ord)

instance Semigroup Count where
  (<>) = (+)

instance Monoid Count where
  mempty = 0

totalNodes :: GT.Element -> Int
totalNodes =
  unCount . countNodes

countNodes :: GT.Element -> Count
countNodes element =
  case GT.elementChildren element of
    GT.Branch nodes -> Count 1 + foldMap countNodes nodes
    GT.Leaf _net -> Count 1
    GT.Void -> Count 1

maxDepth :: GT.Element -> Int
maxDepth element =
  (+ 1) $
    case GT.elementChildren element of
      GT.Branch nodes -> maybe 0 (maximum1 . fmap maxDepth) (NEL.nonEmpty nodes)
      GT.Leaf _net -> 0
      GT.Void -> 0

prettyPrint :: GT.Element -> String
prettyPrint =
  let
    renderLines :: String -> Bool -> GT.Element -> String
    renderLines prefix isLast (GT.Element tag attrs children) =
      let
        connector =
          if isLast
            then "└── "
            else "├── "

        thisLine = prefix <> connector <> show tag <> " " <> show attrs
        newPrefix =
          mconcat
            [ prefix
            , if isLast
                then "    "
                else "│   "
            ]

        childLines =
          case children of
            GT.Void ->
              []

            GT.Leaf net ->
              [ newPrefix <> "└── " <> show (NET.toText net)
              ]

            GT.Branch es ->
              let
                total = length es
              in
                zipWith
                  (\i e -> renderLines newPrefix (i == total - 1) e)
                  [0 ..]
                  es
      in
        thisLine <> concatMap ('\n' :) childLines
  in
    renderLines "" True
