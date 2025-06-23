{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  ) where

import Control.Exception (evaluate)
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog.Internal.Gen (evalGen)
import Hedgehog.Internal.Seed (from)
import Hedgehog.Internal.Tree (nodeValue, runTree)
import Hedgehog.Range (Size (..))
import Lucid.Base (renderBS)
import Test.Tasty.Bench qualified as TB
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Brigid.HTML.Elements (AnyHTML)
import Brigid.HTML.Generation qualified as G
import Brigid.HTML.Generation.Elements (ElementType (Html))
import Brigid.HTML.Render.ByteString (renderLazyHTML)
import Generation.Convert (toBlaze, toLucid)

main :: IO ()
main = do
  let
    size = Size 30
    seed = from 0
    doms =
      flip fmap testParams $ \testCase ->
        maybe
          (error "Structure generation failed.")
          (nodeValue . runTree)
          (evalGen size seed $ G.generateDOM testCase)

    blazes = toBlaze <$> doms
    lucids = toLucid <$> doms

  brigids <- mapM constructBrigid doms

  TB.defaultMain
    [ TB.bgroup "Constructing HTML with libraries"
        [ TB.bgroup "Blaze"
            [ TB.bench (show testSize) $ TB.whnf toBlaze testDOM
            | (testSize, testDOM) <- Map.toAscList doms
            ]
        , TB.bgroup "Lucid"
            [ TB.bench (show testSize) $ TB.whnf toLucid testDOM
            | (testSize, testDOM) <- Map.toAscList doms
            ]
        , TB.bgroup "Brigid"
            [ TB.bench (show testSize) . TB.whnfIO $ constructBrigid testDOM
            | (testSize, testDOM) <- Map.toAscList doms
            ]
        ]
    , TB.bgroup "Rendering HTML to lazy ByteString with libraries"
        [ TB.bgroup "Blaze"
            [ TB.bench (show testSize)
                . TB.whnfIO
                . evaluate
                $! LBS.length (renderHtml testDOM)
            | (testSize, testDOM) <- Map.toAscList blazes
            ]
        , TB.bgroup "Lucid"
            [ TB.bench (show testSize)
                . TB.whnfIO
                . evaluate
                $! LBS.length (renderBS testDOM)
            | (testSize, testDOM) <- Map.toAscList lucids
            ]
        , TB.bgroup "Brigid"
            [ TB.bench (show testSize)
                . TB.whnfIO
                . evaluate
                $! LBS.length (renderLazyHTML testDOM)
            | (testSize, testDOM) <- Map.toAscList brigids
            ]
        ]
    , TB.bgroup "Constructing and rendering libraries"
        [ TB.bgroup "Blaze"
            [ TB.bench (show testSize) $ TB.whnf (renderHtml . toBlaze) testDOM
            | (testSize, testDOM) <- Map.toAscList doms
            ]
        , TB.bgroup "Lucid"
            [ TB.bench (show testSize) $ TB.whnf (renderBS . toLucid) testDOM
            | (testSize, testDOM) <- Map.toAscList doms
            ]
        , TB.bgroup "Brigid"
            [ TB.bench (show testSize)
                . TB.whnfIO
                . fmap renderLazyHTML
                $ constructBrigid testDOM
            | (testSize, testDOM) <- Map.toAscList doms
            ]
        ]
    , TB.bgroup "Construction has linear complexity"
        [
        ]
    , TB.bgroup "Rendering has linear complexity"
        [
        ]
    ]

data TestSize
  = Small
  | Medium
  | Large
  | Stress
  | Absurd
  deriving (Eq, Ord, Show)

testParams :: Map TestSize G.GeneratorParams
testParams =
  Map.fromList
    [ ( Small
      , G.GeneratorParams
          { G.startingElement = Html
          , G.maximumTotalNodes = 100
          , G.maximumDepth = 8
          , G.childrenPerNode = G.mkRange 0 5
          , G.attributesPerNode = G.mkRange 0 10
          }
      )
    , ( Medium
      , G.GeneratorParams
          { G.startingElement = Html
          , G.maximumTotalNodes = 1000
          , G.maximumDepth = 9
          , G.childrenPerNode = G.mkRange 0 6
          , G.attributesPerNode = G.mkRange 0 10
          }
      )
    , ( Large
      , G.GeneratorParams
          { G.startingElement = Html
          , G.maximumTotalNodes = 10000
          , G.maximumDepth = 10
          , G.childrenPerNode = G.mkRange 0 7
          , G.attributesPerNode = G.mkRange 0 10
          }
      )
 -- , ( Stress
 --   , G.GeneratorParams
 --       { G.startingElement = Html
 --       , G.maximumTotalNodes = 100000
 --       , G.maximumDepth = 11
 --       , G.childrenPerNode = G.mkRange 0 8
 --       , G.attributesPerNode = G.mkRange 0 10
 --       }isLinear :: (NFData b, Show a, Enum a, Num a, Ord a) => String -> (a -> b) -> a -> a -> TestTree
 --   )
 -- , ( Absurd
 --   , G.GeneratorParams
 --       { G.startingElement = Html
 --       , G.maximumTotalNodes = 1000000
 --       , G.maximumDepth = 12
 --       , G.childrenPerNode = G.mkRange 0 10
 --       , G.attributesPerNode = G.mkRange 0 10
 --       }
 --   )
    ]

constructBrigid :: G.Element -> IO AnyHTML
constructBrigid =
  either (error . unlines) pure . G.toBrigid
