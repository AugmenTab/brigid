module Main
  ( main
  ) where

import Hedgehog.Internal.Gen (evalGen)
import Hedgehog.Internal.Seed (from)
import Hedgehog.Internal.Tree (nodeValue, runTree)
import Hedgehog.Range (Size (..))
import Lucid.Base (renderBS)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Brigid.HTML.Generation qualified as G
import Brigid.HTML.Generation.Elements (ElementType (Html))
import Brigid.HTML.Render.ByteString (renderLazyHTML)
import Generation.Convert (toBlaze, toLucid)

generateElement :: G.GeneratorParams -> G.Element
generateElement params =
  case evalGen (Size 30) (from 0) (G.generateDOM params) of
    Just nodeTree -> nodeValue $ runTree nodeTree
    Nothing       -> error "DOM generation failed."

fromBrigid :: Either [String] a -> a
fromBrigid (Right x)   = x
fromBrigid (Left errs) = error . unlines $ "toBrigid conversion failed:" : errs

small :: G.GeneratorParams
small =
  G.GeneratorParams
    { G.startingElement   = Html
    , G.maximumTotalNodes = 100
    , G.maximumDepth      = 7
    , G.childrenPerNode   = G.mkRange 0 5
    , G.attributesPerNode = G.mkRange 0 10
    }

medium :: G.GeneratorParams
medium =
  G.GeneratorParams
    { G.startingElement   = Html
    , G.maximumTotalNodes = 1000
    , G.maximumDepth      = 9
    , G.childrenPerNode   = G.mkRange 0 6
    , G.attributesPerNode = G.mkRange 0 10
    }

large :: G.GeneratorParams
large =
  G.GeneratorParams
    { G.startingElement   = Html
    , G.maximumTotalNodes = 10000
    , G.maximumDepth      = 10
    , G.childrenPerNode   = G.mkRange 0 7
    , G.attributesPerNode = G.mkRange 0 10
    }

main :: IO ()
main = do
  let
    smallElement  = generateElement small
    mediumElement = generateElement medium
    largeElement  = generateElement large

    smallBrigid  = fromBrigid $ G.toBrigid smallElement
    mediumBrigid = fromBrigid $ G.toBrigid mediumElement
    largeBrigid  = fromBrigid $ G.toBrigid largeElement

    smallBlaze  = toBlaze smallElement
    mediumBlaze = toBlaze mediumElement
    largeBlaze  = toBlaze largeElement

    smallLucid  = toLucid smallElement
    mediumLucid = toLucid mediumElement
    largeLucid  = toLucid largeElement

  defaultMain
    [ bgroup "render"
        [ bgroup "small"
            [ bench "brigid" $ nf renderLazyHTML smallBrigid
            , bench "blaze"  $ nf renderHtml smallBlaze
            , bench "lucid"  $ nf renderBS smallLucid
            ]
        , bgroup "medium"
            [ bench "brigid" $ nf renderLazyHTML mediumBrigid
            , bench "blaze"  $ nf renderHtml mediumBlaze
            , bench "lucid"  $ nf renderBS mediumLucid
            ]
        , bgroup "large"
            [ bench "brigid" $ nf renderLazyHTML largeBrigid
            , bench "blaze"  $ nf renderHtml largeBlaze
            , bench "lucid"  $ nf renderBS largeLucid
            ]
        ]
    , bgroup "brigid-pipeline"
        [ bgroup "small"
            [ bench "construct+render" $ nf (renderLazyHTML . fromBrigid . G.toBrigid) smallElement
            , bench "render-only"      $ nf renderLazyHTML smallBrigid
            ]
        , bgroup "medium"
            [ bench "construct+render" $ nf (renderLazyHTML . fromBrigid . G.toBrigid) mediumElement
            , bench "render-only"      $ nf renderLazyHTML mediumBrigid
            ]
        , bgroup "large"
            [ bench "construct+render" $ nf (renderLazyHTML . fromBrigid . G.toBrigid) largeElement
            , bench "render-only"      $ nf renderLazyHTML largeBrigid
            ]
        ]
    ]
