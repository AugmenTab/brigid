module Generation.Types
  ( GenM
  , GenContext (..)
  , consumeNode
  , GeneratorParams (..)
  ) where

import Control.Monad.Reader (ReaderT, asks, liftIO)
import Data.IORef (IORef, atomicModifyIORef')
import Hedgehog (GenT, Seed)
import Hedgehog.Range (Size)

type GenM = ReaderT GenContext (GenT IO)

data GenContext =
  GenContext
    { remainingNodes :: IORef Int
    , currentDepth :: Int
    , maxDepth :: Int
    , maxChildrenPerNode :: Int
    , maxAttributesPerNode :: Int
    }

consumeNode :: GenM Bool
consumeNode = do
  ref <- asks remainingNodes
  liftIO $
    atomicModifyIORef' ref $
      \n ->
        if n <= 0
          then (0, False)
          else (n - 1, True)

data GeneratorParams =
  GeneratorParams
    { maxTotalNodesParam :: Int
    , maxDepthParam :: Int
    , maxChildrenPerNodeParam :: Int
    , maxAttributesPerElementParam :: Int
    , sizeParam :: Size
    , seedParam :: Seed
    }
