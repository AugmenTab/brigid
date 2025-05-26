module Generation.Types
  ( GenM
  , GenContext (..)
  , consumeNode
  , consumeNodes
  , GeneratorParams (..)
  ) where

import Control.Monad.Reader (ReaderT, asks, liftIO)
import Data.IORef (IORef, atomicModifyIORef', readIORef)
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
consumeNode =
  consumeNodes 1

consumeNodes :: Int -> GenM Bool
consumeNodes k = do
  ref <- asks remainingNodes
  liftIO $ do
    remaining <- readIORef ref
    putStrLn $ "consumeNode: remaining = " <> show remaining
    atomicModifyIORef' ref $
      \n ->
        if n < k
          then (n, False)
          else (n - k, True)

data GeneratorParams =
  GeneratorParams
    { maxTotalNodesParam :: Int
    , maxDepthParam :: Int
    , maxChildrenPerNodeParam :: Int
    , maxAttributesPerElementParam :: Int
    , sizeParam :: Size
    , seedParam :: Seed
    }
