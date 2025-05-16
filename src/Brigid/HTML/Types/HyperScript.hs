module Brigid.HTML.Types.HyperScript
  ( HyperScript (HyperScript)
  , hyperScriptToBytes
  , hyperScriptToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype HyperScript =
  HyperScript
    { hyperScriptToText :: T.Text
    } deriving (Eq)

instance Show HyperScript where
  show = mappend "HyperScript " . show

hyperScriptToBytes :: HyperScript -> LBS.ByteString
hyperScriptToBytes =
  Render.textToLazyBytes . hyperScriptToText
