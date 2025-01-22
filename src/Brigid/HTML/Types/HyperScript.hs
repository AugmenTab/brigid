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
    }

hyperScriptToBytes :: HyperScript -> LBS.ByteString
hyperScriptToBytes =
  Render.textToBytes . hyperScriptToText
