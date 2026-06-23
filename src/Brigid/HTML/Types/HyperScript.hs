module Brigid.HTML.Types.HyperScript
  ( HyperScript (HyperScript)
  , hyperScriptToBytes
  , hyperScriptToBytesBuilder
  , hyperScriptToText
  , hyperScriptToTextBuilder
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import Data.Text.Encoding qualified as TE

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

hyperScriptToBytesBuilder :: HyperScript -> Builder
hyperScriptToBytesBuilder =
  TE.encodeUtf8Builder . hyperScriptToText

hyperScriptToTextBuilder :: HyperScript -> TBL.Builder
hyperScriptToTextBuilder = TBL.fromText . hyperScriptToText
