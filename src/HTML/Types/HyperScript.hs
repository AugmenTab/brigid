module HTML.Types.HyperScript
  ( HyperScript (HyperScript)
  , hyperScriptToBytes
  , hyperScriptToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

newtype HyperScript =
  HyperScript
    { hyperScriptToText :: T.Text
    }

hyperScriptToBytes :: HyperScript -> LBS.ByteString
hyperScriptToBytes =
  LBS.fromStrict . TE.encodeUtf8 . hyperScriptToText
