module Brigid.HTML.Types.TriggerFilter
  ( TriggerFilter (TriggerFilter)
  , triggerFilterToBytes
  , triggerFilterToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype TriggerFilter =
  TriggerFilter
    { triggerFilterToText :: T.Text
    } deriving (Eq)

instance Show TriggerFilter where
  show = mappend "TriggerFilter " . show

triggerFilterToBytes :: TriggerFilter -> LBS.ByteString
triggerFilterToBytes =
  Render.textToLazyBytes . triggerFilterToText
