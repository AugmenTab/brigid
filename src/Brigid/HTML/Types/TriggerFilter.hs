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
    }

triggerFilterToBytes :: TriggerFilter -> LBS.ByteString
triggerFilterToBytes =
  Render.textToBytes . triggerFilterToText
