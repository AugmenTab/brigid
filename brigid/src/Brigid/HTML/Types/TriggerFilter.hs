module Brigid.HTML.Types.TriggerFilter
  ( TriggerFilter (TriggerFilter)
  , triggerFilterToBytes
  , triggerFilterToBytesBuilder
  , triggerFilterToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

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

triggerFilterToBytesBuilder :: TriggerFilter -> Builder
triggerFilterToBytesBuilder =
  TE.encodeUtf8Builder . triggerFilterToText
