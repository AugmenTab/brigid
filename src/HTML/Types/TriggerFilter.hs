module HTML.Types.TriggerFilter
  ( TriggerFilter (TriggerFilter)
  , triggerFilterToBytes
  , triggerFilterToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

newtype TriggerFilter =
  TriggerFilter
    { triggerFilterToText :: T.Text
    }

triggerFilterToBytes :: TriggerFilter -> LBS.ByteString
triggerFilterToBytes =
  LBS.fromStrict . TE.encodeUtf8 . triggerFilterToText
