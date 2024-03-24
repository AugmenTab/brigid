module HTML.Types.Once
  ( Once (Once)
  , onceToBytes
  , onceToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Once = Once

onceToBytes :: Once -> LBS.ByteString
onceToBytes = const "once"

onceToText :: Once -> T.Text
onceToText = const "once"
