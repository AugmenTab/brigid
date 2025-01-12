module Brigid.HTML.Types.Class
  ( Class (Class)
  , classToBytes
  , classToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

newtype Class =
  Class
    { classToText :: T.Text
    }

classToBytes :: Class -> LBS.ByteString
classToBytes = LBS.fromStrict . TE.encodeUtf8 . classToText
