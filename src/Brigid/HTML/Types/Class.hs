module Brigid.HTML.Types.Class
  ( Class
  , mkClass
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

mkClass :: T.Text -> Class
mkClass = Class

classToBytes :: Class -> LBS.ByteString
classToBytes = LBS.fromStrict . TE.encodeUtf8 . classToText
