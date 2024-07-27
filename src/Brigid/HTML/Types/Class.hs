module Brigid.HTML.Types.Class
  ( Class (Class)
  , classToBytes
  , classToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

newtype Class = Class T.Text

classToBytes :: Class -> LBS.ByteString
classToBytes (Class _class) = LBS.fromStrict $ TE.encodeUtf8 _class

classToText :: Class -> T.Text
classToText (Class _class) = _class
