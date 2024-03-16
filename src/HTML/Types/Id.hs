module HTML.Types.Id
  ( Id (Id)
  , idToBytes
  , idToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

newtype Id = Id T.Text

idToBytes :: Id -> LBS.ByteString
idToBytes = LBS.fromStrict . TE.encodeUtf8 . idToText

idToText :: Id -> T.Text
idToText (Id _id) = _id
