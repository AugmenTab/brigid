module Brigid.HTML.Types.Id
  ( Id (Id)
  , idToBytes
  , idToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

newtype Id =
  Id
    { idToText :: T.Text
    } deriving (Eq, Show)

idToBytes :: Id -> LBS.ByteString
idToBytes = LBS.fromStrict . TE.encodeUtf8 . idToText
