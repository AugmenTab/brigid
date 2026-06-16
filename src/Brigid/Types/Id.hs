module Brigid.Types.Id
  ( Id (Id)
  , idToBytes
  , idToBytesBuilder
  , idToNonEmptyText
  , idToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Brigid.Internal.Render qualified as Render

newtype Id =
  Id
    { idToNonEmptyText :: NET.NonEmptyText
    } deriving (Eq, Ord)

instance Show Id where
  show = mappend "Id " . T.unpack . idToText

idToBytes :: Id -> LBS.ByteString
idToBytes = Render.textToLazyBytes . idToText

idToBytesBuilder :: Id -> Builder
idToBytesBuilder = TE.encodeUtf8Builder . idToText

idToText :: Id -> T.Text
idToText =
  NET.toText . idToNonEmptyText
