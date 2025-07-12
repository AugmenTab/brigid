module Brigid.Types.Id
  ( Id (Id)
  , idToBytes
  , idToNonEmptyText
  , idToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype Id =
  Id
    { idToNonEmptyText :: NET.NonEmptyText
    } deriving (Eq)

instance Show Id where
  show = mappend "Id " . T.unpack . idToText

idToBytes :: Id -> LBS.ByteString
idToBytes = Render.textToLazyBytes . idToText

idToText :: Id -> T.Text
idToText =
  NET.toText . idToNonEmptyText
