module Brigid.HTML.Types.Id
  ( Id (Id)
  , idToBytes
  , idToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype Id =
  Id
    { idToText :: T.Text
    } deriving (Eq, Show)

idToBytes :: Id -> LBS.ByteString
idToBytes = Render.textToBytes . idToText
