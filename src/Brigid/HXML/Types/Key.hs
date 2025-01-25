module Brigid.HXML.Types.Key
  ( Key (Key)
  , keyToBytes
  , keyToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype Key =
  Key
    { keyToText :: T.Text
    } deriving (Eq)

instance Show Key where
  show = mappend "Key " . T.unpack . keyToText

keyToBytes :: Key -> LBS.ByteString
keyToBytes = Render.textToBytes . keyToText
