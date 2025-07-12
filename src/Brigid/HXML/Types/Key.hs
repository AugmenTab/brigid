module Brigid.HXML.Types.Key
  ( Key (Key)
  , keyToBytes
  , keyToNonEmptyText
  , keyToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype Key =
  Key
    { keyToNonEmptyText :: NET.NonEmptyText
    } deriving (Eq)

instance Show Key where
  show = mappend "Key " . T.unpack . keyToText

keyToBytes :: Key -> LBS.ByteString
keyToBytes = Render.textToLazyBytes . keyToText

keyToText :: Key -> T.Text
keyToText = NET.toText . keyToNonEmptyText
