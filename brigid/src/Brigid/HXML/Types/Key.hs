module Brigid.HXML.Types.Key
  ( Key (Key)
  , keyToBytes
  , keyToBytesBuilder
  , keyToNonEmptyText
  , keyToText
  , keyToTextBuilder
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

import Brigid.Internal.Render qualified as Render

newtype Key =
  Key
    { keyToNonEmptyText :: NET.NonEmptyText
    } deriving (Eq)

instance Show Key where
  show = mappend "Key " . T.unpack . keyToText

keyToBytes :: Key -> LBS.ByteString
keyToBytes = Render.textToLazyBytes . keyToText

keyToBytesBuilder :: Key -> Builder
keyToBytesBuilder = Render.textToBytesBuilder . keyToText

keyToText :: Key -> T.Text
keyToText = NET.toText . keyToNonEmptyText

keyToTextBuilder :: Key -> TBL.Builder
keyToTextBuilder = TBL.fromText . keyToText
