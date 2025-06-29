module Brigid.HTML.Types.Document
  ( Document (Document)
  , documentToBytes
  , documentToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Document = Document
  deriving (Eq, Show)

documentToBytes :: Document -> LBS.ByteString
documentToBytes = const "document"

documentToText :: Document -> T.Text
documentToText = const "document"
