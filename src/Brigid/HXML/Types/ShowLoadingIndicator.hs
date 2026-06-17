module Brigid.HXML.Types.ShowLoadingIndicator
  ( ShowLoadingIndicator
      ( All
      , DocumentOnly
      )
  , showLoadingIndicatorToBytes
  , showLoadingIndicatorToBytesBuilder
  , showLoadingIndicatorToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data ShowLoadingIndicator
  = All
  | DocumentOnly
  deriving (Bounded, Enum, Eq, Show)

showLoadingIndicatorToBytes :: ShowLoadingIndicator -> LBS.ByteString
showLoadingIndicatorToBytes indicator =
  case indicator of
    All          -> "all"
    DocumentOnly -> "document-only"

showLoadingIndicatorToBytesBuilder :: ShowLoadingIndicator -> Builder
showLoadingIndicatorToBytesBuilder indicator =
  case indicator of
    All          -> string8 "all"
    DocumentOnly -> string8 "document-only"

showLoadingIndicatorToText :: ShowLoadingIndicator -> T.Text
showLoadingIndicatorToText indicator =
  case indicator of
    All          -> "all"
    DocumentOnly -> "document-only"
