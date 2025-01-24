module Brigid.HXML.Types.ShowLoadingIndicator
  ( ShowLoadingIndicator
      ( All
      , DocumentOnly
      )
  , showLoadingIndicatorToBytes
  , showLoadingIndicatorToText
  ) where

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

showLoadingIndicatorToText :: ShowLoadingIndicator -> T.Text
showLoadingIndicatorToText indicator =
  case indicator of
    All          -> "all"
    DocumentOnly -> "document-only"
