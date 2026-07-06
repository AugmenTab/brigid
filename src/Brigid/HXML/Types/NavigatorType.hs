module Brigid.HXML.Types.NavigatorType
  ( NavigatorType
      ( Stack
      , Tab
      )
  , navigatorTypeToBytes
  , navigatorTypeToBytesBuilder
  , navigatorTypeToText
  , navigatorTypeToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data NavigatorType
  = Stack
  | Tab
  deriving (Bounded, Enum, Eq, Show)

navigatorTypeToBytes :: NavigatorType -> LBS.ByteString
navigatorTypeToBytes navigatorType =
  case navigatorType of
    Stack -> "stack"
    Tab   -> "tab"

navigatorTypeToBytesBuilder :: NavigatorType -> Builder
{-# INLINE navigatorTypeToBytesBuilder #-}
navigatorTypeToBytesBuilder = lazyByteString . navigatorTypeToBytes

navigatorTypeToText :: NavigatorType -> T.Text
navigatorTypeToText navigatorType =
  case navigatorType of
    Stack -> "stack"
    Tab   -> "tab"

navigatorTypeToTextBuilder :: NavigatorType -> TBL.Builder
navigatorTypeToTextBuilder = TBL.fromText . navigatorTypeToText
