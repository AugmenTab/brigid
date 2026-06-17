module Brigid.HXML.Types.NavigatorType
  ( NavigatorType
      ( Stack
      , Tab
      )
  , navigatorTypeToBytes
  , navigatorTypeToBytesBuilder
  , navigatorTypeToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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
navigatorTypeToBytesBuilder navigatorType =
  case navigatorType of
    Stack -> string8 "stack"
    Tab   -> string8 "tab"

navigatorTypeToText :: NavigatorType -> T.Text
navigatorTypeToText navigatorType =
  case navigatorType of
    Stack -> "stack"
    Tab   -> "tab"
