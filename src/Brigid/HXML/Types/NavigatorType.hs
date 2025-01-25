module Brigid.HXML.Types.NavigatorType
  ( NavigatorType
      ( Stack
      , Tab
      )
  , navigatorTypeToBytes
  , navigatorTypeToText
  ) where

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

navigatorTypeToText :: NavigatorType -> T.Text
navigatorTypeToText navigatorType =
  case navigatorType of
    Stack -> "stack"
    Tab   -> "tab"
