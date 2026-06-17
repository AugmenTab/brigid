module Brigid.HTML.Types.ScriptType
  ( ScriptType
      ( ImportMap
      , Module
      )
  , scriptTypeToBytes
  , scriptTypeToBytesBuilder
  , scriptTypeToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T

data ScriptType
  = ImportMap
  | Module
  deriving (Bounded, Enum, Eq, Show)

scriptTypeToBytes :: ScriptType -> LBS8.ByteString
scriptTypeToBytes st =
  LBS8.pack $
    case st of
      ImportMap -> "importmap"
      Module -> "module"

scriptTypeToBytesBuilder :: ScriptType -> Builder
{-# INLINE scriptTypeToBytesBuilder #-}
scriptTypeToBytesBuilder st =
  case st of
    ImportMap -> string8 "importmap"
    Module    -> string8 "module"

scriptTypeToText :: ScriptType -> T.Text
scriptTypeToText st =
  T.pack $
    case st of
      ImportMap -> "importmap"
      Module -> "module"
