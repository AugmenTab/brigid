module Brigid.HTML.Types.ScriptType
  ( ScriptType
      ( ImportMap
      , Module
      )
  , scriptTypeToBytes
  , scriptTypeToBytesBuilder
  , scriptTypeToText
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data ScriptType
  = ImportMap
  | Module
  deriving (Bounded, Enum, Eq, Show)

scriptTypeToBytes :: ScriptType -> LBS.ByteString
scriptTypeToBytes st =
  case st of
    ImportMap -> "importmap"
    Module    -> "module"

scriptTypeToBytesBuilder :: ScriptType -> Builder
{-# INLINE scriptTypeToBytesBuilder #-}
scriptTypeToBytesBuilder = lazyByteString . scriptTypeToBytes

scriptTypeToText :: ScriptType -> T.Text
scriptTypeToText st =
  T.pack $
    case st of
      ImportMap -> "importmap"
      Module -> "module"
