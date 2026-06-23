module Brigid.HTML.Types.ContentEditable
  ( ContentEditableOption
      ( Editable
      , NotEditable
      , PlaintextOnly
      )
  , contentEditableOptionToBytes
  , contentEditableOptionToBytesBuilder
  , contentEditableOptionToText
  , contentEditableOptionToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data ContentEditableOption
  = Editable
  | NotEditable
  | PlaintextOnly
  deriving (Bounded, Enum, Eq, Show)

contentEditableOptionToBytes :: ContentEditableOption -> LBS.ByteString
contentEditableOptionToBytes option =
  case option of
    Editable      -> "true"
    NotEditable   -> "false"
    PlaintextOnly -> "plaintext-only"

contentEditableOptionToBytesBuilder :: ContentEditableOption -> Builder
{-# INLINE contentEditableOptionToBytesBuilder #-}
contentEditableOptionToBytesBuilder option =
  case option of
    Editable      -> string8 "true"
    NotEditable   -> string8 "false"
    PlaintextOnly -> string8 "plaintext-only"

contentEditableOptionToText :: ContentEditableOption -> T.Text
contentEditableOptionToText option =
  case option of
    Editable      -> "true"
    NotEditable   -> "false"
    PlaintextOnly -> "plaintext-only"

contentEditableOptionToTextBuilder :: ContentEditableOption -> TBL.Builder
contentEditableOptionToTextBuilder = TBL.fromText . contentEditableOptionToText
