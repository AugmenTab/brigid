module Brigid.HTML.Types.ContentEditable
  ( ContentEditableOption
      ( Editable
      , NotEditable
      , PlaintextOnly
      )
  , contentEditableOptionToBytes
  , contentEditableOptionToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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

contentEditableOptionToText :: ContentEditableOption -> T.Text
contentEditableOptionToText option =
  case option of
    Editable      -> "true"
    NotEditable   -> "false"
    PlaintextOnly -> "plaintext-only"
