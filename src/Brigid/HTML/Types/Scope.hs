module Brigid.HTML.Types.Scope
  ( Scope
      ( Col
      , Row
      , ColGroup
      , RowGroup
      )
  , scopeToBytes
  , scopeToBytesBuilder
  , scopeToText
  , scopeToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data Scope
  = Col
  | Row
  | ColGroup
  | RowGroup
  deriving (Bounded, Enum, Eq, Show)

scopeToBytes :: Scope -> LBS.ByteString
scopeToBytes scope =
  case scope of
    Col      -> "col"
    Row      -> "row"
    ColGroup -> "colgroup"
    RowGroup -> "rowgroup"

scopeToBytesBuilder :: Scope -> Builder
{-# INLINE scopeToBytesBuilder #-}
scopeToBytesBuilder scope =
  case scope of
    Col      -> string8 "col"
    Row      -> string8 "row"
    ColGroup -> string8 "colgroup"
    RowGroup -> string8 "rowgroup"

scopeToText :: Scope -> T.Text
scopeToText scope =
  case scope of
    Col      -> "col"
    Row      -> "row"
    ColGroup -> "colgroup"
    RowGroup -> "rowgroup"

scopeToTextBuilder :: Scope -> TBL.Builder
scopeToTextBuilder = TBL.fromText . scopeToText
