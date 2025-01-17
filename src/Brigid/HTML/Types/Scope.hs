module Brigid.HTML.Types.Scope
  ( Scope
      ( Col
      , Row
      , ColGroup
      , RowGroup
      )
  , scopeToBytes
  , scopeToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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

scopeToText :: Scope -> T.Text
scopeToText scope =
  case scope of
    Col      -> "col"
    Row      -> "row"
    ColGroup -> "colgroup"
    RowGroup -> "rowgroup"
