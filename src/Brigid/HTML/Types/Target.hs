module Brigid.HTML.Types.Target
  ( Target
      ( Self
      , Blank
      , Parent
      , Top
      , CustomTarget
      )
  , targetToBytes
  , targetToBytesBuilder
  , targetToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Brigid.Internal.Render qualified as Render

data Target
  = Self
  | Blank
  | Parent
  | Top
  | CustomTarget T.Text
  deriving (Eq, Show)

targetToBytes :: Target -> LBS.ByteString
targetToBytes target =
  case target of
    Self           -> "_self"
    Blank          -> "_blank"
    Parent         -> "_parent"
    Top            -> "_top"
    CustomTarget t -> Render.textToLazyBytes t

targetToBytesBuilder :: Target -> Builder
{-# INLINE targetToBytesBuilder #-}
targetToBytesBuilder target =
  case target of
    Self           -> string8 "_self"
    Blank          -> string8 "_blank"
    Parent         -> string8 "_parent"
    Top            -> string8 "_top"
    CustomTarget t -> TE.encodeUtf8Builder t

targetToText :: Target -> T.Text
targetToText target =
  case target of
    Self           -> "_self"
    Blank          -> "_blank"
    Parent         -> "_parent"
    Top            -> "_top"
    CustomTarget t -> t
