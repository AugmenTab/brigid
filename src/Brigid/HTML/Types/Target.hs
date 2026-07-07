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
  , targetToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

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
targetToBytesBuilder = lazyByteString . targetToBytes

targetToText :: Target -> T.Text
targetToText target =
  case target of
    Self           -> "_self"
    Blank          -> "_blank"
    Parent         -> "_parent"
    Top            -> "_top"
    CustomTarget t -> t

targetToTextBuilder :: Target -> TBL.Builder
targetToTextBuilder = TBL.fromText . targetToText
