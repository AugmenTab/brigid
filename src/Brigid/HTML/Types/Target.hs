module Brigid.HTML.Types.Target
  ( Target
      ( Self
      , Blank
      , Parent
      , Top
      , CustomTarget
      )
  , targetToBytes
  , targetToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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

targetToText :: Target -> T.Text
targetToText target =
  case target of
    Self           -> "_self"
    Blank          -> "_blank"
    Parent         -> "_parent"
    Top            -> "_top"
    CustomTarget t -> t
