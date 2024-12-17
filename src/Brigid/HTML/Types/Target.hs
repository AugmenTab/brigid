module Brigid.HTML.Types.Target
  ( Target
      ( Self
      , Blank
      , Parent
      , Top
      )
  , targetToBytes
  , targetToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T

data Target
  = Self
  | Blank
  | Parent
  | Top
  | CustomTarget T.Text

targetToBytes :: Target -> LBS.ByteString
targetToBytes target =
  case target of
    Self           -> "_self"
    Blank          -> "_blank"
    Parent         -> "_parent"
    Top            -> "_top"
    CustomTarget t -> LBS8.pack $ T.unpack t

targetToText :: Target -> T.Text
targetToText target =
  case target of
    Self           -> "_self"
    Blank          -> "_blank"
    Parent         -> "_parent"
    Top            -> "_top"
    CustomTarget t -> t
