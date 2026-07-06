module Brigid.HTML.Types.MetadataName
  ( MetadataName
      ( ApplicationName
      , Author
      , ColorScheme
      , Description
      , Generator
      , Keywords
      , Referrer
      , Robots
      , ThemeColor
      , Viewport
      )
  , metadataNameToBytes
  , metadataNameToBytesBuilder
  , metadataNameToText
  , metadataNameToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data MetadataName
  = ApplicationName
  | Author
  | ColorScheme
  | Description
  | Generator
  | Keywords
  | Referrer
  | Robots
  | ThemeColor
  | Viewport
  deriving (Bounded, Enum, Eq, Show)

metadataNameToBytes :: MetadataName -> LBS.ByteString
metadataNameToBytes name =
  case name of
    ApplicationName -> "application-name"
    Author          -> "author"
    ColorScheme     -> "color-scheme"
    Description     -> "description"
    Generator       -> "generator"
    Keywords        -> "keywords"
    Referrer        -> "referrer"
    Robots          -> "robots"
    ThemeColor      -> "theme-color"
    Viewport        -> "viewport"

metadataNameToBytesBuilder :: MetadataName -> Builder
{-# INLINE metadataNameToBytesBuilder #-}
metadataNameToBytesBuilder = lazyByteString . metadataNameToBytes

metadataNameToText :: MetadataName -> T.Text
metadataNameToText name =
  case name of
    ApplicationName -> "application-name"
    Author          -> "author"
    ColorScheme     -> "color-scheme"
    Description     -> "description"
    Generator       -> "generator"
    Keywords        -> "keywords"
    Referrer        -> "referrer"
    Robots          -> "robots"
    ThemeColor      -> "theme-color"
    Viewport        -> "viewport"

metadataNameToTextBuilder :: MetadataName -> TBL.Builder
metadataNameToTextBuilder = TBL.fromText . metadataNameToText
