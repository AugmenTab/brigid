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

import Data.ByteString.Builder (Builder, string8)
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
metadataNameToBytesBuilder name =
  case name of
    ApplicationName -> string8 "application-name"
    Author          -> string8 "author"
    ColorScheme     -> string8 "color-scheme"
    Description     -> string8 "description"
    Generator       -> string8 "generator"
    Keywords        -> string8 "keywords"
    Referrer        -> string8 "referrer"
    Robots          -> string8 "robots"
    ThemeColor      -> string8 "theme-color"
    Viewport        -> string8 "viewport"

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
