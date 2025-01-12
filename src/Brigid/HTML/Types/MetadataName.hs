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
  , metadataNameToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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
