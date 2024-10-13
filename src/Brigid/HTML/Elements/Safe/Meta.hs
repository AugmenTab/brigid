-- This module is designed to make building meta tags safer by exposing only one
-- combinator that explicitly takes the metadata type, which itself can only
-- contain appropriate values for that metadata type SPECIFICALLY for the meta
-- tag context.
--
module Brigid.HTML.Elements.Safe.Meta
  ( meta
  , Metadata
      ( Charset
   -- , HttpEquiv
   -- , Itemprop
      , Name
      )
  , MetadataName
      ( ApplicationName
      , Author
      , Description
      , Generator
      , Keywords
      , Referrer
      , ColorScheme
      , ThemeColor
      , Viewport
      )
  , ColorSchemeOption
      ( Normal
      , Palettes
      , OnlyLight
      )
  , ColorSchemePalette
      ( Light
      , Dark
      , LightDark
      , DarkLight
      )
  ) where

import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Attributes.Internal (Attribute)
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.Tags (Meta)
import Brigid.HTML.Types (ReferrerPolicy, referrerPolicyToText)

data Metadata
     = Charset
  -- | HttpEquiv
  -- | Itemprop
     | Name MetadataName

meta :: Metadata -> E.ChildHTML E.Head E.Html
meta metadata =
  E.meta $
    case metadata of
      Charset -> [ A.charset ]
   -- HttpEquiv -> []
   -- Itemprop -> []
      Name name -> nameAttributes name

data MetadataName
  = ApplicationName
  | Author
  | Description
  | Generator
  | Keywords
  | Referrer ReferrerPolicy
  | ColorScheme ColorSchemeOption
  | ThemeColor
  | Viewport

metadataNameToText :: MetadataName -> T.Text
metadataNameToText metadata =
  case metadata of
    ApplicationName          -> "application-name"
    Author                   -> "author"
    Description              -> "description"
    Generator                -> "generator"
    Keywords                 -> "keywords"
    Referrer        _content -> "referrer"
    ColorScheme     _content -> "color-scheme"
    ThemeColor               -> "theme-color"
    Viewport                 -> "viewport"

nameAttributes :: MetadataName -> [Attribute Meta]
nameAttributes name =
  [ A.name $ metadataNameToText name
  , A.content $
      case name of
        ApplicationName         -> "" -- TODO
        Author                  -> "" -- TODO
        Description             -> "" -- TODO
        Generator               -> "" -- TODO
        Keywords                -> "" -- TODO
        Referrer        content -> referrerPolicyToText content
        ColorScheme     content -> colorSchemeOptionToText content
        ThemeColor              -> "" -- TODO
        Viewport                -> "" -- TODO
  ]

data ColorSchemeOption
  = Normal
  | Palettes (NEL.NonEmpty ColorSchemePalette)
  | OnlyLight

colorSchemeOptionToText :: ColorSchemeOption -> T.Text
colorSchemeOptionToText scheme =
  case scheme of
    Normal ->
      "normal"

    Palettes palettes ->
      T.intercalate ", " $ colorSchemePaletteToText <$> NEL.toList palettes

    OnlyLight ->
      "only light"

data ColorSchemePalette
  = Light
  | Dark
  | LightDark
  | DarkLight

colorSchemePaletteToText :: ColorSchemePalette -> T.Text
colorSchemePaletteToText palette =
  case palette of
    Light     -> "light"
    Dark      -> "dark"
    LightDark -> "light dark"
    DarkLight -> "dark light"
