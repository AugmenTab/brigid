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
import Brigid.HTML.Types qualified as Types

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
  | ColorScheme ColorSchemeOption
  | Description
  | Generator
  | Keywords
  | Referrer Types.ReferrerPolicy
  | Robots
  | ThemeColor
  | Viewport

nameAttributes :: MetadataName -> [Attribute Meta]
nameAttributes name =
  let (metadataName, metadataContent) =
        case name of
          ApplicationName ->
            (Types.ApplicationName, "") -- TODO

          Author ->
            (Types.Author, "") -- TODO

          ColorScheme content ->
            (Types.ColorScheme, colorSchemeOptionToText content)

          Description ->
            (Types.Description, "") -- TODO

          Generator ->
            (Types.Generator, "") -- TODO

          Keywords ->
            (Types.Keywords, "") -- TODO

          Referrer content ->
            (Types.Referrer, Types.referrerPolicyToText content)

          Robots ->
            (Types.Robots, "") -- TODO

          ThemeColor ->
            (Types.ThemeColor, "") -- TODO

          Viewport ->
            (Types.Viewport, "") -- TODO

   in [ A.name metadataName, A.content metadataContent ]

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
