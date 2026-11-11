-- This module is designed to make building meta tags safer by exposing only one
-- combinator that explicitly takes the metadata type, which itself can only
-- contain appropriate values for that metadata type SPECIFICALLY for the meta
-- tag context.
--
module Brigid.HTML.Elements.Safe.Meta
  ( meta
  , Metadata
      ( Charset
      , HttpEquiv
   -- , Itemprop
      , Name
      )
  , HttpEquivToken
      ( ContentType
      , DefaultStyle
      , Refresh
      , X_UA_Compatible
      , PermissionsPolicy
      , CacheControl
      , Pragma
      , ContentSecurityPolicy
      )
  , MetadataName
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
  | HttpEquiv HttpEquivToken
  -- | Itemprop
  | Name MetadataName
  deriving (Eq, Show)

meta :: Metadata -> E.ChildHTML E.Head E.Html
meta metadata =
  E.meta $
    case metadata of
      Charset         -> [ A.charset ]
      HttpEquiv token -> httpEquivAttributes token
   -- Itemprop        -> []
      Name      name  -> nameAttributes name

data HttpEquivToken
  = ContentType
  -- ^ Specifies the MIME type and character encoding for the document.
  | DefaultStyle
  -- ^ Specifies the preferred stylesheet to use.
  | Refresh
  -- ^ Specifies a redirect or auto-refresh interval.
  | X_UA_Compatible
  -- ^ Specifies which version of Internet Explorer (IE) the page is compatible with.
  | PermissionsPolicy
  -- ^ Specifies permissions for APIs and features (formerly known as Feature-Policy).
  | CacheControl
  -- ^ Provides caching instructions to the browser (similar to the HTTP Cache-Control header).
  | Pragma
  -- ^ Provides backward compatibility for caching instructions (rarely used, replaced by cache-control).
  | ContentSecurityPolicy
  -- ^ Specifies a Content Security Policy (CSP) to restrict or control resources loaded by the document.
  deriving (Eq, Show)

httpEquivAttributes :: HttpEquivToken -> [Attribute Meta]
httpEquivAttributes token =
  let (tokenName, tokenContent) =
        case token of
          ContentType ->
            (Types.ContentType, "") -- TODO

          DefaultStyle ->
            (Types.DefaultStyle, "") -- TODO

          Refresh ->
            (Types.Refresh, "") -- TODO

          X_UA_Compatible ->
            (Types.X_UA_Compatible, "") -- TODO

          PermissionsPolicy ->
            (Types.PermissionsPolicy, "") -- TODO

          CacheControl ->
            (Types.CacheControl, "") -- TODO

          Pragma ->
            (Types.Pragma, "") -- TODO

          ContentSecurityPolicy ->
            (Types.ContentSecurityPolicy, "") -- TODO

   in [ A.httpEquiv tokenName, A.content tokenContent ]

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Bounded, Enum, Eq, Show)

colorSchemePaletteToText :: ColorSchemePalette -> T.Text
colorSchemePaletteToText palette =
  case palette of
    Light     -> "light"
    Dark      -> "dark"
    LightDark -> "light dark"
    DarkLight -> "dark light"
