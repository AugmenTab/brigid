module HTML.Render.Internal.Escape
  ( attribute
  , attributeChar
  , html
  ) where

import Data.Text qualified as T

attribute :: T.Text -> T.Text
attribute =
  T.concatMap attributeChar

attributeChar :: Char -> T.Text
attributeChar c =
  case c of
    '"'  -> "&quot;"
    '\'' -> "&#39;"
    _    -> T.singleton c

html :: T.Text -> T.Text
html =
  T.concatMap $
    \c ->
      case c of
        '&' -> "&amp;"
        '<' -> "&lt;"
        '>' -> "&gt;"
        _   -> T.singleton c
