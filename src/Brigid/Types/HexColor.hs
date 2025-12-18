module Brigid.Types.HexColor
  ( HexColor
  , hexColorFromText
  , hexColorToBytes
  , hexColorToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as C
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype HexColor =
  HexColor
    { hexColorToText :: T.Text
    } deriving (Eq)

instance Show HexColor where
  show = mappend "HexColor " . T.unpack . hexColorToText

hexColorFromText :: T.Text -> Either String HexColor
hexColorFromText txt =
  case T.uncons txt of
    Just ('#', hex)
      | T.all C.isHexDigit hex, T.length hex `elem` [ 3,6 ] ->
          Right . HexColor $ T.toLower txt

    Just _otherwise ->
      Left $ "Invalid HexColor: " <> T.unpack txt

    Nothing ->
      Left "HexColor cannot be empty."

hexColorToBytes :: HexColor -> LBS.ByteString
hexColorToBytes =
  Render.textToLazyBytes . hexColorToText
