module Brigid.HTML.Types.HexColor
  ( HexColor
  , hexColorFromText
  , hexColorToBytes
  , hexColorToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.NonEmptyText qualified as NET
import Data.Set qualified as Set
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype HexColor =
  HexColor
    { hexColorToText :: T.Text
    } deriving (Eq)

instance Show HexColor where
  show = mappend "HexColor " . T.unpack . hexColorToText

hexColorFromText :: T.Text -> Either String HexColor
hexColorFromText txt = do
  let hexChars = Set.fromList $ [ '0'..'9' ] <> [ 'a'..'f' ]
      isValidCode c =
        if T.length c == 2 && T.all (`Set.member` hexChars) c
           then Right ()
           else Left $ "Invalid hexadecimal number: " <> T.unpack c

  net <-
    maybe (Left "HexColor cannot be empty.") Right
      . NET.fromText
      $ T.toLower txt

  code <-
    case NET.uncons net of
      ('#', hex) -> Right hex
      _ -> Left $ "HexColor must begin with '#'."

  hexes <-
    case T.chunksOf 2 code of
      hexes
        | length hexes == 3 ->
            Right hexes

        | otherwise ->
            Left "HexColor must contain exactly 3 hexadecimal numbers."

  _validityCheck <- traverse isValidCode hexes

  Right $ HexColor txt

hexColorToBytes :: HexColor -> LBS.ByteString
hexColorToBytes = Render.textToBytes . hexColorToText
