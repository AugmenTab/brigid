{-# LANGUAGE OverloadedStrings #-}

-- | A deliberately small, independent model of how an HTML or XML parser
-- tokenizes start tags and decodes attribute values. The escaping tests use
-- it to check that rendered markup has the intended attribute structure and
-- that every value decodes back to the original input.
--
-- It follows the HTML tokenizer's attribute-name and attribute-value states
-- (double-quoted, single-quoted, and unquoted), and it decodes numeric
-- character references plus the named references @quot@, @amp@, @lt@, @gt@
-- and @apos@. In HTML mode the semicolon-less legacy forms of those named
-- references are decoded too. In XML mode literal tab, newline and carriage
-- return in a value become spaces, as XML attribute-value normalization
-- requires. It does not attempt to be a full parser. Treat it as a test
-- oracle, not a sanitizer.
module Escaping.Oracle
  ( Mode (..)
  , StartTag (..)
  , parseStartTags
  , allAttributes
  , decodeValue
  ) where

import Data.Char (chr, isAlphaNum, isDigit, isHexDigit)
import Data.Text qualified as T
import Numeric (readHex)

data Mode = HTMLMode | XMLMode
  deriving (Eq, Show)

data StartTag =
  StartTag
    { tagName :: T.Text
    , tagAttributes :: [(T.Text, T.Text)]
    } deriving (Eq, Show)

-- | Every attribute of every start tag, in document order.
allAttributes :: Mode -> T.Text -> Either String [(T.Text, T.Text)]
allAttributes mode =
  fmap (concatMap tagAttributes) . parseStartTags mode

-- | Finds every start tag in the input. Text content, end tags, comments and
-- doctypes are skipped.
parseStartTags :: Mode -> T.Text -> Either String [StartTag]
parseStartTags mode input =
  case T.breakOn "<" input of
    (_, rest)
      | T.null rest -> Right []
      | otherwise ->
          let afterLt = T.drop 1 rest in
          case T.uncons afterLt of
            Just (c, _)
              | c == '/' || c == '!' || c == '?' ->
                  parseStartTags mode (T.drop 1 (T.dropWhile (/= '>') afterLt))
            _ -> do
              let (name, afterName) = T.break isTagNameEnd afterLt
              (attrs, remaining) <- parseAttributes mode afterName
              (StartTag name attrs :) <$> parseStartTags mode remaining

parseAttributes :: Mode -> T.Text -> Either String ([(T.Text, T.Text)], T.Text)
parseAttributes mode input =
  let t = T.dropWhile isWhitespace input in
  case T.uncons t of
    Nothing ->
      Left "unterminated start tag"

    Just ('>', rest) ->
      Right ([], rest)

    Just ('/', rest)
      | Just ('>', rest') <- T.uncons rest -> Right ([], rest')
      | otherwise -> parseAttributes mode rest

    Just (first, afterFirst) -> do
      -- As in the HTML tokenizer, the first character of an attribute name
      -- is always consumed as part of the name, even when it is '='.
      let (nameTail, afterName) = T.break isAttrNameEnd afterFirst
          name = T.cons first nameTail
          afterNameWs = T.dropWhile isWhitespace afterName
      case T.uncons afterNameWs of
        Just ('=', afterEq) -> do
          let valueStart = T.dropWhile isWhitespace afterEq
          (raw, afterValue) <-
            case T.uncons valueStart of
              Just (q, quoted)
                | q == '"' || q == '\'' ->
                    let (raw, close) = T.break (== q) quoted in
                    if T.null close
                      then Left "unterminated attribute value"
                      else Right (raw, T.drop 1 close)
              _ ->
                Right (T.break (\c -> isWhitespace c || c == '>') valueStart)
          (attrs, remaining) <- parseAttributes mode afterValue
          Right ((name, decodeValue mode raw) : attrs, remaining)

        _ -> do
          (attrs, remaining) <- parseAttributes mode afterNameWs
          Right ((name, "") : attrs, remaining)

-- | Decodes a raw attribute value the way a parser would.
decodeValue :: Mode -> T.Text -> T.Text
decodeValue mode raw =
  decodeRefs mode $
    case mode of
      HTMLMode -> raw
      XMLMode  -> T.map (\c -> if c `elem` ['\t', '\n', '\r'] then ' ' else c) raw

decodeRefs :: Mode -> T.Text -> T.Text
decodeRefs mode txt =
  case T.breakOn "&" txt of
    (prefix, rest)
      | T.null rest -> prefix
      | otherwise ->
          let afterAmp = T.drop 1 rest in
          case decodeOne afterAmp of
            Just (c, remaining) -> prefix <> T.singleton c <> decodeRefs mode remaining
            Nothing             -> prefix <> "&" <> decodeRefs mode afterAmp
  where
    decodeOne t
      | Just hexRest <- T.stripPrefix "#x" t <|> T.stripPrefix "#X" t =
          let (digits, after) = T.span isHexDigit hexRest in
          case (T.null digits, T.stripPrefix ";" after) of
            (False, Just after') ->
              case readHex (T.unpack digits) of
                [(n, "")] -> Just (chr n, after')
                _         -> Nothing
            _ -> Nothing
      | Just decRest <- T.stripPrefix "#" t =
          let (digits, after) = T.span isDigit decRest in
          case (T.null digits, T.stripPrefix ";" after) of
            (False, Just after') -> Just (chr (read (T.unpack digits)), after')
            _                    -> Nothing
      | otherwise =
          firstJust (map (named t) namedRefs)

    named t (name, c) = do
      after <- T.stripPrefix name t
      case T.stripPrefix ";" after of
        Just after' -> Just (c, after')
        Nothing
          | mode == HTMLMode && name /= "apos" && legacyAllowed after -> Just (c, after)
          | otherwise -> Nothing

    -- Inside attribute values, HTML does not decode a semicolon-less legacy
    -- reference that is followed by '=' or an alphanumeric character.
    legacyAllowed after =
      case T.uncons after of
        Just (c, _) -> not (c == '=' || isAlphaNum c)
        Nothing     -> True

    namedRefs =
      [ ("quot", '"')
      , ("amp", '&')
      , ("lt", '<')
      , ("gt", '>')
      , ("apos", '\'')
      ]

    firstJust xs =
      case [x | Just x <- xs] of
        (x:_) -> Just x
        []    -> Nothing

    a <|> b = maybe b Just a

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t', '\n', '\r', '\f']

isTagNameEnd :: Char -> Bool
isTagNameEnd c = isWhitespace c || c == '/' || c == '>'

isAttrNameEnd :: Char -> Bool
isAttrNameEnd c = isWhitespace c || c == '/' || c == '>' || c == '='
