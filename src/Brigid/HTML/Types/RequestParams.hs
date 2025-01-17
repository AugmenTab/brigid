module Brigid.HTML.Types.RequestParams
  ( RequestParams
      ( AllParams
      , NoParams
      , Not
      , Params
      )
  , requestParamsToBytes
  , requestParamsToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Brigid.HTML.Internal.Render qualified as Render

data RequestParams
  = AllParams
  | NoParams
  | Not [T.Text]
  | Params [T.Text]
  deriving (Eq, Show)

requestParamsToBytes :: RequestParams -> LBS.ByteString
requestParamsToBytes requestParams =
  case requestParams of
    AllParams ->
      "*"

    NoParams ->
      "none"

    Not params ->
      LBS8.unwords
        [ "not"
        , Render.foldToBytesWithSeparator
            (LBS.fromStrict . TE.encodeUtf8)
            ","
            params
        ]

    Params params ->
      Render.foldToBytesWithSeparator
        (LBS.fromStrict . TE.encodeUtf8)
        ","
        params

requestParamsToText :: RequestParams -> T.Text
requestParamsToText requestParams =
  case requestParams of
    AllParams        -> "*"
    NoParams         -> "none"
    Not       params -> "not " <> Render.foldToTextWithSeparator id "," params
    Params    params -> Render.foldToTextWithSeparator id "," params
