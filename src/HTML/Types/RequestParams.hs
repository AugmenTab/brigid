module HTML.Types.RequestParams
  ( RequestParams
      ( AllParams
      , NoParams
      , Not
      , Params
      )
  , requestParamsToText
  ) where

import Data.Text qualified as T

data RequestParams
  = AllParams
  | NoParams
  | Not [T.Text]
  | Params [T.Text]

requestParamsToText :: RequestParams -> T.Text
requestParamsToText requestParams =
  case requestParams of
    AllParams     -> "*"
    NoParams      -> "none"
    Not    params -> "not " <> T.intercalate "," params
    Params params -> T.intercalate "," params
