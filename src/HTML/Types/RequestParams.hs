module HTML.Types.RequestParams
  ( RequestParams
      ( AllParams
      , None
      , Not
      , Params
      )
  , requestParamsToText
  ) where

import Data.Text qualified as T

data RequestParams
  = AllParams
  | None
  | Not [T.Text]
  | Params [T.Text]

requestParamsToText :: RequestParams -> T.Text
requestParamsToText requestParams =
  case requestParams of
    AllParams     -> "*"
    None          -> "none"
    Not    params -> "not " <> T.intercalate "," params
    Params params -> T.intercalate "," params
