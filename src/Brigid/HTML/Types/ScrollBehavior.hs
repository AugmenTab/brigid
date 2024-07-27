module Brigid.HTML.Types.ScrollBehavior
  ( ScrollBehavior
      ( AutoScroll
      , SmoothScroll
      )
  , scrollBehaviorToBytes
  , scrollBehaviorToText
  , scrollBehaviorFromText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data ScrollBehavior
  = AutoScroll
  | SmoothScroll

scrollBehaviorToBytes :: ScrollBehavior -> LBS.ByteString
scrollBehaviorToBytes wbt =
  case wbt of
    AutoScroll   -> "auto"
    SmoothScroll -> "smooth"

scrollBehaviorToText :: ScrollBehavior -> T.Text
scrollBehaviorToText wbt =
  case wbt of
    AutoScroll   -> "auto"
    SmoothScroll -> "smooth"

scrollBehaviorFromText :: T.Text -> Either String ScrollBehavior
scrollBehaviorFromText txt =
  case txt of
    "auto"   -> Right AutoScroll
    "smooth" -> Right SmoothScroll
    _        -> Left $ "Unknown ScrollBehavior: " <> T.unpack txt
