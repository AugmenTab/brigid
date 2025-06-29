module Brigid.HTML.Types.Swap
  ( SwapStyle
      ( InnerHTML
      , OuterHTML
      , BeforeBegin
      , AfterBegin
      , BeforeEnd
      , AfterEnd
      , SwapDelete
      , SwapNone
      )
  , swapStyleToBytes
  , swapStyleFromText
  , swapStyleToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data SwapStyle
  = InnerHTML
  | OuterHTML
  | BeforeBegin
  | AfterBegin
  | BeforeEnd
  | AfterEnd
  | SwapDelete
  | SwapNone
  deriving (Bounded, Enum, Eq, Show)

swapStyleToBytes :: SwapStyle -> LBS.ByteString
swapStyleToBytes style =
  case style of
    InnerHTML   -> "innerHTML"
    OuterHTML   -> "outerHTML"
    BeforeBegin -> "beforebegin"
    AfterBegin  -> "afterbegin"
    BeforeEnd   -> "beforeend"
    AfterEnd    -> "afterend"
    SwapDelete  -> "delete"
    SwapNone    -> "none"

swapStyleFromText :: T.Text -> Either String SwapStyle
swapStyleFromText txt =
  case txt of
    "innerHTML"   -> Right InnerHTML
    "outerHTML"   -> Right OuterHTML
    "beforebegin" -> Right BeforeBegin
    "afterbegin"  -> Right AfterBegin
    "beforeend"   -> Right BeforeEnd
    "afterend"    -> Right AfterEnd
    "delete"      -> Right SwapDelete
    "none"        -> Right SwapNone
    _             -> Left $ "Unknown SwapStyle: " <> T.unpack txt

swapStyleToText :: SwapStyle -> T.Text
swapStyleToText style =
  case style of
    InnerHTML   -> "innerHTML"
    OuterHTML   -> "outerHTML"
    BeforeBegin -> "beforebegin"
    AfterBegin  -> "afterbegin"
    BeforeEnd   -> "beforeend"
    AfterEnd    -> "afterend"
    SwapDelete  -> "delete"
    SwapNone    -> "none"
