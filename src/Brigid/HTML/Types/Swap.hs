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
  , swapStyleToBytesBuilder
  , swapStyleFromText
  , swapStyleToText
  ) where

import Data.ByteString.Builder (Builder, string8)
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

swapStyleToBytesBuilder :: SwapStyle -> Builder
{-# INLINE swapStyleToBytesBuilder #-}
swapStyleToBytesBuilder style =
  case style of
    InnerHTML   -> string8 "innerHTML"
    OuterHTML   -> string8 "outerHTML"
    BeforeBegin -> string8 "beforebegin"
    AfterBegin  -> string8 "afterbegin"
    BeforeEnd   -> string8 "beforeend"
    AfterEnd    -> string8 "afterend"
    SwapDelete  -> string8 "delete"
    SwapNone    -> string8 "none"

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
