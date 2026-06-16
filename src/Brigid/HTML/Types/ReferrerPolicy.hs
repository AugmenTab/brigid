module Brigid.HTML.Types.ReferrerPolicy
  ( ReferrerPolicy
      ( NoReferrer
      , Origin
      , NoReferrerWhenDowngrade
      , OriginWhenCrossOrigin
      , SameOrigin
      , StrictOrigin
      , StrictOriginWhenCrossOrigin
      , UnsafeURL
      )
  , referrerPolicyToBytes
  , referrerPolicyToBytesBuilder
  , referrerPolicyToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data ReferrerPolicy
  = NoReferrer
  | Origin
  | NoReferrerWhenDowngrade
  | OriginWhenCrossOrigin
  | SameOrigin
  | StrictOrigin
  | StrictOriginWhenCrossOrigin
  | UnsafeURL
  deriving (Bounded, Enum, Eq, Show)

referrerPolicyToBytes :: ReferrerPolicy -> LBS.ByteString
referrerPolicyToBytes referrer =
  case referrer of
    NoReferrer                  -> "no-referrer"
    Origin                      -> "origin"
    NoReferrerWhenDowngrade     -> "no-referrer-when-downgrade"
    OriginWhenCrossOrigin       -> "origin-when-cross-origin"
    SameOrigin                  -> "same-origin"
    StrictOrigin                -> "strict-origin"
    StrictOriginWhenCrossOrigin -> "strict-origin-when-cross-origin"
    UnsafeURL                   -> "unsafe-url"

referrerPolicyToBytesBuilder :: ReferrerPolicy -> Builder
referrerPolicyToBytesBuilder referrer =
  case referrer of
    NoReferrer                  -> string8 "no-referrer"
    Origin                      -> string8 "origin"
    NoReferrerWhenDowngrade     -> string8 "no-referrer-when-downgrade"
    OriginWhenCrossOrigin       -> string8 "origin-when-cross-origin"
    SameOrigin                  -> string8 "same-origin"
    StrictOrigin                -> string8 "strict-origin"
    StrictOriginWhenCrossOrigin -> string8 "strict-origin-when-cross-origin"
    UnsafeURL                   -> string8 "unsafe-url"

referrerPolicyToText :: ReferrerPolicy -> T.Text
referrerPolicyToText referrer =
  case referrer of
    NoReferrer                  -> "no-referrer"
    Origin                      -> "origin"
    NoReferrerWhenDowngrade     -> "no-referrer-when-downgrade"
    OriginWhenCrossOrigin       -> "origin-when-cross-origin"
    SameOrigin                  -> "same-origin"
    StrictOrigin                -> "strict-origin"
    StrictOriginWhenCrossOrigin -> "strict-origin-when-cross-origin"
    UnsafeURL                   -> "unsafe-url"

