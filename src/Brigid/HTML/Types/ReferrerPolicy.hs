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
  , referrerPolicyToText
  ) where

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

