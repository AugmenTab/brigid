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
  , referrerPolicyToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

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
{-# INLINE referrerPolicyToBytesBuilder #-}
referrerPolicyToBytesBuilder = lazyByteString . referrerPolicyToBytes

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

referrerPolicyToTextBuilder :: ReferrerPolicy -> TBL.Builder
referrerPolicyToTextBuilder = TBL.fromText . referrerPolicyToText

