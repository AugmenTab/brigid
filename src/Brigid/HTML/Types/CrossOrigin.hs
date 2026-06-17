module Brigid.HTML.Types.CrossOrigin
  ( CrossOriginFetch
      ( Anonymous
      , UseCredentials
      )
  , crossOriginFetchToBytes
  , crossOriginFetchToBytesBuilder
  , crossOriginFetchToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

{-|
   Represents the options available for the 'crossorigin' attribute.
-}
data CrossOriginFetch
  -- | A cross-origin request (i.e. with an @Origin@ HTTP header) is performed,
  -- but no credential is sent (i.e. no cookie, X.509 certificate, or HTTP
  -- Basic authentication). If the server does not give credentials to the
  -- origin site (by not setting the @Access-Control-Allow-Origin@ HTTP header)
  -- the resource will be tainted and its usage restricted.
  = Anonymous
  -- | A cross-origin request (i.e. with an @Origin@ HTTP header) is performed
  -- along with a credential sent (i.e. a cookie, certificate, and/or HTTP
  -- Basic authentication is performed). If the server does not give
  -- credentials to the origin site (through @Access-Control-Allow-Credentials@
  -- HTTP header), the resource will be tainted and its usage restricted.
  | UseCredentials
  deriving (Bounded, Enum, Eq, Show)

crossOriginFetchToBytes :: CrossOriginFetch -> LBS.ByteString
crossOriginFetchToBytes cors =
  case cors of
    Anonymous      -> "anonymous"
    UseCredentials -> "use-credentials"

crossOriginFetchToBytesBuilder :: CrossOriginFetch -> Builder
{-# INLINE crossOriginFetchToBytesBuilder #-}
crossOriginFetchToBytesBuilder cors =
  case cors of
    Anonymous      -> string8 "anonymous"
    UseCredentials -> string8 "use-credentials"

crossOriginFetchToText :: CrossOriginFetch -> T.Text
crossOriginFetchToText cors =
  case cors of
    Anonymous      -> "anonymous"
    UseCredentials -> "use-credentials"
