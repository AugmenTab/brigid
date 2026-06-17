module Brigid.HTML.Types.HttpEquivToken
  ( HttpEquivToken
      ( ContentType
      , DefaultStyle
      , Refresh
      , X_UA_Compatible
      , PermissionsPolicy
      , CacheControl
      , Pragma
      , ContentSecurityPolicy
      )
  , httpEquivTokenToBytes
  , httpEquivTokenToBytesBuilder
  , httpEquivTokenToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data HttpEquivToken
  = ContentType
  -- ^ Specifies the MIME type and character encoding for the document.
  | DefaultStyle
  -- ^ Specifies the preferred stylesheet to use.
  | Refresh
  -- ^ Specifies a redirect or auto-refresh interval.
  | X_UA_Compatible
  -- ^ Specifies which version of Internet Explorer (IE) the page is compatible with.
  | PermissionsPolicy
  -- ^ Specifies permissions for APIs and features (formerly known as Feature-Policy).
  | CacheControl
  -- ^ Provides caching instructions to the browser (similar to the HTTP Cache-Control header).
  | Pragma
  -- ^ Provides backward compatibility for caching instructions (rarely used, replaced by cache-control).
  | ContentSecurityPolicy
  -- ^ Specifies a Content Security Policy (CSP) to restrict or control resources loaded by the document.
  deriving (Bounded, Enum, Eq, Show)

httpEquivTokenToBytes :: HttpEquivToken -> LBS.ByteString
httpEquivTokenToBytes token =
  case token of
    ContentType           -> "content-type"
    DefaultStyle          -> "default-style"
    Refresh               -> "refresh"
    X_UA_Compatible       -> "x-ua-compatible"
    PermissionsPolicy     -> "permissions-policy"
    CacheControl          -> "cache-control"
    Pragma                -> "pragma"
    ContentSecurityPolicy -> "content-security-policy"

httpEquivTokenToBytesBuilder :: HttpEquivToken -> Builder
{-# INLINE httpEquivTokenToBytesBuilder #-}
httpEquivTokenToBytesBuilder token =
  case token of
    ContentType           -> string8 "content-type"
    DefaultStyle          -> string8 "default-style"
    Refresh               -> string8 "refresh"
    X_UA_Compatible       -> string8 "x-ua-compatible"
    PermissionsPolicy     -> string8 "permissions-policy"
    CacheControl          -> string8 "cache-control"
    Pragma                -> string8 "pragma"
    ContentSecurityPolicy -> string8 "content-security-policy"

httpEquivTokenToText :: HttpEquivToken -> T.Text
httpEquivTokenToText token =
  case token of
    ContentType           -> "content-type"
    DefaultStyle          -> "default-style"
    Refresh               -> "refresh"
    X_UA_Compatible       -> "x-ua-compatible"
    PermissionsPolicy     -> "permissions-policy"
    CacheControl          -> "cache-control"
    Pragma                -> "pragma"
    ContentSecurityPolicy -> "content-security-policy"
