{-# LANGUAGE DataKinds #-}

module HTML.Types.Method
  ( Method
      ( GET
      , POST
      , DELETE
      , PUT
      , PATCH
      )
  , methodToBytes
  , methodFromText
  , methodToText
  , Get
  , Post
  , Delete
  , Put
  , Patch
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data Method
  = GET
  | POST
  | DELETE
  | PUT
  | PATCH

methodToBytes :: Method -> LBS.ByteString
methodToBytes method =
  case method of
    GET    -> "get"
    POST   -> "post"
    DELETE -> "delete"
    PUT    -> "put"
    PATCH  -> "patch"

methodFromText :: T.Text -> Either String Method
methodFromText txt =
  case txt of
    "get"    -> Right GET
    "post"   -> Right POST
    "delete" -> Right DELETE
    "put"    -> Right PUT
    "patch"  -> Right PATCH
    _        -> Left $ "Invalid Method: " <> T.unpack txt

methodToText :: Method -> T.Text
methodToText method =
  case method of
    GET    -> "get"
    POST   -> "post"
    DELETE -> "delete"
    PUT    -> "put"
    PATCH  -> "patch"

type Get = 'GET

type Post = 'POST

type Delete = 'DELETE

type Put = 'PUT

type Patch = 'PATCH
