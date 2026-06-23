{-# LANGUAGE DataKinds #-}

module Brigid.Types.Method
  ( Method
      ( GET
      , POST
      , DELETE
      , PUT
      , PATCH
      )
  , methodFromText
  , methodToBytes
  , methodToText
  , Get
  , Post
  , Delete
  , Put
  , Patch
  , FormMethod
      ( FormGET
      , FormPOST
      )
  , formMethodToBytes
  , formMethodToBytesBuilder
  , formMethodToText
  , formMethodToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data Method
  = GET
  | POST
  | DELETE
  | PUT
  | PATCH

methodFromText :: T.Text -> Either String Method
methodFromText txt =
  case txt of
    "get"    -> Right GET
    "post"   -> Right POST
    "delete" -> Right DELETE
    "put"    -> Right PUT
    "patch"  -> Right PATCH
    _        -> Left $ "Invalid Method: " <> T.unpack txt

methodToBytes :: Method -> LBS.ByteString
methodToBytes method =
  case method of
    GET    -> "get"
    POST   -> "post"
    DELETE -> "delete"
    PUT    -> "put"
    PATCH  -> "patch"

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

data FormMethod
  = FormGET
  | FormPOST
  deriving (Bounded, Enum, Eq, Show)

formMethodToBytes :: FormMethod -> LBS.ByteString
formMethodToBytes method =
  case method of
    FormGET  -> methodToBytes GET
    FormPOST -> methodToBytes POST

formMethodToBytesBuilder :: FormMethod -> Builder
{-# INLINE formMethodToBytesBuilder #-}
formMethodToBytesBuilder method =
  case method of
    FormGET  -> string8 "get"
    FormPOST -> string8 "post"

formMethodToText :: FormMethod -> T.Text
formMethodToText method =
  case method of
    FormGET  -> methodToText GET
    FormPOST -> methodToText POST

formMethodToTextBuilder :: FormMethod -> TBL.Builder
formMethodToTextBuilder = TBL.fromText . formMethodToText
