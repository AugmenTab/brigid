{-# LANGUAGE DataKinds #-}

module HTML.Types.Method
  ( Method
      ( GET
      , POST
      , DELETE
      , PUT
      , PATCH
      )
  , Get
  , Post
  , Delete
  , Put
  , Patch
  ) where

data Method
  = GET
  | POST
  | DELETE
  | PUT
  | PATCH

type Get = 'GET

type Post = 'POST

type Delete = 'DELETE

type Put = 'PUT

type Patch = 'PATCH
