{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.URL
  ( URL
  , URLTypes
  , mkURL
  , urlToText
  , AbsoluteURL
  , absoluteURLFromText
  , absoluteURLToText
  , RelativeURL
      ( Relative_Get
      , Relative_Post
      , Relative_Delete
      , Relative_Put
      , Relative_Patch
      )
  , get
  , post
  , delete
  , put
  , patch
  , relativeURLToText
  , RawURL (..)
  , rawURLFromText
  , rawURLToText
  , Ping
  , PingTypes
  , mkPing
  , pingToText
  ) where

import Beeline.HTTP.Client qualified as B
import Beeline.Routing qualified as R
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.Method (Method, Get, Post, Delete, Put, Patch)

newtype URL = URL (Shrubbery.Union URLTypes)

type URLTypes =
  [ AbsoluteURL
  , RelativeURL Get
  , RawURL
  ]

mkURL :: ( KnownNat branchIndex
         , branchIndex ~ FirstIndexOf url URLTypes
         )
      => url -> URL
mkURL =
  URL . Shrubbery.unify

urlToText :: URL -> T.Text
urlToText (URL url) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AbsoluteURL absoluteURLToText
      . Shrubbery.branch @(RelativeURL Get) relativeURLToText
      . Shrubbery.branch @RawURL rawURLToText
      $ Shrubbery.branchEnd
  ) url

newtype AbsoluteURL = AbsoluteURL B.BaseURI

absoluteURLFromText :: T.Text -> Either String AbsoluteURL
absoluteURLFromText =
  fmap AbsoluteURL . B.parseBaseURI . T.unpack

absoluteURLToText :: AbsoluteURL -> T.Text
absoluteURLToText (AbsoluteURL url) =
  T.pack $ B.renderBaseURI url

data RelativeURL (method :: Method) where
  Relative_Get    :: T.Text -> RelativeURL Get
  Relative_Post   :: T.Text -> RelativeURL Post
  Relative_Delete :: T.Text -> RelativeURL Delete
  Relative_Put    :: T.Text -> RelativeURL Put
  Relative_Patch  :: T.Text -> RelativeURL Patch

get :: route -> R.Builder R.RouteGenerator route route -> RelativeURL Get
get route =
  Relative_Get . snd . flip R.generateRoute route . R.get

post :: route -> R.Builder R.RouteGenerator route route -> RelativeURL Post
post route =
  Relative_Post . snd . flip R.generateRoute route . R.post

delete :: route -> R.Builder R.RouteGenerator route route -> RelativeURL Delete
delete route =
  Relative_Delete . snd . flip R.generateRoute route . R.delete

put :: route -> R.Builder R.RouteGenerator route route -> RelativeURL Put
put route =
  Relative_Put . snd . flip R.generateRoute route . R.put

patch :: route -> R.Builder R.RouteGenerator route route -> RelativeURL Patch
patch route =
  Relative_Patch . snd . flip R.generateRoute route . R.patch

relativeURLToText :: RelativeURL method -> T.Text
relativeURLToText url =
  case url of
    Relative_Get    path -> path
    Relative_Post   path -> path
    Relative_Delete path -> path
    Relative_Put    path -> path
    Relative_Patch  path -> path

-- | This is a basic wrapper around 'T.Text'. It assumes that anything it
-- contains is encoded as appropriate for whatever use case it finds itself in.
-- It provides an unsafe constructor for URLs; it should be handled with care
-- and used only when necessary.
newtype RawURL = RawURL T.Text

rawURLFromText :: T.Text -> RawURL
rawURLFromText = RawURL

rawURLToText :: RawURL -> T.Text
rawURLToText (RawURL url) = url

newtype Ping = Ping (Shrubbery.Union PingTypes)

type PingTypes =
  [ AbsoluteURL
  , RelativeURL Post
  , RawURL
  ]

mkPing :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf url PingTypes
          )
       => url -> Ping
mkPing =
  Ping . Shrubbery.unify

pingToText :: Ping -> T.Text
pingToText (Ping ping) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AbsoluteURL absoluteURLToText
      . Shrubbery.branch @(RelativeURL Post) relativeURLToText
      . Shrubbery.branch @RawURL rawURLToText
      $ Shrubbery.branchEnd
  ) ping
