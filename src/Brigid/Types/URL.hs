{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.Types.URL
  ( URL
  , URLTypes
  , mkURL
  , urlToBytes
  , urlToText
  , AbsoluteURL
  , mkAbsoluteURL
  , absoluteURLFromText
  , absoluteURLToBytes
  , absoluteURLToText
  , RelativeURL
      ( Relative_Get
      , Relative_Post
      , Relative_Delete
      , Relative_Put
      , Relative_Patch
      )
  , eqRelativeURL
  , get
  , post
  , delete
  , put
  , patch
  , relativeURLToBytes
  , relativeURLToText
  , RawURL
  , mkRawURL
  , rawURLToBytes
  , rawURLToText
  , Ping
  , PingTypes
  , mkPing
  , pingToBytes
  , pingToText
  ) where

import Beeline.HTTP.Client qualified as B
import Beeline.Routing qualified as R
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Function (on)
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.Types.Method (Method, Get, Post, Delete, Put, Patch)

newtype URL =
  URL (Shrubbery.Union URLTypes)
    deriving (Eq, Show)

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

urlToBytes :: URL -> LBS.ByteString
urlToBytes (URL url) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AbsoluteURL       absoluteURLToBytes
      . Shrubbery.branch @(RelativeURL Get) relativeURLToBytes
      . Shrubbery.branch @RawURL            rawURLToBytes
      $ Shrubbery.branchEnd
  ) url

urlToText :: URL -> T.Text
urlToText (URL url) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AbsoluteURL       absoluteURLToText
      . Shrubbery.branch @(RelativeURL Get) relativeURLToText
      . Shrubbery.branch @RawURL            rawURLToText
      $ Shrubbery.branchEnd
  ) url

newtype AbsoluteURL = AbsoluteURL B.BaseURI
  deriving (Eq)

instance Show AbsoluteURL where
  show = mappend "AbsoluteURL " . T.unpack . absoluteURLToText

mkAbsoluteURL :: B.BaseURI -> AbsoluteURL
mkAbsoluteURL = AbsoluteURL

absoluteURLFromText :: T.Text -> Either String AbsoluteURL
absoluteURLFromText =
  fmap AbsoluteURL . B.parseBaseURI . T.unpack

absoluteURLToBytes :: AbsoluteURL -> LBS.ByteString
absoluteURLToBytes (AbsoluteURL url) =
  LBS8.pack $ B.renderBaseURI url

absoluteURLToText :: AbsoluteURL -> T.Text
absoluteURLToText (AbsoluteURL url) =
  T.pack $ B.renderBaseURI url

data RelativeURL (method :: Method) where
  Relative_Get    :: T.Text -> RelativeURL Get
  Relative_Post   :: T.Text -> RelativeURL Post
  Relative_Delete :: T.Text -> RelativeURL Delete
  Relative_Put    :: T.Text -> RelativeURL Put
  Relative_Patch  :: T.Text -> RelativeURL Patch

deriving instance Eq (RelativeURL method)
deriving instance Show (RelativeURL method)

eqRelativeURL :: RelativeURL m1 -> RelativeURL m2 -> Bool
eqRelativeURL url1 url2 =
  case (url1, url2) of
    (Relative_Get    a, Relative_Get    b) -> a == b
    (Relative_Post   a, Relative_Post   b) -> a == b
    (Relative_Delete a, Relative_Delete b) -> a == b
    (Relative_Put    a, Relative_Put    b) -> a == b
    (Relative_Patch  a, Relative_Patch  b) -> a == b
    (_url1, _url2) -> False

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

relativeURLToBytes :: RelativeURL method -> LBS.ByteString
relativeURLToBytes =
  LBS8.pack . T.unpack . relativeURLToText

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
newtype RawURL =
  RawURL
    { rawURLToText :: T.Text
    } deriving (Eq)

instance Show RawURL where
  show = mappend "RawURL " . T.unpack . rawURLToText

mkRawURL :: T.Text -> RawURL
mkRawURL = RawURL

rawURLToBytes :: RawURL -> LBS.ByteString
rawURLToBytes =
  LBS8.pack . T.unpack . rawURLToText

newtype Ping = Ping (Shrubbery.Union PingTypes)

instance Eq Ping where
  (==) = (==) `on` pingToText

instance Show Ping where
  show (Ping ping) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @AbsoluteURL        show
        . Shrubbery.branch @(RelativeURL Post) show
        . Shrubbery.branch @RawURL             show
        $ Shrubbery.branchEnd
    ) ping

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

pingToBytes :: Ping -> LBS.ByteString
pingToBytes (Ping ping) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AbsoluteURL absoluteURLToBytes
      . Shrubbery.branch @(RelativeURL Post) relativeURLToBytes
      . Shrubbery.branch @RawURL rawURLToBytes
      $ Shrubbery.branchEnd
  ) ping

pingToText :: Ping -> T.Text
pingToText (Ping ping) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AbsoluteURL absoluteURLToText
      . Shrubbery.branch @(RelativeURL Post) relativeURLToText
      . Shrubbery.branch @RawURL rawURLToText
      $ Shrubbery.branchEnd
  ) ping
