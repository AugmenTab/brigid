{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Action
  ( Action
  , ActionTypes
  , mkAction
  , actionToBytes
  , actionToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Function (on)
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.Types.Method (Get, Post)
import Brigid.Types.URL qualified as URL

newtype Action = Action (Shrubbery.Union ActionTypes)

instance Eq Action where
  (==) = (==) `on` actionToText

instance Show Action where
  show = T.unpack . actionToText

type ActionTypes =
  [ URL.AbsoluteURL
  , URL.RelativeURL Get
  , URL.RelativeURL Post
  , URL.RawURL
  ]

mkAction :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf action ActionTypes
            )
         => action -> Action
mkAction =
  Action . Shrubbery.unify

actionToBytes :: Action -> LBS.ByteString
actionToBytes (Action action) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @URL.AbsoluteURL        URL.absoluteURLToBytes
      . Shrubbery.branch @(URL.RelativeURL Get)  URL.relativeURLToBytes
      . Shrubbery.branch @(URL.RelativeURL Post) URL.relativeURLToBytes
      . Shrubbery.branch @URL.RawURL             URL.rawURLToBytes
      $ Shrubbery.branchEnd
  ) action

actionToText :: Action -> T.Text
actionToText (Action action) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @URL.AbsoluteURL        URL.absoluteURLToText
      . Shrubbery.branch @(URL.RelativeURL Get)  URL.relativeURLToText
      . Shrubbery.branch @(URL.RelativeURL Post) URL.relativeURLToText
      . Shrubbery.branch @URL.RawURL             URL.rawURLToText
      $ Shrubbery.branchEnd
  ) action
