{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Types.PushURL
  ( PushURL
  , mkPushURL
  , unPushURL
  , PushURLTypes
  , pushURLToText
  ) where

import Data.Bool qualified as B
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import HTML.Types.Method (Get)
import HTML.Types.URL qualified as URL

newtype PushURL =
  PushURL
    { unPushURL :: Shrubbery.Union PushURLTypes
    }

type PushURLTypes =
  [ URL.AbsoluteURL
  , URL.RelativeURL Get
  , Bool
  , URL.RawURL
  ]

mkPushURL :: (KnownNat branchIndex, branchIndex ~ FirstIndexOf a PushURLTypes)
          => a -> PushURL
mkPushURL =
  PushURL . Shrubbery.unify

pushURLToText :: PushURL -> T.Text
pushURLToText (PushURL url) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @URL.AbsoluteURL URL.absoluteURLToText
      . Shrubbery.branch @(URL.RelativeURL _) URL.relativeURLToText
      . Shrubbery.branch @Bool (B.bool "false" "true")
      . Shrubbery.branch @URL.RawURL URL.rawURLToText
      $ Shrubbery.branchEnd
  ) url
