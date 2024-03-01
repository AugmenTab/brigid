{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Types.Href
  ( Href
  , unHref
  , HrefTypes
  , mkHref
  , hrefToText
  ) where

import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import HTML.Types.Email (Email, emailToText)
import HTML.Types.Id (Id, idToText)
import HTML.Types.URL qualified as URL

newtype Href method =
  Href
    { unHref :: Shrubbery.Union (HrefTypes method)
    }

type HrefTypes method =
  [ URL.AbsoluteURL
  , URL.RelativeURL method
  , Id
  , Email
  -- TODO: PhoneNumber
  -- TODO: SMS; will require escaping the text portion during rendering
  , URL.RawURL
  ]

mkHref :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf a (HrefTypes method)
          )
       => a
       -> Href method
mkHref =
  Href . Shrubbery.unify

hrefToText :: Href method -> T.Text
hrefToText (Href href) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @URL.AbsoluteURL URL.absoluteURLToText
      . Shrubbery.branch @(URL.RelativeURL _) URL.relativeURLToText
      . Shrubbery.branch @Id (T.cons '#' . idToText)
      . Shrubbery.branch @Email (("mailto:" <>) . emailToText)
      . Shrubbery.branch @URL.RawURL URL.rawURLToText
      $ Shrubbery.branchEnd
  ) href
