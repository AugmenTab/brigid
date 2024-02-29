{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Types.Href
  ( Href
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

newtype Href =
  Href (Shrubbery.Union HrefTypes)

type HrefTypes =
  [ URL.AbsoluteURL
  , URL.RelativeURL
  , Id
  , Email
  -- TODO: PhoneNumber
  -- TODO: SMS
  , URL.RawURL
  ]

mkHref :: (KnownNat branchIndex, branchIndex ~ FirstIndexOf a HrefTypes)
       => a
       -> Href
mkHref =
  Href . Shrubbery.unify

hrefToText :: Href -> T.Text
hrefToText (Href href) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @URL.AbsoluteURL URL.absoluteURLToText
      . Shrubbery.branch @URL.RelativeURL URL.relativeURLToText
      . Shrubbery.branch @Id (T.cons '#' . idToText)
      . Shrubbery.branch @Email (("mailto:" <>) . emailToText)
      . Shrubbery.branch @URL.RawURL URL.rawURLToText
      $ Shrubbery.branchEnd
  ) href
