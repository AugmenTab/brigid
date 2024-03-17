{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Types.Href
  ( Href
  , mkHref
  , unHref
  , HrefTypes
  , hrefToText
  , BlankHref (BlankHref)
  , HrefSelectorTypes
  , mkHrefSelector
  , hrefSelectorToText
  ) where

import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import HTML.Types.Email (Email, emailToText)
import HTML.Types.Id (Id, idToText)
import HTML.Types.Method (Get, Post)
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
  , BlankHref
  , URL.RawURL
  ]

mkHref :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf href (HrefTypes method)
          )
       => href -> Href method
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
      . Shrubbery.branch @BlankHref (const "#")
      . Shrubbery.branch @URL.RawURL URL.rawURLToText
      $ Shrubbery.branchEnd
  ) href

data BlankHref = BlankHref

newtype HrefSelector =
  HrefSelector
    { unHrefSelector :: Shrubbery.Union HrefSelectorTypes
    }

type HrefSelectorTypes =
  [ URL.AbsoluteURL
  , URL.RelativeURL Get
  , URL.RelativeURL Post
  , Id
  , Email
  -- TODO: PhoneNumber
  -- TODO: SMS; will require escaping the text portion during rendering
  , BlankHref
  , URL.RawURL
  ]

mkHrefSelector :: ( KnownNat branchIndex
                  , branchIndex ~ FirstIndexOf href HrefSelectorTypes
                  )
               => href -> HrefSelector
mkHrefSelector =
  HrefSelector . Shrubbery.unify

hrefSelectorToText :: HrefSelector -> T.Text
hrefSelectorToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @URL.AbsoluteURL URL.absoluteURLToText
      . Shrubbery.branch @(URL.RelativeURL Get) URL.relativeURLToText
      . Shrubbery.branch @(URL.RelativeURL Post) URL.relativeURLToText
      . Shrubbery.branch @Id (T.cons '#' . idToText)
      . Shrubbery.branch @Email (("mailto:" <>) . emailToText)
      . Shrubbery.branch @BlankHref (const "#")
      . Shrubbery.branch @URL.RawURL URL.rawURLToText
      $ Shrubbery.branchEnd
  ) . unHrefSelector

