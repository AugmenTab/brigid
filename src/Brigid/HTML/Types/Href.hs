{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Href
  ( Href
  , mkHref
  , unHref
  , HrefTypes
  , hrefToBytes
  , hrefToText
  , BlankHref (BlankHref)
  , HrefSelectorTypes
  , mkHrefSelector
  , hrefSelectorToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.PhoneNumber.Util (PhoneNumberFormat (RFC3966), formatNumber)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.Phone (PhoneNumber)
import Brigid.Types.EmailAddress (EmailAddress, emailAddressToBytes, emailAddressToText)
import Brigid.Types.Id (Id, idToBytes, idToText)
import Brigid.Types.Method (Get, Post)
import Brigid.Types.URL qualified as URL

newtype Href =
  Href
    { unHref :: Shrubbery.Union HrefTypes
    } deriving (Eq, Show)

type HrefTypes =
  [ URL.AbsoluteURL
  , URL.RelativeURL Get
  , Id
  , EmailAddress
  , PhoneNumber
  -- TODO: SMS; will require escaping the text portion during rendering
  , BlankHref
  , URL.RawURL
  ]

mkHref :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf href HrefTypes
          )
       => href -> Href
mkHref =
  Href . Shrubbery.unify

hrefToBytes :: Href -> LBS.ByteString
hrefToBytes (Href href) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @URL.AbsoluteURL     URL.absoluteURLToBytes
      . Shrubbery.branch @(URL.RelativeURL _) URL.relativeURLToBytes
      . Shrubbery.branch @Id                  (LBS8.cons '#' . idToBytes)
      . Shrubbery.branch @EmailAddress        (("mailto:" <>) . emailAddressToBytes)
      . Shrubbery.branch @PhoneNumber         (LBS.fromStrict . formatNumber RFC3966)
      . Shrubbery.branch @BlankHref           (const "#")
      . Shrubbery.branch @URL.RawURL          URL.rawURLToBytes
      $ Shrubbery.branchEnd
  ) href

hrefToText :: Href -> T.Text
hrefToText (Href href) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @URL.AbsoluteURL     URL.absoluteURLToText
      . Shrubbery.branch @(URL.RelativeURL _) URL.relativeURLToText
      . Shrubbery.branch @Id                  (T.cons '#' . idToText)
      . Shrubbery.branch @EmailAddress        (("mailto:" <>) . emailAddressToText)
      . Shrubbery.branch @PhoneNumber         (TE.decodeUtf8 . formatNumber RFC3966)
      . Shrubbery.branch @BlankHref           (const "#")
      . Shrubbery.branch @URL.RawURL          URL.rawURLToText
      $ Shrubbery.branchEnd
  ) href

data BlankHref = BlankHref
  deriving (Eq, Show)

newtype HrefSelector =
  HrefSelector
    { unHrefSelector :: Shrubbery.Union HrefSelectorTypes
    }

type HrefSelectorTypes =
  [ URL.AbsoluteURL
  , URL.RelativeURL Get
  , URL.RelativeURL Post
  , Id
  , EmailAddress
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
      . Shrubbery.branch @URL.AbsoluteURL        URL.absoluteURLToText
      . Shrubbery.branch @(URL.RelativeURL Get)  URL.relativeURLToText
      . Shrubbery.branch @(URL.RelativeURL Post) URL.relativeURLToText
      . Shrubbery.branch @Id                     (T.cons '#' . idToText)
      . Shrubbery.branch @EmailAddress           (("mailto:" <>) . emailAddressToText)
      . Shrubbery.branch @BlankHref              (const "#")
      . Shrubbery.branch @URL.RawURL             URL.rawURLToText
      $ Shrubbery.branchEnd
  ) . unHrefSelector
