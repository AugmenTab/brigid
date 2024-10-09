{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Relationship
  ( Relationship
  , RelationshipTypes
  , mkRelationship
  , relationshipToBytes
  , relationshipToText
  , Rel_Alternate (Rel_Alternate)
  , Rel_Author (Rel_Author)
  , Rel_Bookmark (Rel_Bookmark)
  , Rel_Canonical (Rel_Canonical)
  , Rel_DNS_Prefetch (Rel_DNS_Prefetch)
  , Rel_External (Rel_External)
  , Rel_Expect (Rel_Expect)
  , Rel_Help (Rel_Help)
  , Rel_Icon (Rel_Icon)
  , Rel_License (Rel_License)
  , Rel_Manifest (Rel_Manifest)
  , Rel_Me (Rel_Me)
  , Rel_ModulePreload (Rel_ModulePreload)
  , Rel_Next (Rel_Next)
  , Rel_NoFollow (Rel_NoFollow)
  , Rel_NoOpener (Rel_NoOpener)
  , Rel_NoReferrer (Rel_NoReferrer)
  , Rel_Opener (Rel_Opener)
  , Rel_Pingback (Rel_Pingback)
  , Rel_Preconnect (Rel_Preconnect)
  , Rel_Prefetch (Rel_Prefetch)
  , Rel_Preload (Rel_Preload)
  , Rel_Prerender (Rel_Prerender)
  , Rel_Prev (Rel_Prev)
  , Rel_Privacy_Policy (Rel_Privacy_Policy)
  , Rel_Search (Rel_Search)
  , Rel_Stylesheet (Rel_Stylesheet)
  , Rel_Tag (Rel_Tag)
  , Rel_Terms_Of_Service (Rel_Terms_Of_Service)
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

newtype Relationship = Relationship (Shrubbery.Union RelationshipTypes)

type RelationshipTypes =
  [ Rel_Alternate
  , Rel_Author
  , Rel_Bookmark
  , Rel_Canonical
  , Rel_DNS_Prefetch
  , Rel_External
  , Rel_Expect
  , Rel_Help
  , Rel_Icon
  , Rel_License
  , Rel_Manifest
  , Rel_Me
  , Rel_ModulePreload
  , Rel_Next
  , Rel_NoFollow
  , Rel_NoOpener
  , Rel_NoReferrer
  , Rel_Opener
  , Rel_Pingback
  , Rel_Preconnect
  , Rel_Prefetch
  , Rel_Preload
  , Rel_Prerender
  , Rel_Prev
  , Rel_Privacy_Policy
  , Rel_Search
  , Rel_Stylesheet
  , Rel_Tag
  , Rel_Terms_Of_Service
  ]

mkRelationship :: ( KnownNat branchIndex
                  , branchIndex ~ FirstIndexOf rel RelationshipTypes
                  )
               => rel -> Relationship
mkRelationship =
  Relationship . Shrubbery.unify

relationshipToBytes :: Relationship -> LBS.ByteString
relationshipToBytes (Relationship rel) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Rel_Alternate        (const "alternate")
      . Shrubbery.branch @Rel_Author           (const "author")
      . Shrubbery.branch @Rel_Bookmark         (const "bookmark")
      . Shrubbery.branch @Rel_Canonical        (const "canonical")
      . Shrubbery.branch @Rel_DNS_Prefetch     (const "dns-prefetch")
      . Shrubbery.branch @Rel_External         (const "external")
      . Shrubbery.branch @Rel_Expect           (const "expect")
      . Shrubbery.branch @Rel_Help             (const "help")
      . Shrubbery.branch @Rel_Icon             (const "icon")
      . Shrubbery.branch @Rel_License          (const "license")
      . Shrubbery.branch @Rel_Manifest         (const "manifest")
      . Shrubbery.branch @Rel_Me               (const "me")
      . Shrubbery.branch @Rel_ModulePreload    (const "modulepreload")
      . Shrubbery.branch @Rel_Next             (const "next")
      . Shrubbery.branch @Rel_NoFollow         (const "nofollow")
      . Shrubbery.branch @Rel_NoOpener         (const "noopener")
      . Shrubbery.branch @Rel_NoReferrer       (const "noreferrer")
      . Shrubbery.branch @Rel_Opener           (const "opener")
      . Shrubbery.branch @Rel_Pingback         (const "pingback")
      . Shrubbery.branch @Rel_Preconnect       (const "preconnect")
      . Shrubbery.branch @Rel_Prefetch         (const "prefetch")
      . Shrubbery.branch @Rel_Preload          (const "preload")
      . Shrubbery.branch @Rel_Prerender        (const "prerender")
      . Shrubbery.branch @Rel_Prev             (const "prev")
      . Shrubbery.branch @Rel_Privacy_Policy   (const "privacy-policy")
      . Shrubbery.branch @Rel_Search           (const "search")
      . Shrubbery.branch @Rel_Stylesheet       (const "stylesheet")
      . Shrubbery.branch @Rel_Tag              (const "tag")
      . Shrubbery.branch @Rel_Terms_Of_Service (const "terms-of-service")
      $ Shrubbery.branchEnd
  ) rel

relationshipToText :: Relationship -> T.Text
relationshipToText (Relationship rel) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Rel_Alternate        (const "alternate")
      . Shrubbery.branch @Rel_Author           (const "author")
      . Shrubbery.branch @Rel_Bookmark         (const "bookmark")
      . Shrubbery.branch @Rel_Canonical        (const "canonical")
      . Shrubbery.branch @Rel_DNS_Prefetch     (const "dns-prefetch")
      . Shrubbery.branch @Rel_External         (const "external")
      . Shrubbery.branch @Rel_Expect           (const "expect")
      . Shrubbery.branch @Rel_Help             (const "help")
      . Shrubbery.branch @Rel_Icon             (const "icon")
      . Shrubbery.branch @Rel_License          (const "license")
      . Shrubbery.branch @Rel_Manifest         (const "manifest")
      . Shrubbery.branch @Rel_Me               (const "me")
      . Shrubbery.branch @Rel_ModulePreload    (const "modulepreload")
      . Shrubbery.branch @Rel_Next             (const "next")
      . Shrubbery.branch @Rel_NoFollow         (const "nofollow")
      . Shrubbery.branch @Rel_NoOpener         (const "noopener")
      . Shrubbery.branch @Rel_NoReferrer       (const "noreferrer")
      . Shrubbery.branch @Rel_Opener           (const "opener")
      . Shrubbery.branch @Rel_Pingback         (const "pingback")
      . Shrubbery.branch @Rel_Preconnect       (const "preconnect")
      . Shrubbery.branch @Rel_Prefetch         (const "prefetch")
      . Shrubbery.branch @Rel_Preload          (const "preload")
      . Shrubbery.branch @Rel_Prerender        (const "prerender")
      . Shrubbery.branch @Rel_Prev             (const "prev")
      . Shrubbery.branch @Rel_Privacy_Policy   (const "privacy-policy")
      . Shrubbery.branch @Rel_Search           (const "search")
      . Shrubbery.branch @Rel_Stylesheet       (const "stylesheet")
      . Shrubbery.branch @Rel_Tag              (const "tag")
      . Shrubbery.branch @Rel_Terms_Of_Service (const "terms-of-service")
      $ Shrubbery.branchEnd
  ) rel

data Rel_Alternate = Rel_Alternate

data Rel_Author = Rel_Author

data Rel_Bookmark = Rel_Bookmark

data Rel_Canonical = Rel_Canonical

data Rel_DNS_Prefetch = Rel_DNS_Prefetch

data Rel_External = Rel_External

data Rel_Expect = Rel_Expect

data Rel_Help = Rel_Help

data Rel_Icon = Rel_Icon

data Rel_License = Rel_License

data Rel_Manifest = Rel_Manifest

data Rel_Me = Rel_Me

data Rel_ModulePreload = Rel_ModulePreload

data Rel_Next = Rel_Next

data Rel_NoFollow = Rel_NoFollow

data Rel_NoOpener = Rel_NoOpener

data Rel_NoReferrer = Rel_NoReferrer

data Rel_Opener = Rel_Opener

data Rel_Pingback = Rel_Pingback

data Rel_Preconnect = Rel_Preconnect

data Rel_Prefetch = Rel_Prefetch

data Rel_Preload = Rel_Preload

data Rel_Prerender = Rel_Prerender

data Rel_Prev = Rel_Prev

data Rel_Privacy_Policy = Rel_Privacy_Policy

data Rel_Search = Rel_Search

data Rel_Stylesheet = Rel_Stylesheet

data Rel_Tag = Rel_Tag

data Rel_Terms_Of_Service = Rel_Terms_Of_Service
