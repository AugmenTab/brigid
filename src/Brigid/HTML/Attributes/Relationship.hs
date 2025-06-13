{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HTML.Attributes.Relationship
  ( ValidRelationship
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Elements.TagType (TagErrorMessage, TagType)
import Brigid.HTML.Internal.TagOperations (Elem, Remove)
import Brigid.HTML.Types qualified as Types

type ValidRelationship rel tag =
  AlertRelationship (Elem tag (ValidRelationshipsFor rel)) rel tag ~ 'True

type family ValidRelationshipsFor (rel :: Type) :: [TagType] where
  ValidRelationshipsFor Types.Rel_Alternate        = Remove Tags.Form TagGroups.RelTags
  ValidRelationshipsFor Types.Rel_Author           = Remove Tags.Form TagGroups.RelTags
  ValidRelationshipsFor Types.Rel_Bookmark         = [ Tags.Anchor, Tags.Area, Tags.CustomHTML ]
  ValidRelationshipsFor Types.Rel_Canonical        = [ Tags.CustomHTML, Tags.Link ]
  -- TODO: Valid for link only in body and head.
  ValidRelationshipsFor Types.Rel_DNS_Prefetch     = '[]

  ValidRelationshipsFor Types.Rel_External         = Remove Tags.Link TagGroups.RelTags

  -- TODO: Unsure which elements can accept this.
  ValidRelationshipsFor Types.Rel_Expect           = '[]

  ValidRelationshipsFor Types.Rel_Help             = TagGroups.RelTags
  ValidRelationshipsFor Types.Rel_Icon             = [ Tags.CustomHTML, Tags.Link ]
  ValidRelationshipsFor Types.Rel_License          = TagGroups.RelTags

  -- TODO: Unsure which elements can accept this.
  ValidRelationshipsFor Types.Rel_Manifest         = '[ Tags.CustomHTML ]

  -- TODO: Unsure which elements can accept this.
  ValidRelationshipsFor Types.Rel_Me               = '[ Tags.CustomHTML ]

  ValidRelationshipsFor Types.Rel_ModulePreload    = [ Tags.CustomHTML, Tags.Link ]
  ValidRelationshipsFor Types.Rel_Next             = TagGroups.RelTags
  ValidRelationshipsFor Types.Rel_NoFollow         = Remove Tags.Link TagGroups.RelTags
  ValidRelationshipsFor Types.Rel_NoOpener         = Remove Tags.Link TagGroups.RelTags
  ValidRelationshipsFor Types.Rel_NoReferrer       = Remove Tags.Link TagGroups.RelTags

  -- TODO: Unsure which elements can accept the following.
  ValidRelationshipsFor Types.Rel_Opener           = '[ Tags.CustomHTML ]
  ValidRelationshipsFor Types.Rel_Pingback         = '[ Tags.CustomHTML ]
  ValidRelationshipsFor Types.Rel_Preconnect       = '[ Tags.CustomHTML ]
  ValidRelationshipsFor Types.Rel_Prefetch         = '[ Tags.CustomHTML ]
  ValidRelationshipsFor Types.Rel_Preload          = '[ Tags.CustomHTML ]
  ValidRelationshipsFor Types.Rel_Prerender        = '[ Tags.CustomHTML ]

  ValidRelationshipsFor Types.Rel_Prev             = TagGroups.RelTags
  ValidRelationshipsFor Types.Rel_Privacy_Policy   = Remove Tags.Form TagGroups.RelTags
  ValidRelationshipsFor Types.Rel_Search           = TagGroups.RelTags
  ValidRelationshipsFor Types.Rel_Stylesheet       = [ Tags.CustomHTML, Tags.Link ]
  ValidRelationshipsFor Types.Rel_Tag              = [ Tags.Anchor, Tags.Area, Tags.CustomHTML ]
  ValidRelationshipsFor Types.Rel_Terms_Of_Service = Remove Tags.Form TagGroups.RelTags

type family AlertRelationship (member :: Bool) (rel :: Type) (tag :: TagType) :: Bool where
  AlertRelationship 'True rel tag =
    'True

  AlertRelationship 'False rel tag =
    TypeError
      ( 'Text "The "
          ':<>: RelationshipTypeErrorMessage rel
          ':<>: 'Text " rel type is not valid for the "
          ':<>: TagErrorMessage tag
          ':<>: 'Text " element."
      )

type family RelationshipTypeErrorMessage (rel :: Type) :: ErrorMessage where
  RelationshipTypeErrorMessage Types.Rel_Alternate        = 'Text "Alternate"
  RelationshipTypeErrorMessage Types.Rel_Author           = 'Text "Author"
  RelationshipTypeErrorMessage Types.Rel_Bookmark         = 'Text "Bookmark"
  RelationshipTypeErrorMessage Types.Rel_Canonical        = 'Text "Canonical"
  RelationshipTypeErrorMessage Types.Rel_DNS_Prefetch     = 'Text "DNS_Prefetch"
  RelationshipTypeErrorMessage Types.Rel_External         = 'Text "External"
  RelationshipTypeErrorMessage Types.Rel_Expect           = 'Text "Expect"
  RelationshipTypeErrorMessage Types.Rel_Help             = 'Text "Help"
  RelationshipTypeErrorMessage Types.Rel_Icon             = 'Text "Icon"
  RelationshipTypeErrorMessage Types.Rel_License          = 'Text "License"
  RelationshipTypeErrorMessage Types.Rel_Manifest         = 'Text "Manifest"
  RelationshipTypeErrorMessage Types.Rel_Me               = 'Text "Me"
  RelationshipTypeErrorMessage Types.Rel_ModulePreload    = 'Text "ModulePreload"
  RelationshipTypeErrorMessage Types.Rel_Next             = 'Text "Next"
  RelationshipTypeErrorMessage Types.Rel_NoFollow         = 'Text "NoFollow"
  RelationshipTypeErrorMessage Types.Rel_NoOpener         = 'Text "NoOpener"
  RelationshipTypeErrorMessage Types.Rel_NoReferrer       = 'Text "NoReferrer"
  RelationshipTypeErrorMessage Types.Rel_Opener           = 'Text "Opener"
  RelationshipTypeErrorMessage Types.Rel_Pingback         = 'Text "Pingback"
  RelationshipTypeErrorMessage Types.Rel_Preconnect       = 'Text "Preconnect"
  RelationshipTypeErrorMessage Types.Rel_Prefetch         = 'Text "Prefetch"
  RelationshipTypeErrorMessage Types.Rel_Preload          = 'Text "Preload"
  RelationshipTypeErrorMessage Types.Rel_Prerender        = 'Text "Prerender"
  RelationshipTypeErrorMessage Types.Rel_Prev             = 'Text "Prev"
  RelationshipTypeErrorMessage Types.Rel_Privacy_Policy   = 'Text "Privacy_Policy"
  RelationshipTypeErrorMessage Types.Rel_Search           = 'Text "Search"
  RelationshipTypeErrorMessage Types.Rel_Stylesheet       = 'Text "Stylesheet"
  RelationshipTypeErrorMessage Types.Rel_Tag              = 'Text "Tag"
  RelationshipTypeErrorMessage Types.Rel_Terms_Of_Service = 'Text "Terms_Of_Service"
