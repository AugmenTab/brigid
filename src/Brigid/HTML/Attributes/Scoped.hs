{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Attributes.Scoped
  ( async
  , charset
  , content
  , crossorigin
  , defer
  , disable
  , disabled
  , href
  , name
  , referrerpolicy
  , rel
  ) where

import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Attributes.AttributeType (AttributeType (..))
import Brigid.HTML.Attributes.Elements (ValidAttribute)
import Brigid.HTML.Attributes.Href (ValidHref)
import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Attributes.Relationship (ValidRelationship)
import Brigid.HTML.Types qualified as Types

-- Scoped Attributes
--
async :: ValidAttribute 'Async tag => Attribute tag
async = Attr_Async

-- | Limited to UTF-8, since that is the only valid option for HTML5.
--
charset :: ValidAttribute 'Charset tag => Attribute tag
charset = Attr_Charset

-- | The `content` attribute is left as simple 'T.Text' because its value is
-- dependent on the `name` or `http-equiv` attributes, and managing the
-- inter-dependency across multiple attributes in the `meta` tag is too complex
-- to reconcile here, For safe construction of the `content` attributes
-- together with its dependencies on a `meta` tag, use
-- 'Brigid.HTML.Elements.Safe.Meta'.
--
content :: ValidAttribute 'Content tag => T.Text -> Attribute tag
content = Attr_Content

{-|
   This enumerated attribute indicates whether CORS must be used when fetching
   the resource. CORS-enabled images can be reused in the @<canvas>@ element
   without being tainted. If the attribute is not present, the resource is
   fetched without a CORS request (i.e. without sending the @Origin@ HTTP
   header), preventing its non-tainted usage. If invalid, it is handled as if
   the enumerated keyword @anonymous@ was used.
-}
crossorigin :: ValidAttribute 'CrossOrigin tag
            => Types.CrossOriginFetch
            -> Attribute tag
crossorigin = Attr_CrossOrigin

defer :: ValidAttribute 'Defer tag => Attribute tag
defer = Attr_Defer

disable :: ValidAttribute 'Disabled tag => Bool -> Attribute tag
disable = Attr_Disabled

disabled :: ValidAttribute 'Disabled tag => Attribute tag
disabled = disable True

href :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf href (Types.HrefTypes Types.Get)
        , ValidHref href tag
        , ValidAttribute 'Href tag
        )
     => href -> Attribute tag
href =
  Attr_Href . Types.mkHref

-- | The `name` attribute is left as simple 'T.Text' because its dependency on
-- the `content` attribute in the `meta` tag is too complex to reconcile here,
-- and is not a concern on other tags that use this attribute. For safe
-- construction of the `name` and `content` attributes together on a `meta`
-- tag, use 'Brigid.HTML.Elements.Safe.Meta'.
--
name :: ValidAttribute 'Name tag => T.Text -> Attribute tag
name = Attr_Name

referrerpolicy :: ValidAttribute 'ReferrerPolicy tag
               => Types.ReferrerPolicy -> Attribute tag
referrerpolicy = Attr_ReferrerPolicy

rel :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf rel Types.RelationshipTypes
       , ValidRelationship rel tag
       , ValidAttribute 'Rel tag
       )
    => rel -> Attribute tag
rel =
  Attr_Rel . Types.mkRelationship
