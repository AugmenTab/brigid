{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Attributes.Scoped
  ( async
  , autoplay
  , charset
  , cite
  , cols
  , colspan
  , content
  , controls
  , controlslist
  , crossorigin
  , defer
  , disable
  , disabled
  , headers
  , height
  , href
  , ismap
  , maxlength
  , minlength
  , name
  , nomodule
  , ping
  , referrerpolicy
  , rel
  , rows
  , rowspan
  , src
  , width
  ) where

import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Attributes.AttributeType (AttributeType (..))
import Brigid.HTML.Attributes.Elements (ValidAttribute)
import Brigid.HTML.Attributes.Href (ValidHref)
import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Attributes.Relationship (ValidRelationship)
import Brigid.HTML.Attributes.Source (ValidSource)
import Brigid.HTML.Types qualified as Types

-- Scoped Attributes
--
async :: ValidAttribute 'Async tag => Attribute tag
async = Attr_Async

autoplay :: ValidAttribute 'Autoplay tag => Attribute tag
autoplay = Attr_Autoplay

-- | Limited to UTF-8, since that is the only valid option for HTML5.
--
charset :: ValidAttribute 'Charset tag => Attribute tag
charset = Attr_Charset

cite :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf cite Types.URLTypes
        , ValidAttribute 'Cite tag
        )
     => cite -> Attribute tag
cite =
  Attr_Cite . Types.mkURL

cols :: ValidAttribute 'Cols tag => Word -> Attribute tag
cols = Attr_Cols

colspan :: ValidAttribute 'Colspan tag => Word -> Attribute tag
colspan = Attr_Colspan

-- | The `content` attribute is left as simple 'T.Text' because its value is
-- dependent on the `name` or `http-equiv` attributes, and managing the
-- inter-dependency across multiple attributes in the `meta` tag is too complex
-- to reconcile here, For safe construction of the `content` attributes
-- together with its dependencies on a `meta` tag, use
-- 'Brigid.HTML.Elements.Safe.Meta'.
--
content :: ValidAttribute 'Content tag => T.Text -> Attribute tag
content = Attr_Content

controls :: ValidAttribute 'Controls tag => Attribute tag
controls = Attr_Controls

controlslist :: ValidAttribute 'ControlsList tag
             => Types.ControlsList -> Attribute tag
controlslist = Attr_ControlsList

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

headers :: ValidAttribute 'Headers tag
        => NEL.NonEmpty Types.Id -> Attribute tag
headers = Attr_Headers

height :: ValidAttribute 'Height tag => Word -> Attribute tag
height = Attr_Height

href :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf href (Types.HrefTypes Types.Get)
        , ValidHref href tag
        , ValidAttribute 'Href tag
        )
     => href -> Attribute tag
href =
  Attr_Href . Types.mkHref

-- | The `ismap` attribute is only valid on <img> tags that are nested within
-- an <a> tag. For safe construction of the `ismap` attribute, use
-- 'Brigid.HTML.Elements.Safe.Image'.
--
ismap :: ValidAttribute 'IsMap tag => Attribute tag
ismap = Attr_IsMap

-- TODO: For all `Safe` module versions of elements for which this attribute
-- applies, a comparison of min/maxlength should be done to ensure that the
-- bounds are applied appropriately.
--
maxlength :: ValidAttribute 'MaxLength tag => Word -> Attribute tag
maxlength = Attr_MaxLength

-- TODO: For all `Safe` module versions of elements for which this attribute
-- applies, a comparison of min/maxlength should be done to ensure that the
-- bounds are applied appropriately.
--
minlength :: ValidAttribute 'MinLength tag => Word -> Attribute tag
minlength = Attr_MinLength

-- | The `name` attribute is left as simple 'T.Text' because its dependency on
-- the `content` attribute in the `meta` tag is too complex to reconcile here,
-- and is not a concern on other tags that use this attribute. For safe
-- construction of the `name` and `content` attributes together on a `meta`
-- tag, use 'Brigid.HTML.Elements.Safe.Meta'.
--
name :: ValidAttribute 'Name tag => T.Text -> Attribute tag
name = Attr_Name

nomodule :: ValidAttribute 'NoModule tag => Bool -> Attribute tag
nomodule = Attr_NoModule

ping :: ValidAttribute 'Ping tag => NEL.NonEmpty Types.Ping -> Attribute tag
ping = Attr_Ping

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

rows :: ValidAttribute 'Rows tag => Word -> Attribute tag
rows = Attr_Rows

rowspan :: ValidAttribute 'Rowspan tag => Word -> Attribute tag
rowspan = Attr_Rowspan

src :: ( KnownNat branchIndex
       , branchIndex ~ FirstIndexOf url Types.URLTypes
       , ValidSource url tag
       , ValidAttribute 'Src tag
       )
    => url -> Attribute tag
src =
  Attr_Src . Types.mkURL

width :: ValidAttribute 'Width tag => Word -> Attribute tag
width = Attr_Width
