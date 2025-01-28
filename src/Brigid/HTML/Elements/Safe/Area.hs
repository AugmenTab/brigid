{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Elements.Safe.Area
  ( area
  , Area (..)
  , Coord
  , Shape
      ( Default
      , Rect
      , Circle
      , Poly
      )
  , HrefAttributes
  , mkHrefAttributes
  ) where

import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Integer (Positive)
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Attributes.Href (ValidHref)
import Brigid.HTML.Attributes.Internal (Attribute)
import Brigid.HTML.Attributes.Relationship (ValidRelationship)
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types

area :: ValidChild Tags.Area parent grandparent
     => Area -> E.ChildHTML parent grandparent
area areaData =
  E.area $
    concat
      [ maybe [] shapeAttrs $ areaShape areaData
      , maybe [] hrefAttributes $ areaHrefAttributes areaData
      , areaOtherAttributes areaData
      ]

data Area =
  Area
    { areaShape           :: Maybe Shape
    , areaHrefAttributes  :: Maybe HrefAttributes
    , areaOtherAttributes :: [Attribute Tags.Area]
    }

type Coord = (Integer, Integer)

data Shape
  = Default
  | Rect Coord Coord
  | Circle Coord Positive
  | Poly (NonEmpty Coord)

shapeAttrs :: Shape -> [Attribute Tags.Area]
shapeAttrs shape =
  case shape of
    Default ->
      [ A.shape Types.Default ]

    Rect (x1, y1) (x2, y2) ->
      [ A.shape Types.Rect, A.coords $ x1 :| [ y1, x2, y2 ] ]

    Circle (x, y) rad ->
      [ A.shape Types.Circle, A.coords $ x :| [ y, fromIntegral rad ] ]

    Poly ((x, y) :| coords) ->
      [ A.shape Types.Poly
      , A.coords $ x :| y : foldl' (\acc (a, b) -> a : b : acc) [] coords
      ]

data HrefAttributes =
  HrefAttributes
    { hrefAttributesHref   :: Attribute Tags.Area
    , hrefAttributesAlt    :: Maybe (Attribute Tags.Area)
    , hrefAttributesRel    :: Maybe (Attribute Tags.Area)
    , hrefAttributesTarget :: Maybe (Attribute Tags.Area)
    }

mkHrefAttributes :: ( KnownNat hrefBranchIndex
                    , KnownNat relBranchIndex
                    , hrefBranchIndex ~ FirstIndexOf href (Types.HrefTypes Types.Get)
                    , relBranchIndex ~ FirstIndexOf rel Types.RelationshipTypes
                    , ValidHref href Tags.Area
                    , ValidRelationship rel Tags.Area
                    )
                 => href
                 -> Maybe T.Text
                 -> Maybe rel
                 -> Maybe Types.Target
                 -> HrefAttributes
mkHrefAttributes href mbAlt mbRel mbTarget =
  HrefAttributes
    { hrefAttributesHref   = A.href href
    , hrefAttributesAlt    = A.alt <$> mbAlt
    , hrefAttributesRel    = A.rel <$> mbRel
    , hrefAttributesTarget = A.target <$> mbTarget
    }

hrefAttributes :: HrefAttributes -> [Attribute Tags.Area]
hrefAttributes attrs =
  catMaybes
    [ Just $ hrefAttributesHref attrs
    , hrefAttributesAlt    attrs
    , hrefAttributesRel    attrs
    , hrefAttributesTarget attrs
    ]
