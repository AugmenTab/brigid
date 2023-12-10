{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.HTML1.Elements.Tags
  ( HTML
  , Html
  , Comment, comment
  , Text, text, texts
  , Anchor, a
  , Bold, b
  , Division, div
  , H1, h1
  , Image, img
  , Span, span

  , Id, id
  ) where

import Prelude hiding (div, id, span)
import Data.Kind (Type)
import Data.List qualified as L
import Data.Text qualified as T
import Text.Blaze qualified as Blaze
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

import Data.HTML.Elements.Tags qualified as Tags
import Data.HTML.Types (Elem)

type HTML parent = ValidChildOf parent

type family ValidElementsFor attribute :: [Type] where
  ValidElementsFor Id = '[Division]

class Attribute attr where
  toAttribute :: attr -> H.Attribute

type IsValidAttribute element attr =
  Elem element (ValidElementsFor attr)

data ValidAttributeOf element where
  WrapAttribute :: (Attribute attr, IsValidAttribute element attr)
                => attr -> ValidAttributeOf element

unwrapAttribute :: ValidAttributeOf element -> H.Attribute
unwrapAttribute (WrapAttribute attr) = toAttribute attr

newtype Id = Id T.Text

instance Attribute Id where
  toAttribute (Id idText) = A.id $ Blaze.textValue idText

{-| The id global attribute defines an identifier (ID) which must be unique in
   the whole document. Its purpose is to identify the element when linking
   (using a fragment identifier), scripting, or styling (with CSS).
-}
id :: IsValidAttribute element Id => T.Text -> ValidAttributeOf element
id = WrapAttribute . Id -- TODO: Escape the provided id string.

-- class_ :: IsValidAttribute element Class => T.Text -> ValidAttributeOf element
-- class_ = WrapAttribute . Class

-- classes :: IsValidAttribute element Class
--         => [T.Text] -> ValidAttributeOf element
-- classes = class_ . T.unwords

class Renderable tag where
  toHTML :: tag -> H.Html

type family ValidChildrenFor element :: [Type] where
  ValidChildrenFor Comment  = NonElement
  ValidChildrenFor Text     = NonElement

  ValidChildrenFor Anchor   = '[]
  ValidChildrenFor Bold     = PhrasingContent
  ValidChildrenFor Division = FlowContent
  ValidChildrenFor H1       = PhrasingContent
  ValidChildrenFor Image    = VoidElement
  ValidChildrenFor Span     = PhrasingContent

  ValidChildrenFor Html     = FlowContent

data Html = Html

type NonElement  = '[]
type VoidElement = '[]

-- Below are truncated version of these tag groups.
type FlowContent =
  '[ Tags.Text
   , Tags.Anchor
   , Tags.Bold
   , Tags.Division
   , Tags.H1
   , Tags.Image
   , Tags.Span
   ]

type PhrasingContent =
  '[ Tags.Text
   , Tags.Bold
   , Tags.Image
   , Tags.Span
   ]

type IsValidChild child parent =
  Elem child (ValidChildrenFor parent)

data ValidChildOf parent where
  WrapElement :: (Renderable child, IsValidChild child parent)
              => child -> ValidChildOf parent

unwrapElement :: ValidChildOf parent -> H.Html
unwrapElement (WrapElement child) = toHTML child

newtype Comment = Comment T.Text

instance Renderable Comment where
  toHTML (Comment txt) = H.toHtml $ Blaze.textComment txt

comment :: IsValidChild Comment parent => T.Text -> ValidChildOf parent
comment = WrapElement . Comment

newtype Text = Text T.Text

instance Renderable Text where
  toHTML (Text txt) = H.toHtml $ Blaze.text txt

text :: IsValidChild Text parent => T.Text -> ValidChildOf parent
text = WrapElement . Text

texts :: IsValidChild Text parent => [T.Text] -> ValidChildOf parent
texts = text . T.unwords

data Anchor = Anchor [ValidAttributeOf Anchor] [ValidChildOf Anchor]

instance Renderable Anchor where
  toHTML (Anchor attributes children) =
    H.a ! foldMap unwrapAttribute attributes $ foldMap unwrapElement children

a :: IsValidChild Anchor parent
  => [ValidAttributeOf Anchor] -> [ValidChildOf Anchor] -> ValidChildOf parent
a attributes children =
  WrapElement $ Anchor attributes children

data Bold = Bold [ValidAttributeOf Bold] [ValidChildOf Bold]

instance Renderable Bold where
  toHTML (Bold attributes children) =
    H.b ! foldMap unwrapAttribute attributes $ foldMap unwrapElement children

b :: IsValidChild Bold parent
  => [ValidAttributeOf Bold]
  -> [ValidChildOf Bold]
  -> ValidChildOf parent
b attributes children =
  WrapElement $ Bold attributes children

data Division = Division [ValidAttributeOf Division] [ValidChildOf Division]

instance Renderable Division where
  toHTML (Division attributes children) =
    H.div ! foldMap unwrapAttribute attributes $ foldMap unwrapElement children

div :: IsValidChild Division parent
    => [ValidAttributeOf Division]
    -> [ValidChildOf Division]
    -> ValidChildOf parent
div attributes children =
  WrapElement $ Division attributes children

newtype Image = Image [ValidAttributeOf Image]

instance Renderable Image where
  toHTML (Image attributes) =
    H.img ! foldMap unwrapAttribute attributes

img :: IsValidChild Image parent
    => [ValidAttributeOf Image]
    -> ValidChildOf parent
img = WrapElement . Image

data Span = Span [ValidAttributeOf Span] [ValidChildOf Span]

instance Renderable Span where
  toHTML (Span attributes children) =
    H.span ! foldMap unwrapAttribute attributes $ foldMap unwrapElement children

span :: IsValidChild Span parent
     => [ValidAttributeOf Span]
     -> [ValidChildOf Span]
     -> ValidChildOf parent
span attributes children =
  WrapElement $ Span attributes children

data H1 = H1 [ValidAttributeOf H1] [ValidChildOf H1]

instance Renderable H1 where
  toHTML (H1 attributes children) =
    H.span ! foldMap unwrapAttribute attributes $ foldMap unwrapElement children

h1 :: IsValidChild H1 parent
   => [ValidAttributeOf H1]
   -> [ValidChildOf H1]
   -> ValidChildOf parent
h1 attributes children =
  WrapElement $ H1 attributes children
