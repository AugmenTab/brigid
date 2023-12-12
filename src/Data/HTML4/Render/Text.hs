{-# LANGUAGE GADTs #-}

module Data.HTML4.Render.Text
  ( renderHTML
  ) where

import Data.Bool qualified as B
import Data.List qualified as L
import Data.Maybe (mapMaybe)
import Data.Text qualified as T

import Data.HTML4.Elements.Internal (Node(..))
import Data.HTML4.Attributes.Internal (Attribute(..))

renderHTML :: Node cat -> T.Text
renderHTML html =
  case html of
    A      attrs content -> buildTag "a"      attrs $ Right content
    Div    attrs content -> buildTag "div"    attrs $ Right content
    Span   attrs content -> buildTag "span"   attrs $ Right content
    P      attrs content -> buildTag "p"      attrs $ Right content
    H1     attrs content -> buildTag "h1"     attrs $ Right content
    Ul     attrs content -> buildTag "ul"     attrs $ Right content
    Li     attrs content -> buildTag "li"     attrs $ Right content
    Img    attrs         -> buildTag "img"    attrs $ Left OmitTag
    Iframe attrs         -> buildTag "iframe" attrs $ Left WithTag

-- This represents an element that, for one reason or another, does not contain
-- child elements.
--
data NoContent
  -- OmitTag means the tag is self closing, and thus omits a closing tag.
  = OmitTag
  -- WithTag means the tag requires an explicit closing tag despite not being
  -- able to contain child elements.
  | WithTag

buildTag :: T.Text
         -> [Attribute attr]
         -> Either NoContent [Node content]
         -> T.Text
buildTag tag attributes content =
  T.concat
    [ "<"
    , tag
    , B.bool T.empty " " $ L.null attributes
    , T.unwords $ mapMaybe renderAttribute attributes
    , case content of
        Left  OmitTag   -> " />"
        Left  WithTag   -> ">"
        Right _children -> ">"
    , case content of
        Left  _type    -> T.empty
        Right children -> foldMap renderHTML children
    , case content of
        Left  OmitTag   -> T.empty
        Left  WithTag   -> "</" <> tag <> ">"
        Right _children -> "</" <> tag <> ">"
    ]

renderAttribute :: Attribute any -> Maybe T.Text
renderAttribute attr =
  case attr of
    Id       _id        -> Just $ "id=\"" <> _id <> "\""
    Class    _class     -> Just $ "class=\"" <> _class <> "\""
    Width    width      -> Just $ "width=\"" <> T.pack (show width) <> "\""
    Disabled isDisabled -> B.bool Nothing (Just "disabled") isDisabled
