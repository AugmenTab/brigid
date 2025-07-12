{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brigid.HTML.Types.Class
  ( Class (Class)
  , classToBytes
  , classToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype Class =
  Class
    { classToText :: T.Text
    } deriving (Eq, Monoid)

instance Semigroup Class where
  Class c1 <> Class c2 = Class $ T.unwords [ c1, c2 ]

instance Show Class where
  show = mappend "Class " . T.unpack . classToText

classToBytes :: Class -> LBS.ByteString
classToBytes = Render.textToLazyBytes . classToText
