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
    } deriving (Eq)

instance Show Class where
  show = mappend "Class " . T.unpack . classToText

classToBytes :: Class -> LBS.ByteString
classToBytes = Render.textToLazyBytes . classToText
