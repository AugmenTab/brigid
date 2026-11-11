module Brigid.HTML.Types.Class
  ( Class (Class)
  , classToBytes
  , classToBytesBuilder
  , classToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable1 (fold1)
import Data.List.NonEmpty qualified as NEL
import Data.String (IsString (fromString))
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype Class =
  Class
    { classToText :: T.Text
    } deriving (Eq, Ord)

instance IsString Class where
  fromString = Class . T.pack

instance Semigroup Class where
  Class c1 <> Class c2 = Class $ T.unwords [ c1, c2 ]

instance Monoid Class where
  mempty  = Class T.empty
  mconcat = maybe mempty fold1 . NEL.nonEmpty

instance Show Class where
  show = mappend "Class " . T.unpack . classToText

classToBytes :: Class -> LBS.ByteString
classToBytes = Render.textToLazyBytes . classToText

classToBytesBuilder :: Class -> Builder
classToBytesBuilder = Render.textToBytesBuilder . classToText
