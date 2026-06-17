module Brigid.HTML.Types.ClassSelector
  ( ClassSelector
  , toClassSelector
  , not
  , classSelectorToBytes
  , classSelectorToBytesBuilder
  , classSelectorToText
  ) where

import Prelude hiding (not)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T

import Brigid.HTML.Types.Class (Class, classToBytes, classToBytesBuilder, classToText)

data ClassSelector =
  ClassSelector
    { classSelectorExcluded :: Bool
    , classSelectorClass    :: Class
    } deriving (Eq, Show)

toClassSelector :: Class -> ClassSelector
toClassSelector = ClassSelector True

not :: ClassSelector -> ClassSelector
not cs = cs { classSelectorExcluded = False }

classSelectorToBytes :: ClassSelector -> LBS.ByteString
classSelectorToBytes selector =
  if classSelectorExcluded selector
     then ":not(." <> classToBytes (classSelectorClass selector) <> ")"
     else LBS8.cons '.' . classToBytes $ classSelectorClass selector

classSelectorToBytesBuilder :: ClassSelector -> Builder
{-# INLINE classSelectorToBytesBuilder #-}
classSelectorToBytesBuilder selector =
  if classSelectorExcluded selector
     then ":not(." <> classToBytesBuilder (classSelectorClass selector) <> ")"
     else "." <> classToBytesBuilder (classSelectorClass selector)

classSelectorToText :: ClassSelector -> T.Text
classSelectorToText selector =
  if classSelectorExcluded selector
     then T.cons '.' . classToText $ classSelectorClass selector
     else ":not(." <> classToText (classSelectorClass selector) <> ")"
