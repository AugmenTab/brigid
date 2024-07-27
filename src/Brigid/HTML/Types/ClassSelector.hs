module Brigid.HTML.Types.ClassSelector
  ( ClassSelector
  , toClassSelector
  , not
  , classSelectorToBytes
  , classSelectorToText
  ) where

import Prelude hiding (not)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T

import Brigid.HTML.Types.Class (Class, classToBytes, classToText)

data ClassSelector =
  ClassSelector
    { classSelectorExcluded :: Bool
    , classSelectorClass    :: Class
    }

toClassSelector :: Class -> ClassSelector
toClassSelector = ClassSelector True

not :: ClassSelector -> ClassSelector
not cs = cs { classSelectorExcluded = False }

classSelectorToBytes :: ClassSelector -> LBS.ByteString
classSelectorToBytes selector =
  if classSelectorExcluded selector
     then ":not(." <> classToBytes (classSelectorClass selector) <> ")"
     else LBS8.cons '.' . classToBytes $ classSelectorClass selector

classSelectorToText :: ClassSelector -> T.Text
classSelectorToText selector =
  if classSelectorExcluded selector
     then T.cons '.' . classToText $ classSelectorClass selector
     else ":not(." <> classToText (classSelectorClass selector) <> ")"
