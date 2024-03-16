module HTML.Types.ClassSelector
  ( ClassSelector
  , toClassSelector
  , classSelectorToBytes
  , classSelectorToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T

import HTML.Types.Class (Class, classToBytes, classToText)
import HTML.Types.Negatable (Negatable(not))

data ClassSelector =
  ClassSelector
    { classSelectorClass :: Class
    , classSelectorExcluded :: Bool
    }

instance Negatable ClassSelector where
  not cs = cs { classSelectorExcluded = False }

toClassSelector :: Class -> ClassSelector
toClassSelector class_ =
  ClassSelector
    { classSelectorClass = class_
    , classSelectorExcluded = True
    }

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
