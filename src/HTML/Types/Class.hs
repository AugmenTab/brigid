module HTML.Types.Class
  ( Class (Class)
  , classToText
  ) where

import Data.Text qualified as T

newtype Class = Class T.Text

classToText :: Class -> T.Text
classToText (Class _class) = _class
