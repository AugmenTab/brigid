module HTML.Types.Id
  ( Id (Id)
  , idToText
  ) where

import Data.Text qualified as T

newtype Id = Id T.Text

idToText :: Id -> T.Text
idToText (Id _id) = _id
