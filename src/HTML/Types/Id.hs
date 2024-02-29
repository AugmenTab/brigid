module HTML.Types.Id
  ( Id
  , idFromText
  , idToText
  ) where

import Data.Text qualified as T

newtype Id = Id T.Text

idFromText :: T.Text -> Id
idFromText = Id

idToText :: Id -> T.Text
idToText (Id _id) = _id
