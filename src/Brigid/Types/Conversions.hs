module Brigid.Types.Conversions
  ( idToName
  , nameToId
  ) where

import Data.Coerce (coerce)

import Brigid.Types.Id (Id (Id))
import Brigid.Types.Name (Name (Name))

idToName :: Id -> Name
idToName = coerce

nameToId :: Name -> Id
nameToId = coerce
