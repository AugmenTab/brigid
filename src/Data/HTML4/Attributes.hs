{-# LANGUAGE DataKinds #-}

module Data.HTML4.Attributes
  ( id
  , class_
  , classes
  , disabled
  , disable
  ) where

import Prelude hiding (id)
import Data.Text qualified as T

import Data.HTML4.Internal qualified as H

id :: T.Text -> H.Attribute tag
id = H.Id

class_ :: T.Text -> H.Attribute tag
class_ = H.Class

classes :: [T.Text] -> H.Attribute tag
classes = H.Class . T.unwords

disabled :: H.Contains (H.ValidElementsFor 'H.DisabledType) tag
         => H.Attribute tag
disabled = disable True

disable :: H.Contains (H.ValidElementsFor 'H.DisabledType) tag
        => Bool -> H.Attribute tag
disable = H.Disabled
