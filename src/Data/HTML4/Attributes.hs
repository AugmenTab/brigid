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

import Data.HTML4.Attributes.AttributeType qualified as AttrType
import Data.HTML4.Attributes.Internal qualified as A

id :: T.Text -> A.Attribute tag
id = A.Id

class_ :: T.Text -> A.Attribute tag
class_ = A.Class

classes :: [T.Text] -> A.Attribute tag
classes = A.Class . T.unwords

disabled :: A.ValidAttribute 'AttrType.Disabled tag => A.Attribute tag
disabled = disable True

disable :: A.ValidAttribute 'AttrType.Disabled tag => Bool -> A.Attribute tag
disable = A.Disabled
