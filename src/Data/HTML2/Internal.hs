{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, TypeInType, ScopedTypeVariables #-}

module Data.HTML2.Internal
  ( -- IsMember
  , -- width
  ) where

  {-
import Data.Kind (Type)
import Data.Text qualified as T
import GHC.TypeLits (TypeError, ErrorMessage(Text, ShowType, (:<>:)))

-- Define newtypes
newtype Id = Id T.Text
newtype Class = Class T.Text
newtype Width = Width Int

data GlobalList
type GlobalVariables = '[Id, Class]

data OtherList
type OtherVariables  = '[Width]

-- Closed type family for membership checking
type family IsMember (t :: Type) (ts :: [Type]) (listName :: Type) :: Bool where
    IsMember t '[]       listName  = TypeError (AttributeError t listName)
    IsMember t (t ': ts) _listName = 'True
    IsMember t (x ': ts) listName  = IsMember t ts listName

type family AttributeError (t :: Type) (listName :: Type) :: ErrorMessage where
  AttributeError t GlobalList =
    'Text "Type "
      ':<>: 'ShowType t
      ':<>: 'Text " not found in "
      ':<>: 'Text "GlobalVariables."

  AttributeError t OtherList =
    'Text "Type "
      ':<>: 'ShowType t
      ':<>: 'Text " not found in "
      ':<>: 'Text "OtherVariables."

-- Examples of usage
type IsGlobal tag = IsMember tag GlobalVariables GlobalList ~ 'True

width :: IsGlobal Width => Int -> Width
width = Width
-}
