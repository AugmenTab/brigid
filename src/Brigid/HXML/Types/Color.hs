{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HXML.Types.Color
  ( Color
  , ColorTypes
  , mkColor
  , colorToBytes
  , colorToText
  , ColorName (ColorName)
  , colorNameToBytes
  , colorNameToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Function (on)
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.Internal.Render qualified as Render
import Brigid.Types.HexColor (HexColor, hexColorToBytes, hexColorToText)

newtype Color = Color (Shrubbery.Union ColorTypes)

instance Eq Color where
  (==) = (==) `on` colorToText

instance Show Color where
  show (Color color) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @HexColor  show
        . Shrubbery.branch @ColorName show
        $ Shrubbery.branchEnd
    ) color

type ColorTypes =
  [ HexColor
  , ColorName
  ]

mkColor :: ( KnownNat branchIndex
           , branchIndex ~ FirstIndexOf color ColorTypes
           )
        => color -> Color
mkColor =
  Color . Shrubbery.unify

colorToBytes :: Color -> LBS.ByteString
colorToBytes (Color color) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @HexColor  hexColorToBytes
      . Shrubbery.branch @ColorName colorNameToBytes
      $ Shrubbery.branchEnd
  ) color

colorToText :: Color -> T.Text
colorToText (Color color) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @HexColor  hexColorToText
      . Shrubbery.branch @ColorName colorNameToText
      $ Shrubbery.branchEnd
  ) color

newtype ColorName =
  ColorName
    { colorNameToText :: T.Text
    } deriving (Eq)

instance Show ColorName where
  show = mappend "ColorName " . T.unpack . colorNameToText

colorNameToBytes :: ColorName -> LBS.ByteString
colorNameToBytes = Render.textToLazyBytes . colorNameToText
