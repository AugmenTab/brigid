{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.TypeOption
  ( TypeOption
  , TypeOptionTypes
  , mkTypeOption
  , typeOptionToBytes
  , typeOptionToText
  , RawTypeOption
  , mkRawTypeOption
  , rawTypeOptionToBytes
  , rawTypeOptionToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.InputType (InputType, inputTypeToBytes, inputTypeToText)
import Brigid.HTML.Types.NumberingType (NumberingType, numberingTypeToBytes, numberingTypeToText)

newtype TypeOption = TypeOption (Shrubbery.Union TypeOptionTypes)

instance Show TypeOption where
  show (TypeOption typeOption) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @InputType     show
        . Shrubbery.branch @NumberingType show
        . Shrubbery.branch @RawTypeOption show
        $ Shrubbery.branchEnd
    ) typeOption

type TypeOptionTypes =
  [ InputType
  , NumberingType
  , RawTypeOption
  ]

mkTypeOption :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf type_ TypeOptionTypes
                )
             => type_ -> TypeOption
mkTypeOption =
  TypeOption . Shrubbery.unify

typeOptionToBytes :: TypeOption -> LBS.ByteString
typeOptionToBytes (TypeOption typeOption) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InputType     inputTypeToBytes
      . Shrubbery.branch @NumberingType numberingTypeToBytes
      . Shrubbery.branch @RawTypeOption rawTypeOptionToBytes
      $ Shrubbery.branchEnd
  ) typeOption

typeOptionToText :: TypeOption -> T.Text
typeOptionToText (TypeOption typeOption) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InputType     inputTypeToText
      . Shrubbery.branch @NumberingType numberingTypeToText
      . Shrubbery.branch @RawTypeOption rawTypeOptionToText
      $ Shrubbery.branchEnd
  ) typeOption

newtype RawTypeOption =
  RawTypeOption
    { rawTypeOptionToText :: T.Text
    } deriving (Eq)

instance Show RawTypeOption where
  show = mappend "RawTypeOption " . show

mkRawTypeOption :: T.Text -> RawTypeOption
mkRawTypeOption = RawTypeOption

rawTypeOptionToBytes :: RawTypeOption -> LBS.ByteString
rawTypeOptionToBytes =
  LBS8.pack . T.unpack . rawTypeOptionToText
