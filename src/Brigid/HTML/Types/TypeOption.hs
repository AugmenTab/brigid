{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.TypeOption
  ( TypeOption
  , TypeOptionTypes
  , mkTypeOption
  , typeOptionToBytes
  , typeOptionToBytesBuilder
  , typeOptionToText
  , typeOptionToTextBuilder
  , RawTypeOption
  , mkRawTypeOption
  , rawTypeOptionToBytes
  , rawTypeOptionToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.InputType (InputType, inputTypeToBytes, inputTypeToBytesBuilder, inputTypeToText)
import Brigid.HTML.Types.NumberingType (NumberingType, numberingTypeToBytes, numberingTypeToBytesBuilder, numberingTypeToText)
import Brigid.HTML.Types.ScriptType (ScriptType, scriptTypeToBytes, scriptTypeToBytesBuilder, scriptTypeToText)

newtype TypeOption =
  TypeOption (Shrubbery.Union TypeOptionTypes)
    deriving (Eq, Show)

type TypeOptionTypes =
  [ InputType
  , NumberingType
  , ScriptType
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
      . Shrubbery.branch @ScriptType    scriptTypeToBytes
      . Shrubbery.branch @RawTypeOption rawTypeOptionToBytes
      $ Shrubbery.branchEnd
  ) typeOption

typeOptionToBytesBuilder :: TypeOption -> Builder
{-# INLINABLE typeOptionToBytesBuilder #-}
typeOptionToBytesBuilder (TypeOption typeOption) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InputType     inputTypeToBytesBuilder
      . Shrubbery.branch @NumberingType numberingTypeToBytesBuilder
      . Shrubbery.branch @ScriptType    scriptTypeToBytesBuilder
      . Shrubbery.branch @RawTypeOption (TE.encodeUtf8Builder . rawTypeOptionToText)
      $ Shrubbery.branchEnd
  ) typeOption

typeOptionToText :: TypeOption -> T.Text
typeOptionToText (TypeOption typeOption) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InputType     inputTypeToText
      . Shrubbery.branch @NumberingType numberingTypeToText
      . Shrubbery.branch @ScriptType    scriptTypeToText
      . Shrubbery.branch @RawTypeOption rawTypeOptionToText
      $ Shrubbery.branchEnd
  ) typeOption

typeOptionToTextBuilder :: TypeOption -> TBL.Builder
typeOptionToTextBuilder = TBL.fromText . typeOptionToText

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
