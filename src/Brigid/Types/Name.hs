{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.Types.Name
  ( Name (Name)
  , nameToBytes
  , nameToBytesBuilder
  , nameToNonEmptyText
  , nameToText
  , nameToTextBuilder
  , NameOption
  , NameOptionTypes
  , mkNameOption
  , nameOptionToBytes
  , nameOptionToBytesBuilder
  , nameOptionToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.NonEmptyText qualified as NET
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.MetadataName (MetadataName, metadataNameToBytes, metadataNameToBytesBuilder, metadataNameToText)
import Brigid.Internal.Render qualified as Render

newtype Name =
  Name
    { nameToNonEmptyText :: NET.NonEmptyText
    } deriving (Eq, Ord, Show)

nameToBytes :: Name -> LBS.ByteString
nameToBytes = Render.textToLazyBytes . nameToText

nameToBytesBuilder :: Name -> Builder
nameToBytesBuilder = TE.encodeUtf8Builder . nameToText

nameToText :: Name -> T.Text
nameToText =
  NET.toText . nameToNonEmptyText

nameToTextBuilder :: Name -> TBL.Builder
nameToTextBuilder = TBL.fromText . nameToText

newtype NameOption =
  NameOption (Shrubbery.Union NameOptionTypes)
    deriving (Eq, Show)

type NameOptionTypes =
  [ Name
  , MetadataName
  ]

mkNameOption :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf name NameOptionTypes
                )
             => name -> NameOption
mkNameOption =
  NameOption . Shrubbery.unify

nameOptionToBytes :: NameOption -> LBS.ByteString
nameOptionToBytes (NameOption nameOption) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Name         nameToBytes
      . Shrubbery.branch @MetadataName metadataNameToBytes
      $ Shrubbery.branchEnd
  ) nameOption

nameOptionToBytesBuilder :: NameOption -> Builder
{-# INLINABLE nameOptionToBytesBuilder #-}
nameOptionToBytesBuilder (NameOption nameOption) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Name         nameToBytesBuilder
      . Shrubbery.branch @MetadataName metadataNameToBytesBuilder
      $ Shrubbery.branchEnd
  ) nameOption

nameOptionToText :: NameOption -> T.Text
nameOptionToText (NameOption nameOption) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Name         nameToText
      . Shrubbery.branch @MetadataName metadataNameToText
      $ Shrubbery.branchEnd
  ) nameOption
