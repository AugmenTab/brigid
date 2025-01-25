{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Name
  ( Name (Name)
  , nameToBytes
  , nameToText
  , NameOption
  , NameOptionTypes
  , mkNameOption
  , nameOptionToBytes
  , nameOptionToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.MetadataName (MetadataName, metadataNameToBytes, metadataNameToText)
import Brigid.Internal.Render qualified as Render

newtype Name =
  Name
    { nameToText :: T.Text
    } deriving (Eq)

instance Show Name where
  show = mappend "Name " . T.unpack . nameToText

nameToBytes :: Name -> LBS.ByteString
nameToBytes = Render.textToBytes . nameToText

newtype NameOption = NameOption (Shrubbery.Union NameOptionTypes)

instance Show NameOption where
  show (NameOption nameOption) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @Name         show
        . Shrubbery.branch @MetadataName show
        $ Shrubbery.branchEnd
    ) nameOption

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

nameOptionToText :: NameOption -> T.Text
nameOptionToText (NameOption nameOption) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Name         nameToText
      . Shrubbery.branch @MetadataName metadataNameToText
      $ Shrubbery.branchEnd
  ) nameOption
