{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.For
  ( ForOption
  , ForOptionTypes
  , mkForOption
  , forOptionToBytes
  , forOptionToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Containers.ListUtils (nubOrdOn)
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.Internal.Render (foldToBytesWithSeparator, foldToTextWithSeparator)
import Brigid.Types.Id (Id, idToBytes, idToText)

newtype ForOption = ForOption (Shrubbery.Union ForOptionTypes)

instance Show ForOption where
  show (ForOption for) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @Id                show
        . Shrubbery.branch @(NEL.NonEmpty Id) show
        $ Shrubbery.branchEnd
    ) for

type ForOptionTypes =
  [ Id
  , NEL.NonEmpty Id
  ]

mkForOption :: ( KnownNat branchIndex
               , branchIndex ~ FirstIndexOf for ForOptionTypes
               )
            => for -> ForOption
mkForOption =
  ForOption . Shrubbery.unify

forOptionToBytes :: ForOption -> LBS.ByteString
forOptionToBytes (ForOption for) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Id                idToBytes
      . Shrubbery.branch @(NEL.NonEmpty Id) renderIdsBytes
      $ Shrubbery.branchEnd
  ) for

forOptionToText :: ForOption -> T.Text
forOptionToText (ForOption for) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Id                idToText
      . Shrubbery.branch @(NEL.NonEmpty Id) renderIdsText
      $ Shrubbery.branchEnd
  ) for

-- Helpers
--
renderIdsBytes :: NEL.NonEmpty Id -> LBS.ByteString
renderIdsBytes =
  foldToBytesWithSeparator idToBytes " " . nubOrdOn idToBytes . NEL.toList

renderIdsText :: NEL.NonEmpty Id -> T.Text
renderIdsText =
  foldToTextWithSeparator idToText " " . nubOrdOn idToText . NEL.toList
