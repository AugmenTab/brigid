{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HTML.Types.Aria.Invalid
  ( AriaInvalid (..)
  , AriaInvalidTypes
  , ariaInvalidToBytes
  , ariaInvalidToText
  , InvalidGrammar (InvalidGrammar)
  , InvalidSpelling (InvalidSpelling)
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Shrubbery qualified

import Brigid.Internal.Render qualified as Render

newtype AriaInvalid = AriaInvalid (Shrubbery.Union AriaInvalidTypes)
  deriving (Eq, Show)

type AriaInvalidTypes =
  [ InvalidGrammar
  , InvalidSpelling
  , Bool
  ]

ariaInvalidToBytes :: AriaInvalid -> LBS.ByteString
ariaInvalidToBytes (AriaInvalid hp) =
  ( Shrubbery.dissectUnion
      . Shrubbery.branchBuild
      . Shrubbery.branch @InvalidGrammar invalidGrammarToBytes
      . Shrubbery.branch @InvalidSpelling invalidSpellingToBytes
      . Shrubbery.branch @Bool Render.enumBoolToBytes
      $ Shrubbery.branchEnd
  ) hp

ariaInvalidToText :: AriaInvalid -> T.Text
ariaInvalidToText (AriaInvalid hp) =
  ( Shrubbery.dissectUnion
      . Shrubbery.branchBuild
      . Shrubbery.branch @InvalidGrammar invalidGrammarToText
      . Shrubbery.branch @InvalidSpelling invalidSpellingToText
      . Shrubbery.branch @Bool Render.enumBoolToText
      $ Shrubbery.branchEnd
  ) hp

data InvalidGrammar = InvalidGrammar
  deriving (Eq, Show)

invalidGrammarToBytes :: InvalidGrammar -> LBS.ByteString
invalidGrammarToBytes InvalidGrammar = LBS8.pack "grammar"

invalidGrammarToText :: InvalidGrammar -> T.Text
invalidGrammarToText InvalidGrammar = T.pack "grammar"

data InvalidSpelling = InvalidSpelling
  deriving (Eq, Show)

invalidSpellingToBytes :: InvalidSpelling -> LBS.ByteString
invalidSpellingToBytes InvalidSpelling = LBS8.pack "spelling"

invalidSpellingToText :: InvalidSpelling -> T.Text
invalidSpellingToText InvalidSpelling = T.pack "spelling"
