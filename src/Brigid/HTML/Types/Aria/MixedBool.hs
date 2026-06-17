{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HTML.Types.Aria.MixedBool
  ( AriaMixedBool (..)
  , AriaMixedBoolTypes
  , ariaMixedBoolToBytes
  , ariaMixedBoolToBytesBuilder
  , ariaMixedBoolToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Shrubbery qualified

import Brigid.HTML.Types.Mixed (Mixed, mixedToBytes, mixedToBytesBuilder, mixedToText)
import Brigid.Internal.Render qualified as Render

newtype AriaMixedBool = AriaMixedBool (Shrubbery.Union AriaMixedBoolTypes)
  deriving (Eq, Show)

type AriaMixedBoolTypes =
  [ Mixed
  , Bool
  ]

ariaMixedBoolToBytes :: AriaMixedBool -> LBS.ByteString
ariaMixedBoolToBytes (AriaMixedBool mb) =
  ( Shrubbery.dissectUnion
      . Shrubbery.branchBuild
      . Shrubbery.branch @Mixed mixedToBytes
      . Shrubbery.branch @Bool Render.enumBoolToBytes
      $ Shrubbery.branchEnd
  ) mb

ariaMixedBoolToBytesBuilder :: AriaMixedBool -> Builder
ariaMixedBoolToBytesBuilder (AriaMixedBool mb) =
  ( Shrubbery.dissectUnion
      . Shrubbery.branchBuild
      . Shrubbery.branch @Mixed mixedToBytesBuilder
      . Shrubbery.branch @Bool  Render.enumBoolToBytesBuilder
      $ Shrubbery.branchEnd
  ) mb

ariaMixedBoolToText :: AriaMixedBool -> T.Text
ariaMixedBoolToText (AriaMixedBool mb) =
  ( Shrubbery.dissectUnion
      . Shrubbery.branchBuild
      . Shrubbery.branch @Mixed mixedToText
      . Shrubbery.branch @Bool Render.enumBoolToText
      $ Shrubbery.branchEnd
  ) mb
