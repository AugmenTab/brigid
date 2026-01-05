{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HTML.Types.Aria.MixedBool
  ( AriaMixedBool (..)
  , AriaMixedBoolTypes
  , ariaMixedBoolToBytes
  , ariaMixedBoolToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Shrubbery qualified

import Brigid.HTML.Types.Mixed (Mixed, mixedToBytes, mixedToText)
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

ariaMixedBoolToText :: AriaMixedBool -> T.Text
ariaMixedBoolToText (AriaMixedBool mb) =
  ( Shrubbery.dissectUnion
      . Shrubbery.branchBuild
      . Shrubbery.branch @Mixed mixedToText
      . Shrubbery.branch @Bool Render.enumBoolToText
      $ Shrubbery.branchEnd
  ) mb
