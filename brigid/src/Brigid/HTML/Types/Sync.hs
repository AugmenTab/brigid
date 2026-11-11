{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Sync
  ( Sync
  , mkSync
  , syncToBytes
  , syncToBytesBuilder
  , syncToText
  , syncToTextBuilder
  , SyncStrategy
      ( SyncDrop
      , SyncAbort
      , SyncReplace
      , SyncQueue
      )
  , syncStrategyToBytes
  , syncStrategyToBytesBuilder
  , syncStrategyToText
  , SyncQueueStrategy
      ( SyncQueueFirst
      , SyncQueueLast
      , SyncQueueAll
      )
  , syncQueueStrategyToBytes
  , syncQueueStrategyToBytesBuilder
  , syncQueueStrategyToText
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import GHC.TypeLits (KnownNat)
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.QuerySelector
  ( HxTarget
  , HxTargetTypes
  , hxTargetToBytes
  , hxTargetToBytesBuilder
  , hxTargetToText
  , mkHxTarget
  )

-- | The selector half of @hx-sync@ uses the exact same extended selector
-- grammar as @hx-target@ (@this@, @closest ...@, @find ...@, or a plain CSS
-- selector), so this reuses 'HxTarget' rather than duplicating it.
--
data Sync =
  Sync
    { syncSelector :: HxTarget
    , syncStrategy :: Maybe SyncStrategy
    } deriving (Eq, Show)

mkSync :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf selector HxTargetTypes
          )
       => selector -> Maybe SyncStrategy -> Sync
mkSync selector = Sync (mkHxTarget selector)

syncToBytes :: Sync -> LBS.ByteString
syncToBytes sync =
  hxTargetToBytes (syncSelector sync)
    <> maybe mempty ((":" <>) . syncStrategyToBytes) (syncStrategy sync)

syncToBytesBuilder :: Sync -> Builder
syncToBytesBuilder sync =
  hxTargetToBytesBuilder (syncSelector sync)
    <> maybe mempty ((":" <>) . syncStrategyToBytesBuilder) (syncStrategy sync)

syncToText :: Sync -> T.Text
syncToText sync =
  hxTargetToText (syncSelector sync)
    <> maybe mempty ((":" <>) . syncStrategyToText) (syncStrategy sync)

syncToTextBuilder :: Sync -> TBL.Builder
syncToTextBuilder = TBL.fromText . syncToText

data SyncStrategy
  = SyncDrop
  | SyncAbort
  | SyncReplace
  | SyncQueue (Maybe SyncQueueStrategy)
  deriving (Eq, Show)

syncStrategyToBytes :: SyncStrategy -> LBS.ByteString
syncStrategyToBytes strategy =
  case strategy of
    SyncDrop ->
      "drop"

    SyncAbort ->
      "abort"

    SyncReplace ->
      "replace"

    SyncQueue mbQueue ->
      "queue" <> maybe mempty ((" " <>) . syncQueueStrategyToBytes) mbQueue

syncStrategyToBytesBuilder :: SyncStrategy -> Builder
{-# INLINE syncStrategyToBytesBuilder #-}
syncStrategyToBytesBuilder = lazyByteString . syncStrategyToBytes

syncStrategyToText :: SyncStrategy -> T.Text
syncStrategyToText strategy =
  case strategy of
    SyncDrop ->
      "drop"

    SyncAbort ->
      "abort"

    SyncReplace ->
      "replace"

    SyncQueue mbQueue ->
      "queue" <> maybe mempty ((" " <>) . syncQueueStrategyToText) mbQueue

data SyncQueueStrategy
  = SyncQueueFirst
  | SyncQueueLast
  | SyncQueueAll
  deriving (Bounded, Enum, Eq, Show)

syncQueueStrategyToBytes :: SyncQueueStrategy -> LBS.ByteString
syncQueueStrategyToBytes strategy =
  case strategy of
    SyncQueueFirst -> "first"
    SyncQueueLast  -> "last"
    SyncQueueAll   -> "all"

syncQueueStrategyToBytesBuilder :: SyncQueueStrategy -> Builder
{-# INLINE syncQueueStrategyToBytesBuilder #-}
syncQueueStrategyToBytesBuilder = lazyByteString . syncQueueStrategyToBytes

syncQueueStrategyToText :: SyncQueueStrategy -> T.Text
syncQueueStrategyToText strategy =
  case strategy of
    SyncQueueFirst -> "first"
    SyncQueueLast  -> "last"
    SyncQueueAll   -> "all"
