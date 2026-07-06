module Brigid.HTML.Types.TrackKind
  ( TrackKind
      ( Subtitles
      , Captions
      , Chapters
      , Metadata
      )
  , trackKindToBytes
  , trackKindToBytesBuilder
  , trackKindToText
  , trackKindToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data TrackKind
  = Subtitles
  | Captions
  | Chapters
  | Metadata
  deriving (Bounded, Enum, Eq, Show)

trackKindToBytes :: TrackKind -> LBS.ByteString
trackKindToBytes trackKind =
  case trackKind of
    Subtitles -> "subtitles"
    Captions  -> "captions"
    Chapters  -> "chapters"
    Metadata  -> "metadata"

trackKindToBytesBuilder :: TrackKind -> Builder
{-# INLINE trackKindToBytesBuilder #-}
trackKindToBytesBuilder = lazyByteString . trackKindToBytes

trackKindToText :: TrackKind -> T.Text
trackKindToText trackKind =
  case trackKind of
    Subtitles -> "subtitles"
    Captions  -> "captions"
    Chapters  -> "chapters"
    Metadata  -> "metadata"

trackKindToTextBuilder :: TrackKind -> TBL.Builder
trackKindToTextBuilder = TBL.fromText . trackKindToText
