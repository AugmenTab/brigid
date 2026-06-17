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
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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
trackKindToBytesBuilder trackKind =
  case trackKind of
    Subtitles -> string8 "subtitles"
    Captions  -> string8 "captions"
    Chapters  -> string8 "chapters"
    Metadata  -> string8 "metadata"

trackKindToText :: TrackKind -> T.Text
trackKindToText trackKind =
  case trackKind of
    Subtitles -> "subtitles"
    Captions  -> "captions"
    Chapters  -> "chapters"
    Metadata  -> "metadata"
