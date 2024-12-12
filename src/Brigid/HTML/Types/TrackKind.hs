module Brigid.HTML.Types.TrackKind
  ( TrackKind
      ( Subtitles
      , Captions
      , Chapters
      , Metadata
      )
  , trackKindToBytes
  , trackKindToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data TrackKind
  = Subtitles
  | Captions
  | Chapters
  | Metadata

trackKindToBytes :: TrackKind -> LBS.ByteString
trackKindToBytes trackKind =
  case trackKind of
    Subtitles -> "subtitles"
    Captions  -> "captions"
    Chapters  -> "chapters"
    Metadata  -> "metadata"

trackKindToText :: TrackKind -> T.Text
trackKindToText trackKind =
  case trackKind of
    Subtitles -> "subtitles"
    Captions  -> "captions"
    Chapters  -> "chapters"
    Metadata  -> "metadata"
