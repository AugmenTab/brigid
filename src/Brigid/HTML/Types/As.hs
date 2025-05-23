module Brigid.HTML.Types.As
  ( As
      ( AsAudio
      , AsDocument
      , AsEmbed
      , AsFetch
      , AsFont
      , AsImage
      , AsObject
      , AsScript
      , AsStyle
      , AsTrack
      , AsVideo
      , AsWorker
      )
  , asToBytes
  , asToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data As
  = AsAudio
  | AsDocument
  | AsEmbed
  | AsFetch
  | AsFont
  | AsImage
  | AsObject
  | AsScript
  | AsStyle
  | AsTrack
  | AsVideo
  | AsWorker
  deriving (Bounded, Enum, Eq, Show)

asToBytes :: As -> LBS.ByteString
asToBytes as =
  case as of
    AsAudio    -> "audio"
    AsDocument -> "document"
    AsEmbed    -> "embed"
    AsFetch    -> "fetch"
    AsFont     -> "font"
    AsImage    -> "image"
    AsObject   -> "object"
    AsScript   -> "script"
    AsStyle    -> "style"
    AsTrack    -> "track"
    AsVideo    -> "video"
    AsWorker   -> "worker"

asToText :: As -> T.Text
asToText as =
  case as of
    AsAudio    -> "audio"
    AsDocument -> "document"
    AsEmbed    -> "embed"
    AsFetch    -> "fetch"
    AsFont     -> "font"
    AsImage    -> "image"
    AsObject   -> "object"
    AsScript   -> "script"
    AsStyle    -> "style"
    AsTrack    -> "track"
    AsVideo    -> "video"
    AsWorker   -> "worker"
