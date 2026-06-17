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
  , asToBytesBuilder
  , asToText
  ) where

import Data.ByteString.Builder (Builder, string8)
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

asToBytesBuilder :: As -> Builder
{-# INLINE asToBytesBuilder #-}
asToBytesBuilder as =
  case as of
    AsAudio    -> string8 "audio"
    AsDocument -> string8 "document"
    AsEmbed    -> string8 "embed"
    AsFetch    -> string8 "fetch"
    AsFont     -> string8 "font"
    AsImage    -> string8 "image"
    AsObject   -> string8 "object"
    AsScript   -> string8 "script"
    AsStyle    -> string8 "style"
    AsTrack    -> string8 "track"
    AsVideo    -> string8 "video"
    AsWorker   -> string8 "worker"

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
