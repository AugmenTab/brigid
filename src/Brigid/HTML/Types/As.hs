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
  , asToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

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
asToBytesBuilder = lazyByteString . asToBytes

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

asToTextBuilder :: As -> TBL.Builder
asToTextBuilder = TBL.fromText . asToText
