module Brigid.HTML.Types.ControlsList
  ( ControlsList
      ( NoDownload
      , NoFullscreen
      , NoRemotePlayback
      )
  , controlsListToBytes
  , controlsListToBytesBuilder
  , controlsListToText
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data ControlsList
  = NoDownload
  | NoFullscreen
  | NoRemotePlayback
  deriving (Bounded, Enum, Eq, Show)

controlsListToBytes :: ControlsList -> LBS.ByteString
controlsListToBytes controlslist =
  case controlslist of
    NoDownload       -> "nodownload"
    NoFullscreen     -> "nofullscreen"
    NoRemotePlayback -> "noremoteplayback"

controlsListToBytesBuilder :: ControlsList -> Builder
{-# INLINE controlsListToBytesBuilder #-}
controlsListToBytesBuilder controlslist =
  case controlslist of
    NoDownload       -> string8 "nodownload"
    NoFullscreen     -> string8 "nofullscreen"
    NoRemotePlayback -> string8 "noremoteplayback"

controlsListToText :: ControlsList -> T.Text
controlsListToText controlslist =
  case controlslist of
    NoDownload       -> "nodownload"
    NoFullscreen     -> "nofullscreen"
    NoRemotePlayback -> "noremoteplayback"
