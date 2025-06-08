module Brigid.HTML.Types.ControlsList
  ( ControlsList
      ( NoDownload
      , NoFullscreen
      , NoRemotePlayback
      )
  , controlsListToBytes
  , controlsListToText
  ) where

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

controlsListToText :: ControlsList -> T.Text
controlsListToText controlslist =
  case controlslist of
    NoDownload       -> "nodownload"
    NoFullscreen     -> "nofullscreen"
    NoRemotePlayback -> "noremoteplayback"
