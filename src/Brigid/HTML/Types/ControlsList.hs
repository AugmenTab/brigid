module Brigid.HTML.Types.ControlsList
  ( ControlsList
      ( NoDownload
      , NoFullscreen
      , NoRemotePlayback
      )
  , controlslistToBytes
  , controlslistToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data ControlsList
  = NoDownload
  | NoFullscreen
  | NoRemotePlayback

controlslistToBytes :: ControlsList -> LBS.ByteString
controlslistToBytes controlslist =
  case controlslist of
    NoDownload       -> "nodownload"
    NoFullscreen     -> "nofullscreen"
    NoRemotePlayback -> "noremoteplayback"

controlslistToText :: ControlsList -> T.Text
controlslistToText controlslist =
  case controlslist of
    NoDownload       -> "nodownload"
    NoFullscreen     -> "nofullscreen"
    NoRemotePlayback -> "noremoteplayback"
