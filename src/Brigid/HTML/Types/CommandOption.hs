module Brigid.HTML.Types.CommandOption
  ( CommandOption
      ( ShowModal
      , Close
      , RequestClose
      , ShowPopover
      , HidePopover
      , TogglePopover
      , CustomCommand
      )
  , commandOptionToBytes
  , commandOptionToBytesBuilder
  , commandOptionToText
  , commandOptionToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

import Brigid.Internal.Render qualified as Render

data CommandOption
  = ShowModal
  | Close
  | RequestClose
  | ShowPopover
  | HidePopover
  | TogglePopover
  | CustomCommand T.Text
  deriving (Eq, Show)

commandOptionToBytes :: CommandOption -> LBS.ByteString
commandOptionToBytes co =
  case co of
    ShowModal -> "show-modal"
    Close -> "close"
    RequestClose -> "request-close"
    ShowPopover -> "show-popover"
    HidePopover -> "hide-popover"
    TogglePopover -> "toggle-popover"
    CustomCommand cmd -> "--" <> Render.textToLazyBytes cmd

commandOptionToBytesBuilder :: CommandOption -> Builder
{-# INLINE commandOptionToBytesBuilder #-}
commandOptionToBytesBuilder = lazyByteString . commandOptionToBytes

commandOptionToText :: CommandOption -> T.Text
commandOptionToText co =
  case co of
    ShowModal -> "show-modal"
    Close -> "close"
    RequestClose -> "request-close"
    ShowPopover -> "show-popover"
    HidePopover -> "hide-popover"
    TogglePopover -> "toggle-popover"
    CustomCommand cmd -> "--" <> cmd

commandOptionToTextBuilder :: CommandOption -> TBL.Builder
commandOptionToTextBuilder = TBL.fromText . commandOptionToText
