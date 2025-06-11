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
  , commandOptionToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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
