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

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import Data.Text.Encoding qualified as TE

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
commandOptionToBytesBuilder co =
  case co of
    ShowModal         -> string8 "show-modal"
    Close             -> string8 "close"
    RequestClose      -> string8 "request-close"
    ShowPopover       -> string8 "show-popover"
    HidePopover       -> string8 "hide-popover"
    TogglePopover     -> string8 "toggle-popover"
    CustomCommand cmd -> string8 "--" <> TE.encodeUtf8Builder cmd

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
