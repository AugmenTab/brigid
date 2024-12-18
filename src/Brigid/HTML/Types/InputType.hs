module Brigid.HTML.Types.InputType
  ( InputType
      ( Button
      , Checkbox
      , Color
      , Date
      , DatetimeLocal
      , Email
      , File
      , Hidden
      , Image
      , Month
      , Number
      , Password
      , Radio
      , Range
      , Reset
      , Search
      , Submit
      , Tel
      , Text
      , Time
      , Url
      , Week
      )
  , inputTypeToBytes
  , inputTypeToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data InputType
  = Button
  | Checkbox
  | Color
  | Date
  | DatetimeLocal
  | Email
  | File
  | Hidden
  | Image
  | Month
  | Number
  | Password
  | Radio
  | Range
  | Reset
  | Search
  | Submit
  | Tel
  | Text
  | Time
  | Url
  | Week

inputTypeToBytes :: InputType -> LBS.ByteString
inputTypeToBytes input =
  case input of
    Button        -> "button"
    Checkbox      -> "checkbox"
    Color         -> "color"
    Date          -> "date"
    DatetimeLocal -> "datetime-local"
    Email         -> "email"
    File          -> "file"
    Hidden        -> "hidden"
    Image         -> "image"
    Month         -> "month"
    Number        -> "number"
    Password      -> "password"
    Radio         -> "radio"
    Range         -> "range"
    Reset         -> "reset"
    Search        -> "search"
    Submit        -> "submit"
    Tel           -> "tel"
    Text          -> "text"
    Time          -> "time"
    Url           -> "url"
    Week          -> "week"

inputTypeToText :: InputType -> T.Text
inputTypeToText input =
  case input of
    Button        -> "button"
    Checkbox      -> "checkbox"
    Color         -> "color"
    Date          -> "date"
    DatetimeLocal -> "datetime-local"
    Email         -> "email"
    File          -> "file"
    Hidden        -> "hidden"
    Image         -> "image"
    Month         -> "month"
    Number        -> "number"
    Password      -> "password"
    Radio         -> "radio"
    Range         -> "range"
    Reset         -> "reset"
    Search        -> "search"
    Submit        -> "submit"
    Tel           -> "tel"
    Text          -> "text"
    Time          -> "time"
    Url           -> "url"
    Week          -> "week"
