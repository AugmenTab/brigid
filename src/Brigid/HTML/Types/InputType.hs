module Brigid.HTML.Types.InputType
  ( InputType
      ( InputButton
      , InputCheckbox
      , InputColor
      , InputDate
      , InputDatetimeLocal
      , InputEmail
      , InputFile
      , InputHidden
      , InputImage
      , InputMonth
      , InputNumber
      , InputPassword
      , InputRadio
      , InputRange
      , InputReset
      , InputSearch
      , InputSubmit
      , InputTel
      , InputText
      , InputTime
      , InputUrl
      , InputWeek
      )
  , inputTypeToBytes
  , inputTypeToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data InputType
  = InputButton
  | InputCheckbox
  | InputColor
  | InputDate
  | InputDatetimeLocal
  | InputEmail
  | InputFile
  | InputHidden
  | InputImage
  | InputMonth
  | InputNumber
  | InputPassword
  | InputRadio
  | InputRange
  | InputReset
  | InputSearch
  | InputSubmit
  | InputTel
  | InputText
  | InputTime
  | InputUrl
  | InputWeek

inputTypeToBytes :: InputType -> LBS.ByteString
inputTypeToBytes input =
  case input of
    InputButton        -> "button"
    InputCheckbox      -> "checkbox"
    InputColor         -> "color"
    InputDate          -> "date"
    InputDatetimeLocal -> "datetime-local"
    InputEmail         -> "email"
    InputFile          -> "file"
    InputHidden        -> "hidden"
    InputImage         -> "image"
    InputMonth         -> "month"
    InputNumber        -> "number"
    InputPassword      -> "password"
    InputRadio         -> "radio"
    InputRange         -> "range"
    InputReset         -> "reset"
    InputSearch        -> "search"
    InputSubmit        -> "submit"
    InputTel           -> "tel"
    InputText          -> "text"
    InputTime          -> "time"
    InputUrl           -> "url"
    InputWeek          -> "week"

inputTypeToText :: InputType -> T.Text
inputTypeToText input =
  case input of
    InputButton        -> "button"
    InputCheckbox      -> "checkbox"
    InputColor         -> "color"
    InputDate          -> "date"
    InputDatetimeLocal -> "datetime-local"
    InputEmail         -> "email"
    InputFile          -> "file"
    InputHidden        -> "hidden"
    InputImage         -> "image"
    InputMonth         -> "month"
    InputNumber        -> "number"
    InputPassword      -> "password"
    InputRadio         -> "radio"
    InputRange         -> "range"
    InputReset         -> "reset"
    InputSearch        -> "search"
    InputSubmit        -> "submit"
    InputTel           -> "tel"
    InputText          -> "text"
    InputTime          -> "time"
    InputUrl           -> "url"
    InputWeek          -> "week"
