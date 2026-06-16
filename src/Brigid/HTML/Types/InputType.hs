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
  , inputTypeToBytesBuilder
  , inputTypeToText
  ) where

import Data.ByteString.Builder (Builder, string8)
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
  deriving (Bounded, Enum, Eq, Show)

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

inputTypeToBytesBuilder :: InputType -> Builder
inputTypeToBytesBuilder input =
  case input of
    InputButton        -> string8 "button"
    InputCheckbox      -> string8 "checkbox"
    InputColor         -> string8 "color"
    InputDate          -> string8 "date"
    InputDatetimeLocal -> string8 "datetime-local"
    InputEmail         -> string8 "email"
    InputFile          -> string8 "file"
    InputHidden        -> string8 "hidden"
    InputImage         -> string8 "image"
    InputMonth         -> string8 "month"
    InputNumber        -> string8 "number"
    InputPassword      -> string8 "password"
    InputRadio         -> string8 "radio"
    InputRange         -> string8 "range"
    InputReset         -> string8 "reset"
    InputSearch        -> string8 "search"
    InputSubmit        -> string8 "submit"
    InputTel           -> string8 "tel"
    InputText          -> string8 "text"
    InputTime          -> string8 "time"
    InputUrl           -> string8 "url"
    InputWeek          -> string8 "week"

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
