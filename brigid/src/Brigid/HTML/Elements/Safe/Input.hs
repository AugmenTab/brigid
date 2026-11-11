module Brigid.HTML.Elements.Safe.Input
  ( Tags.InputButton, button
  , Tags.InputCheckbox, checkbox
  , Tags.InputColor, color
  , Tags.InputDate, date
  , Tags.InputDatetimeLocal, datetimeLocal
  , Tags.InputEmail, email
  , Tags.InputFile, file
  , Tags.InputHidden, hidden
  , Tags.InputImage, image
  , Tags.InputMonth, month
  , Tags.InputNumber, number
  , Tags.InputPassword, password
  , Tags.InputRadio, radio
  , Tags.InputRange, range
  , Tags.InputReset, reset
  , Tags.InputSearch, search
  , Tags.InputSubmit, submit
  , Tags.InputTel, tel
  , Tags.InputText, text
  , Tags.InputTime, time
  , Tags.InputUrl, url
  , Tags.InputWeek, week
  ) where

import Brigid.HTML.Attributes.Scoped (type_)
import Brigid.HTML.Attributes.Internal (Attribute)
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Internal (ChildHTML (..))
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Types (InputType (..))

button :: ValidChild Tags.InputButton parent grandparent
       => [Attribute Tags.InputButton]
       -> ChildHTML parent grandparent
button =
  Tag_InputButton . (type_ InputButton :)

checkbox :: ValidChild Tags.InputCheckbox parent grandparent
         => [Attribute Tags.InputCheckbox]
         -> ChildHTML parent grandparent
checkbox =
  Tag_InputCheckbox . (type_ InputCheckbox :)

color :: ValidChild Tags.InputColor parent grandparent
      => [Attribute Tags.InputColor]
      -> ChildHTML parent grandparent
color =
  Tag_InputColor . (type_ InputColor :)

date :: ValidChild Tags.InputDate parent grandparent
     => [Attribute Tags.InputDate]
     -> ChildHTML parent grandparent
date =
  Tag_InputDate . (type_ InputDate :)

datetimeLocal :: ValidChild Tags.InputDatetimeLocal parent grandparent
              => [Attribute Tags.InputDatetimeLocal]
              -> ChildHTML parent grandparent
datetimeLocal =
  Tag_InputDatetimeLocal . (type_ InputDatetimeLocal :)

email :: ValidChild Tags.InputEmail parent grandparent
      => [Attribute Tags.InputEmail]
      -> ChildHTML parent grandparent
email =
  Tag_InputEmail . (type_ InputEmail :)

file :: ValidChild Tags.InputFile parent grandparent
     => [Attribute Tags.InputFile]
     -> ChildHTML parent grandparent
file =
  Tag_InputFile . (type_ InputFile :)

hidden :: ValidChild Tags.InputHidden parent grandparent
       => [Attribute Tags.InputHidden]
       -> ChildHTML parent grandparent
hidden =
  Tag_InputHidden . (type_ InputHidden :)

image :: ValidChild Tags.InputImage parent grandparent
      => [Attribute Tags.InputImage]
      -> ChildHTML parent grandparent
image =
  Tag_InputImage . (type_ InputImage :)

month :: ValidChild Tags.InputMonth parent grandparent
      => [Attribute Tags.InputMonth]
      -> ChildHTML parent grandparent
month =
  Tag_InputMonth . (type_ InputMonth :)

number :: ValidChild Tags.InputNumber parent grandparent
       => [Attribute Tags.InputNumber]
       -> ChildHTML parent grandparent
number =
  Tag_InputNumber . (type_ InputNumber :)

password :: ValidChild Tags.InputPassword parent grandparent
         => [Attribute Tags.InputPassword]
         -> ChildHTML parent grandparent
password =
  Tag_InputPassword . (type_ InputPassword :)

-- TODO: How to ensure that only one element has the `checked` attribute?
--
radio :: ValidChild Tags.InputRadio parent grandparent
      => [Attribute Tags.InputRadio]
      -> ChildHTML parent grandparent
radio =
  Tag_InputRadio . (type_ InputRadio :)

range :: ValidChild Tags.InputRange parent grandparent
      => [Attribute Tags.InputRange]
      -> ChildHTML parent grandparent
range =
  Tag_InputRange . (type_ InputRange :)

reset :: ValidChild Tags.InputReset parent grandparent
      => [Attribute Tags.InputReset]
      -> ChildHTML parent grandparent
reset =
  Tag_InputReset . (type_ InputReset :)

search :: ValidChild Tags.InputSearch parent grandparent
       => [Attribute Tags.InputSearch]
       -> ChildHTML parent grandparent
search =
  Tag_InputSearch . (type_ InputSearch :)

submit :: ValidChild Tags.InputSubmit parent grandparent
       => [Attribute Tags.InputSubmit]
       -> ChildHTML parent grandparent
submit =
  Tag_InputSubmit . (type_ InputSubmit :)

tel :: ValidChild Tags.InputTel parent grandparent
    => [Attribute Tags.InputTel]
    -> ChildHTML parent grandparent
tel =
  Tag_InputTel . (type_ InputTel :)

text :: ValidChild Tags.InputText parent grandparent
     => [Attribute Tags.InputText]
     -> ChildHTML parent grandparent
text =
  Tag_InputText . (type_ InputText :)

time :: ValidChild Tags.InputTime parent grandparent
     => [Attribute Tags.InputTime]
     -> ChildHTML parent grandparent
time =
  Tag_InputTime . (type_ InputTime :)

url :: ValidChild Tags.InputUrl parent grandparent
    => [Attribute Tags.InputUrl]
    -> ChildHTML parent grandparent
url =
  Tag_InputUrl . (type_ InputUrl :)

week :: ValidChild Tags.InputWeek parent grandparent
     => [Attribute Tags.InputWeek]
     -> ChildHTML parent grandparent
week =
  Tag_InputWeek . (type_ InputWeek :)
