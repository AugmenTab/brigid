module Brigid.HXML.Elements
  ( HXML
  , ChildHXML
  , noElement
  , Tags.Comment, comment
  , Tags.Content, content
  , Tags.RawHXML, rawHXML
  , Tags.CustomHXML, customHXML
  , Tags.Behavior, behavior
  , Tags.Body, body
  , Tags.DateField, dateField
  , Tags.Document, doc
  , Tags.Form, form
  , Tags.Header, header
  , Tags.Image, image
  , Tags.Item, item
  , Tags.Items, items
  , Tags.List, list
  , Tags.Modifier, modifier
  , Tags.Navigator, navigator
  , Tags.NavRoute, navRoute
  , Tags.Option, option
  , Tags.PickerField, pickerField
  , Tags.PickerItem, pickerItem
  , Tags.Screen, screen
  , Tags.Section, section
  , Tags.SectionList, sectionList
  , Tags.SectionTitle, sectionTitle
  , Tags.SelectMultiple, selectMultiple
  , Tags.SelectSingle, selectSingle
  , Tags.Spinner, spinner
  , Tags.Style, style
  , Tags.Styles, styles
  , Tags.Switch, switch
  , Tags.Text, text
  , Tags.TextArea, textArea
  , Tags.TextField, textField
  , Tags.View, view
  , Tags.WebView, webView
  ) where

import Data.Containers.ListUtils (nubOrdOn)
import Data.Text qualified as T

import Brigid.HXML.Attributes.Internal (Attribute, attributeText)
import Brigid.HXML.Elements.Children (ValidChild)
import Brigid.HXML.Elements.Internal (ChildHXML (..))
import Brigid.HXML.Elements.Tags qualified as Tags
import Brigid.Types.NoContent (NoContent)

type HXML = ChildHXML Tags.HXML

noElement :: ChildHXML parent
noElement = Tag_NoElement

comment :: T.Text -> ChildHXML parent
comment = Tag_Comment

content :: ValidChild Tags.Content parent => T.Text -> ChildHXML parent
content = Tag_Content

rawHXML :: T.Text -> ChildHXML parent
rawHXML = Tag_RawHXML

customHXML :: T.Text
           -> [Attribute Tags.CustomHXML]
           -> Either NoContent [ChildHXML Tags.CustomHXML]
           -> ChildHXML parent
customHXML element =
  Tag_CustomHXML element . nubOrdOn attributeText

behavior :: [Attribute Tags.Behavior] -> ChildHXML parent
behavior = Tag_Behavior . nubOrdOn attributeText

body :: ValidChild Tags.Body parent
     => [Attribute Tags.Body] -> [ChildHXML Tags.Body] -> ChildHXML parent
body = Tag_Body . nubOrdOn attributeText

dateField :: [Attribute Tags.DateField] -> ChildHXML parent
dateField = Tag_DateField . nubOrdOn attributeText

doc :: ValidChild Tags.Document parent
    => [Attribute Tags.Document] -> [ChildHXML Tags.Document] -> ChildHXML parent
doc = Tag_Document . nubOrdOn attributeText

form :: ValidChild Tags.Form parent
     => [Attribute Tags.Form] -> [ChildHXML Tags.Form] -> ChildHXML parent
form = Tag_Form . nubOrdOn attributeText

header :: ValidChild Tags.Header parent
       => [Attribute Tags.Header] -> [ChildHXML Tags.Header] -> ChildHXML parent
header = Tag_Header . nubOrdOn attributeText

image :: ValidChild Tags.Image parent
      => [Attribute Tags.Image]
      -> [ChildHXML Tags.Image]
      -> ChildHXML parent
image = Tag_Image . nubOrdOn attributeText

item :: ValidChild Tags.Item parent
     => [Attribute Tags.Item] -> [ChildHXML Tags.Item] -> ChildHXML parent
item = Tag_Item . nubOrdOn attributeText

items :: ValidChild Tags.Items parent
      => [Attribute Tags.Items] -> [ChildHXML Tags.Items] -> ChildHXML parent
items = Tag_Items . nubOrdOn attributeText

list :: ValidChild Tags.List parent
     => [Attribute Tags.List] -> [ChildHXML Tags.List] -> ChildHXML parent
list = Tag_List . nubOrdOn attributeText

modifier :: ValidChild Tags.Modifier parent
         => [Attribute Tags.Modifier]
         -> [ChildHXML Tags.Modifier]
         -> ChildHXML parent
modifier = Tag_Modifier . nubOrdOn attributeText

navigator :: ValidChild Tags.Navigator parent
          => [Attribute Tags.Navigator]
          -> [ChildHXML Tags.Navigator]
          -> ChildHXML parent
navigator = Tag_Navigator . nubOrdOn attributeText

navRoute :: ValidChild Tags.NavRoute parent
         => [Attribute Tags.NavRoute]
         -> [ChildHXML Tags.NavRoute]
         -> ChildHXML parent
navRoute = Tag_NavRoute . nubOrdOn attributeText

option :: ValidChild Tags.Option parent
       => [Attribute Tags.Option] -> [ChildHXML Tags.Option] -> ChildHXML parent
option = Tag_Option . nubOrdOn attributeText

pickerField :: ValidChild Tags.PickerField parent
            => [Attribute Tags.PickerField]
            -> [ChildHXML Tags.PickerField]
            -> ChildHXML parent
pickerField = Tag_PickerField . nubOrdOn attributeText

pickerItem :: [Attribute Tags.PickerItem] -> ChildHXML parent
pickerItem = Tag_PickerItem . nubOrdOn attributeText

screen :: ValidChild Tags.Screen parent
       => [Attribute Tags.Screen] -> [ChildHXML Tags.Screen] -> ChildHXML parent
screen = Tag_Screen . nubOrdOn attributeText

section :: ValidChild Tags.Section parent
        => [Attribute Tags.Section]
        -> [ChildHXML Tags.Section]
        -> ChildHXML parent
section = Tag_Section . nubOrdOn attributeText

sectionList :: ValidChild Tags.SectionList parent
            => [Attribute Tags.SectionList]
            -> [ChildHXML Tags.SectionList]
            -> ChildHXML parent
sectionList = Tag_SectionList . nubOrdOn attributeText

sectionTitle :: ValidChild Tags.SectionTitle parent
             => [Attribute Tags.SectionTitle]
             -> [ChildHXML Tags.SectionTitle]
             -> ChildHXML parent
sectionTitle = Tag_SectionTitle . nubOrdOn attributeText

selectMultiple :: ValidChild Tags.SelectMultiple parent
               => [Attribute Tags.SelectMultiple]
               -> [ChildHXML Tags.SelectMultiple]
               -> ChildHXML parent
selectMultiple = Tag_SelectMultiple . nubOrdOn attributeText

selectSingle :: ValidChild Tags.SelectSingle parent
             => [Attribute Tags.SelectSingle]
             -> [ChildHXML Tags.SelectSingle]
             -> ChildHXML parent
selectSingle = Tag_SelectSingle . nubOrdOn attributeText

spinner :: [Attribute Tags.Spinner] -> ChildHXML parent
spinner = Tag_Spinner . nubOrdOn attributeText

style :: ValidChild Tags.Style parent
      => [Attribute Tags.Style] -> [ChildHXML Tags.Style] -> ChildHXML parent
style = Tag_Style . nubOrdOn attributeText

styles :: ValidChild Tags.Styles parent
       => [ChildHXML Tags.Styles] -> ChildHXML parent
styles = Tag_Styles

switch :: [Attribute Tags.Switch] -> ChildHXML parent
switch = Tag_Switch . nubOrdOn attributeText

text :: ValidChild Tags.Text parent
     => [Attribute Tags.Text] -> [ChildHXML Tags.Text] -> ChildHXML parent
text = Tag_Text . nubOrdOn attributeText

textArea :: [Attribute Tags.TextArea] -> ChildHXML parent
textArea = Tag_TextArea . nubOrdOn attributeText

textField :: [Attribute Tags.TextField] -> ChildHXML parent
textField = Tag_TextField . nubOrdOn attributeText

view :: ValidChild Tags.View parent
     => [Attribute Tags.View] -> [ChildHXML Tags.View] -> ChildHXML parent
view = Tag_View . nubOrdOn attributeText

webView :: [Attribute Tags.WebView] -> ChildHXML parent
webView = Tag_WebView . nubOrdOn attributeText
