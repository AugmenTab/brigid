module Brigid.HXML.Elements
  ( HXML
  , ChildHXML
  , noElement
  , Tags.Comment, comment
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

import Data.Text qualified as T

import Brigid.HXML.Elements.Children (ValidChild)
import Brigid.HXML.Elements.Internal (ChildHXML (..))
import Brigid.HXML.Elements.Tags qualified as Tags
import Brigid.HXML.Types (NoContent)

type HXML = ChildHXML Tags.HXML

noElement :: ChildHXML parent
noElement = Tag_NoElement

comment :: T.Text -> ChildHXML parent
comment = Tag_Comment

rawHXML :: T.Text -> ChildHXML parent
rawHXML = Tag_RawHXML

customHXML :: T.Text
           -> Either NoContent [ChildHXML Tags.CustomHXML]
           -> ChildHXML parent
customHXML = Tag_CustomHXML

behavior :: ChildHXML parent
behavior = Tag_Behavior

body :: ValidChild Tags.Body parent
     => [ChildHXML Tags.Body] -> ChildHXML parent
body = Tag_Body

dateField :: ChildHXML parent
dateField = Tag_DateField

doc :: ValidChild Tags.Document parent
    => [ChildHXML Tags.Document] -> ChildHXML parent
doc = Tag_Document

form :: ValidChild Tags.Form parent
     => [ChildHXML Tags.Form] -> ChildHXML parent
form = Tag_Form

header :: ValidChild Tags.Header parent
       => [ChildHXML Tags.Header] -> ChildHXML parent
header = Tag_Header

image :: ChildHXML parent
image = Tag_Image

item :: ValidChild Tags.Item parent
     => [ChildHXML Tags.Item] -> ChildHXML parent
item = Tag_Item

items :: ValidChild Tags.Items parent
      => [ChildHXML Tags.Items] -> ChildHXML parent
items = Tag_Items

list :: ValidChild Tags.List parent
     => [ChildHXML Tags.List] -> ChildHXML parent
list = Tag_List

modifier :: ValidChild Tags.Modifier parent
         => [ChildHXML Tags.Modifier] -> ChildHXML parent
modifier = Tag_Modifier

navigator :: ValidChild Tags.Navigator parent
          => [ChildHXML Tags.Navigator] -> ChildHXML parent
navigator = Tag_Navigator

navRoute :: ChildHXML parent
navRoute = Tag_NavRoute

option :: ValidChild Tags.Option parent
       => [ChildHXML Tags.Option] -> ChildHXML parent
option = Tag_Option

pickerField :: ValidChild Tags.PickerField parent
            => [ChildHXML Tags.PickerField] -> ChildHXML parent
pickerField = Tag_PickerField

pickerItem :: ChildHXML parent
pickerItem = Tag_PickerItem

screen :: ValidChild Tags.Screen parent
       => [ChildHXML Tags.Screen] -> ChildHXML parent
screen = Tag_Screen

section :: ChildHXML parent
section = Tag_Section

sectionList :: ValidChild Tags.SectionList parent
            => [ChildHXML Tags.SectionList] -> ChildHXML parent
sectionList = Tag_SectionList

sectionTitle :: ValidChild Tags.SectionTitle parent
             => [ChildHXML Tags.SectionTitle] -> ChildHXML parent
sectionTitle = Tag_SectionTitle

selectMultiple :: ValidChild Tags.SelectMultiple parent
               => [ChildHXML Tags.SelectMultiple] -> ChildHXML parent
selectMultiple = Tag_SelectMultiple

selectSingle :: ValidChild Tags.SelectSingle parent
             => [ChildHXML Tags.SelectSingle] -> ChildHXML parent
selectSingle = Tag_SelectSingle

spinner :: ChildHXML parent
spinner = Tag_Spinner

style :: ValidChild Tags.Style parent
      => [ChildHXML Tags.Style] -> ChildHXML parent
style = Tag_Style

styles :: ValidChild Tags.Styles parent
       => [ChildHXML Tags.Styles] -> ChildHXML parent
styles = Tag_Styles

switch :: ChildHXML parent
switch = Tag_Switch

text :: ChildHXML parent
text = Tag_Text

textArea :: ChildHXML parent
textArea = Tag_TextArea

textField :: ChildHXML parent
textField = Tag_TextField

view :: ValidChild Tags.View parent
     => [ChildHXML Tags.View] -> ChildHXML parent
view = Tag_View

webView :: ChildHXML parent
webView = Tag_WebView
