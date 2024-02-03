{-# LANGUAGE DataKinds #-}

module HTML.Elements.Tags
  ( Document
  , Comment
  , Text
  , RawHTML
  , CustomHTML
  , Anchor
  , Abbreviation
  , ContactAddress
  , Area
  , Article
  , Aside
  , Audio
  , BringAttentionTo
  , Base
  , BidirectionalIsolation
  , BidirectionalOverride
  , Blockquote
  , Body
  , LineBreak
  , Button
  , Canvas
  , TableCaption
  , Citation
  , Code
  , TableColumn
  , TableColumnGroup
  , Data
  , DataList
  , DescriptionDetails
  , DeletedText
  , Details
  , Definition
  , Dialog
  , Division
  , DescriptionList
  , DescriptionTerm
  , Emphasis
  , Embed
  , Fieldset
  , FigureCaption
  , Figure
  , Footer
  , Form
  , H1
  , H2
  , H3
  , H4
  , H5
  , H6
  , Head
  , Header
  , HeadingGroup
  , HorizontalRule
  , Html
  , IdiomaticText
  , IFrame
  , Image
  , Input
  , InsertedText
  , KeyboardInput
  , Label
  , Legend
  , ListItem
  , Link
  , Main
  , Map
  , Mark
  , Menu
  , Meta
  , Meter
  , Nav
  , NoScript
  , Object
  , OrderedList
  , OptionGroup
  , Option
  , Output
  , Paragraph
  , Picture
  , PreformattedText
  , Progress
  , Quotation
  , RubyParenthesis
  , RubyText
  , Ruby
  , Strikethrough
  , Sample
  , Script
  , Search
  , Section
  , Select
  , Slot
  , SideComment
  , Source
  , Span
  , Strong
  , Style
  , Subscript
  , Summary
  , Superscript
  , Table
  , TableBody
  , TableDataCell
  , ContentTemplate
  , TextArea
  , TableFoot
  , TableHeader
  , TableHead
  , Time
  , Title
  , TableRow
  , Track
  , Underline
  , UnorderedList
  , Variable
  , Video
  , WordBreakOpportunity
  ) where

import HTML.Elements.TagType qualified as TagType

-- This type synonym represents the HTML document itself. It exists so that the
-- HTML tag can have a parent. It is important that this tag type synonym never
-- be given a corresponding constructor for `ChildHTML`, and that it never be
-- exported from `HTML.Elements`.
type Document = 'TagType.Document

-- | This type synonym represents an HTML comment.
type Comment = 'TagType.Comment

-- | This type synonym represents (escaped) textual content within an element.
type Text = 'TagType.Text

-- | This type synonym represents raw HTML. This content is unchecked and
-- should be considered unsafe. Its intended use-case is for writing
-- out-of-spec HTML and inserting templating engine commands.
type RawHTML = 'TagType.RawHTML

-- | This type synonym represents a custom HTML element.
type CustomHTML = 'TagType.CustomHTML

-- | This type synonym represents the @\<a>@ element.
type Anchor = 'TagType.Anchor

-- | This type synonym represents the @\<abbr>@ element.
type Abbreviation = 'TagType.Abbreviation

-- | This type synonym represents the @\<address>@ element.
type ContactAddress = 'TagType.ContactAddress

-- | This type synonym represents the @\<area>@ element.
type Area = 'TagType.Area

-- | This type synonym represents the @\<article>@ element.
type Article = 'TagType.Article

-- | This type synonym represents the @\<aside>@ element.
type Aside = 'TagType.Aside

-- | This type synonym represents the @\<audio>@ element.
type Audio = 'TagType.Audio

-- | This type synonym represents the @\<b>@ element.
type BringAttentionTo = 'TagType.BringAttentionTo

-- | This type synonym represents the @\<base>@ element.
type Base = 'TagType.Base

-- | This type synonym represents the @\<bdi>@ element.
type BidirectionalIsolation = 'TagType.BidirectionalIsolation

-- | This type synonym represents the @\<bdo>@ element.
type BidirectionalOverride = 'TagType.BidirectionalOverride

-- | This type synonym represents the @\<blockquote>@ element.
type Blockquote = 'TagType.Blockquote

-- | This type synonym represents the @\<body>@ element.
type Body = 'TagType.Body

-- | This type synonym represents the @\<br>@ element.
type LineBreak = 'TagType.LineBreak

-- | This type synonym represents the @\<button>@ element.
type Button = 'TagType.Button

-- | This type synonym represents the @\<canvas>@ element.
type Canvas = 'TagType.Canvas

-- | This type synonym represents the @\<caption>@ element.
type TableCaption = 'TagType.TableCaption

-- | This type synonym represents the @\<cite>@ element.
type Citation = 'TagType.Citation

-- | This type synonym represents the @\<code>@ element.
type Code = 'TagType.Code

-- | This type synonym represents the @\<col>@ element.
type TableColumn = 'TagType.TableColumn

-- | This type synonym represents the @\<colgroup>@ element.
type TableColumnGroup = 'TagType.TableColumnGroup

-- | This type synonym represents the @\<data>@ element.
type Data = 'TagType.Data

-- | This type synonym represents the @\<datalist>@ element.
type DataList = 'TagType.DataList

-- | This type synonym represents the @\<dd>@ element.
type DescriptionDetails = 'TagType.DescriptionDetails

-- | This type synonym represents the @\<del>@ element.
type DeletedText = 'TagType.DeletedText

-- | This type synonym represents the @\<details>@ element.
type Details = 'TagType.Details

-- | This type synonym represents the @\<dfn>@ element.
type Definition = 'TagType.Definition

-- | This type synonym represents the @\<dialog>@ element.
type Dialog = 'TagType.Dialog

-- | This type synonym represents the @\<div>@ element.
type Division = 'TagType.Division

-- | This type synonym represents the @\<dl>@ element.
type DescriptionList = 'TagType.DescriptionList

-- | This type synonym represents the @\<dt>@ element.
type DescriptionTerm = 'TagType.DescriptionTerm

-- | This type synonym represents the @\<em>@ element.
type Emphasis = 'TagType.Emphasis

-- | This type synonym represents the @\<embed>@ element.
type Embed = 'TagType.Embed

-- | This type synonym represents the @\<fieldset>@ element.
type Fieldset = 'TagType.Fieldset

-- | This type synonym represents the @\<figcaption>@ element.
type FigureCaption = 'TagType.FigureCaption

-- | This type synonym represents the @\<figure>@ element.
type Figure = 'TagType.Figure

-- | This type synonym represents the @\<footer>@ element.
type Footer = 'TagType.Footer

-- | This type synonym represents the @\<form>@ element.
type Form = 'TagType.Form

-- | This type synonym represents the @\<h1>@ element.
type H1 = 'TagType.H1

-- | This type synonym represents the @\<h2>@ element.
type H2 = 'TagType.H2

-- | This type synonym represents the @\<h3>@ element.
type H3 = 'TagType.H3

-- | This type synonym represents the @\<h4>@ element.
type H4 = 'TagType.H4

-- | This type synonym represents the @\<h5>@ element.
type H5 = 'TagType.H5

-- | This type synonym represents the @\<h6>@ element.
type H6 = 'TagType.H6

-- | This type synonym represents the @\<head>@ element.
type Head = 'TagType.Head

-- | This type synonym represents the @\<header>@ element.
type Header = 'TagType.Header

-- | This type synonym represents the @\<hgroup>@ element.
type HeadingGroup = 'TagType.HeadingGroup

-- | This type synonym represents the @\<hr>@ element.
type HorizontalRule = 'TagType.HorizontalRule

{-| This type synonym represents the @\<html>@ element. It will also apply the
   documentation type declaration (@DOCTYPE HTML@).
-}
type Html = 'TagType.Html

-- | This type synonym represents the @\<i>@ element.
type IdiomaticText = 'TagType.IdiomaticText

-- | This type synonym represents the @\<iframe>@ element.
type IFrame = 'TagType.IFrame

-- | This type synonym represents the @\<img>@ element.
type Image = 'TagType.Image

-- | This type synonym represents the @\<input>@ element.
type Input = 'TagType.Input

-- | This type synonym represents the @\<ins>@ element.
type InsertedText = 'TagType.InsertedText

-- | This type synonym represents the @\<kbd>@ element.
type KeyboardInput = 'TagType.KeyboardInput

-- | This type synonym represents the @\<label>@ element.
type Label = 'TagType.Label

-- | This type synonym represents the @\<legend>@ element.
type Legend = 'TagType.Legend

-- | This type synonym represents the @\<li>@ element.
type ListItem = 'TagType.ListItem

-- | This type synonym represents the @\<link>@ element.
type Link = 'TagType.Link

-- | This type synonym represents the @\<main>@ element.
type Main = 'TagType.Main

-- | This type synonym represents the @\<map>@ element.
type Map = 'TagType.Map

-- | This type synonym represents the @\<mark>@ element.
type Mark = 'TagType.Mark

-- | This type synonym represents the @\<menu>@ element.
type Menu = 'TagType.Menu

-- | This type synonym represents the @\<meta>@ element.
type Meta = 'TagType.Meta

-- | This type synonym represents the @\<meter>@ element.
type Meter = 'TagType.Meter

-- | This type synonym represents the @\<nav>@ element.
type Nav = 'TagType.Nav

-- | This type synonym represents the @\<noscript>@ element.
type NoScript = 'TagType.NoScript

-- | This type synonym represents the @\<object>@ element.
type Object = 'TagType.Object

-- | This type synonym represents the @\<ol>@ element.
type OrderedList = 'TagType.OrderedList

-- | This type synonym represents the @\<optgroup>@ element.
type OptionGroup = 'TagType.OptionGroup

-- | This type synonym represents the @\<option>@ element.
type Option = 'TagType.Option

-- | This type synonym represents the @\<output>@ element.
type Output = 'TagType.Output

-- | This type synonym represents the @\<p>@ element.
type Paragraph = 'TagType.Paragraph

-- | This type synonym represents the @\<picture>@ element.
type Picture = 'TagType.Picture

-- | This type synonym represents the @\<pre>@ element.
type PreformattedText = 'TagType.PreformattedText

-- | This type synonym represents the @\<progress>@ element.
type Progress = 'TagType.Progress

-- | This type synonym represents the @\<q>@ element.
type Quotation = 'TagType.Quotation

-- | This type synonym represents the @\<rp>@ element.
type RubyParenthesis = 'TagType.RubyParenthesis

-- | This type synonym represents the @\<rt>@ element.
type RubyText = 'TagType.RubyText

-- | This type synonym represents the @\<ruby>@ element.
type Ruby = 'TagType.Ruby

-- | This type synonym represents the @\<s>@ element.
type Strikethrough = 'TagType.Strikethrough

-- | This type synonym represents the @\<samp>@ element.
type Sample = 'TagType.Sample

-- | This type synonym represents the @\<script>@ element.
type Script = 'TagType.Script

-- | This type synonym represents the @\<search>@ element.
type Search = 'TagType.Search

-- | This type synonym represents the @\<section>@ element.
type Section = 'TagType.Section

-- | This type synonym represents the @\<select>@ element.
type Select = 'TagType.Select

-- | This type synonym represents the @\<slot>@ element.
type Slot = 'TagType.Slot

-- | This type synonym represents the @\<small>@ element.
type SideComment = 'TagType.SideComment

-- | This type synonym represents the @\<source>@ element.
type Source = 'TagType.Source

-- | This type synonym represents the @\<span>@ element.
type Span = 'TagType.Span

-- | This type synonym represents the @\<strong>@ element.
type Strong = 'TagType.Strong

-- | This type synonym represents the @\<style>@ element.
type Style = 'TagType.Style

-- | This type synonym represents the @\<sub>@ element.
type Subscript = 'TagType.Subscript

-- | This type synonym represents the @\<summary>@ element.
type Summary = 'TagType.Summary

-- | This type synonym represents the @\<sup>@ element.
type Superscript = 'TagType.Superscript

-- | This type synonym represents the @\<table>@ element.
type Table = 'TagType.Table

-- | This type synonym represents the @\<tbody>@ element.
type TableBody = 'TagType.TableBody

-- | This type synonym represents the @\<td>@ element.
type TableDataCell = 'TagType.TableDataCell

-- | This type synonym represents the @\<template>@ element.
type ContentTemplate = 'TagType.ContentTemplate

-- | This type synonym represents the @\<textarea>@ element.
type TextArea = 'TagType.TextArea

-- | This type synonym represents the @\<tfoot>@ element.
type TableFoot = 'TagType.TableFoot

-- | This type synonym represents the @\<th>@ element.
type TableHeader = 'TagType.TableHeader

-- | This type synonym represents the @\<thead>@ element.
type TableHead = 'TagType.TableHead

-- | This type synonym represents the @\<time>@ element.
type Time = 'TagType.Time

-- | This type synonym represents the @\<title>@ element.
type Title = 'TagType.Title

-- | This type synonym represents the @\<tr>@ element.
type TableRow = 'TagType.TableRow

-- | This type synonym represents the @\<track>@ element.
type Track = 'TagType.Track

-- | This type synonym represents the @\<u>@ element.
type Underline = 'TagType.Underline

-- | This type synonym represents the @\<ul>@ element.
type UnorderedList = 'TagType.UnorderedList

-- | This type synonym represents the @\<var>@ element.
type Variable = 'TagType.Variable

-- | This type synonym represents the @\<video>@ element.
type Video = 'TagType.Video

-- | This type synonym represents the @\<wbr>@ element.
type WordBreakOpportunity = 'TagType.WordBreakOpportunity
