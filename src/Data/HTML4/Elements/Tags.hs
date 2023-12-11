{-# LANGUAGE DataKinds #-}

module Data.HTML4.Elements.Tags
  ( Comment
  , Text
  , Anchor
  , Abbreviation
  , Address
  , Area
  , Article
  , Aside
  , Audio
  , Bold
  , Base
  , BiDirectionalIsolation
  , BiDirectionalOverride
  , Blockquote
  , Body
  , Break
  , Button
  , Canvas
  , Caption
  , Cite
  , Code
  , Column
  , ColumnGroup
  , Data
  , DataList
  , Description
  , Deletion
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
  , Italic
  , IFrame
  , Image
  , Input
  , Insertion
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
  , Sample
  , Script
  , Search
  , Section
  , Select
  , Slot
  , Small
  , Source
  , Span
  , Strikethrough
  , Strong
  , Style
  , Subscript
  , Summary
  , Superscript
  , Table
  , TableBody
  , TableDataCell
  , TableFooter
  , TableHeaderCell
  , TableHeader
  , TableRow
  , Template
  , TextArea
  , Time
  , Title
  , Track
  , Underline
  , UnorderedList
  , Variable
  , Video
  , WordBreakOpportunity
  ) where

import Data.HTML4.Elements.TagType qualified as TagType

-- | This type synonym represents @HTML@ for an HTML comment.
type Comment = 'TagType.Comment

-- | This type synonym represents @HTML@ for textual content within an element.
type Text = 'TagType.Text

-- | This type synonym represents @HTML@ for the @\<a>@ element.
type Anchor = 'TagType.Anchor

-- | This type synonym represents @HTML@ for the @\<abbr>@ element.
type Abbreviation = 'TagType.Abbreviation

-- | This type synonym represents @HTML@ for the @\<address>@ element.
type Address = 'TagType.Address

-- | This type synonym represents @HTML@ for the @\<area>@ element.
type Area = 'TagType.Area

-- | This type synonym represents @HTML@ for the @\<article>@ element.
type Article = 'TagType.Article

-- | This type synonym represents @HTML@ for the @\<aside>@ element.
type Aside = 'TagType.Aside

-- | This type synonym represents @HTML@ for the @\<audio>@ element.
type Audio = 'TagType.Audio

-- | This type synonym represents @HTML@ for the @\<b>@ element.
type Bold = 'TagType.Bold

-- | This type synonym represents @HTML@ for the @\<base>@ element.
type Base = 'TagType.Base

-- | This type synonym represents @HTML@ for the @\<bdi>@ element.
type BiDirectionalIsolation = 'TagType.BiDirectionalIsolation

-- | This type synonym represents @HTML@ for the @\<bdo>@ element.
type BiDirectionalOverride = 'TagType.BiDirectionalOverride

-- | This type synonym represents @HTML@ for the @\<blockquote>@ element.
type Blockquote = 'TagType.Blockquote

-- | This type synonym represents @HTML@ for the @\<body>@ element.
type Body = 'TagType.Body

-- | This type synonym represents @HTML@ for the @\<br>@ element.
type Break = 'TagType.Break

-- | This type synonym represents @HTML@ for the @\<button>@ element.
type Button = 'TagType.Button

-- | This type synonym represents @HTML@ for the @\<canvas>@ element.
type Canvas = 'TagType.Canvas

-- | This type synonym represents @HTML@ for the @\<caption>@ element.
type Caption = 'TagType.Caption

-- | This type synonym represents @HTML@ for the @\<cite>@ element.
type Cite = 'TagType.Cite

-- | This type synonym represents @HTML@ for the @\<code>@ element.
type Code = 'TagType.Code

-- | This type synonym represents @HTML@ for the @\<col>@ element.
type Column = 'TagType.Column

-- | This type synonym represents @HTML@ for the @\<colgroup>@ element.
type ColumnGroup = 'TagType.ColumnGroup

-- | This type synonym represents @HTML@ for the @\<data>@ element.
type Data = 'TagType.Data

-- | This type synonym represents @HTML@ for the @\<datalist>@ element.
type DataList = 'TagType.DataList

-- | This type synonym represents @HTML@ for the @\<dd>@ element.
type Description = 'TagType.Description

-- | This type synonym represents @HTML@ for the @\<del>@ element.
type Deletion = 'TagType.Deletion

-- | This type synonym represents @HTML@ for the @\<details>@ element.
type Details = 'TagType.Details

-- | This type synonym represents @HTML@ for the @\<dfn>@ element.
type Definition = 'TagType.Definition

-- | This type synonym represents @HTML@ for the @\<dialog>@ element.
type Dialog = 'TagType.Dialog

-- | This type synonym represents @HTML@ for the @\<div>@ element.
type Division = 'TagType.Division

-- | This type synonym represents @HTML@ for the @\<dl>@ element.
type DescriptionList = 'TagType.DescriptionList

-- | This type synonym represents @HTML@ for the @\<dt>@ element.
type DescriptionTerm = 'TagType.DescriptionTerm

-- | This type synonym represents @HTML@ for the @\<em>@ element.
type Emphasis = 'TagType.Emphasis

-- | This type synonym represents @HTML@ for the @\<embed>@ element.
type Embed = 'TagType.Embed

-- | This type synonym represents @HTML@ for the @\<fieldset>@ element.
type Fieldset = 'TagType.Fieldset

-- | This type synonym represents @HTML@ for the @\<figcaption>@ element.
type FigureCaption = 'TagType.FigureCaption

-- | This type synonym represents @HTML@ for the @\<figure>@ element.
type Figure = 'TagType.Figure

-- | This type synonym represents @HTML@ for the @\<footer>@ element.
type Footer = 'TagType.Footer

-- | This type synonym represents @HTML@ for the @\<form>@ element.
type Form = 'TagType.Form

-- | This type synonym represents @HTML@ for the @\<h1>@ element.
type H1 = 'TagType.H1

-- | This type synonym represents @HTML@ for the @\<h2>@ element.
type H2 = 'TagType.H2

-- | This type synonym represents @HTML@ for the @\<h3>@ element.
type H3 = 'TagType.H3

-- | This type synonym represents @HTML@ for the @\<h4>@ element.
type H4 = 'TagType.H4

-- | This type synonym represents @HTML@ for the @\<h5>@ element.
type H5 = 'TagType.H5

-- | This type synonym represents @HTML@ for the @\<h6>@ element.
type H6 = 'TagType.H6

-- | This type synonym represents @HTML@ for the @\<head>@ element.
type Head = 'TagType.Head

-- | This type synonym represents @HTML@ for the @\<header>@ element.
type Header = 'TagType.Header

-- | This type synonym represents @HTML@ for the @\<hgroup>@ element.
type HeadingGroup = 'TagType.HeadingGroup

-- | This type synonym represents @HTML@ for the @\<hr>@ element.
type HorizontalRule = 'TagType.HorizontalRule

{-| This type synonym represents @HTML@ for the @\<html>@ element. It will also apply the
   documentation type declaration (@DOCTYPE HTML@).
-}
type Html = 'TagType.Html

-- | This type synonym represents @HTML@ for the @\<i>@ element.
type Italic = 'TagType.Italic

-- | This type synonym represents @HTML@ for the @\<iframe>@ element.
type IFrame = 'TagType.IFrame

-- | This type synonym represents @HTML@ for the @\<img>@ element.
type Image = 'TagType.Image

-- | This type synonym represents @HTML@ for the @\<input>@ element.
type Input = 'TagType.Input

-- | This type synonym represents @HTML@ for the @\<ins>@ element.
type Insertion = 'TagType.Insertion

-- | This type synonym represents @HTML@ for the @\<kbd>@ element.
type KeyboardInput = 'TagType.KeyboardInput

-- | This type synonym represents @HTML@ for the @\<label>@ element.
type Label = 'TagType.Label

-- | This type synonym represents @HTML@ for the @\<legend>@ element.
type Legend = 'TagType.Legend

-- | This type synonym represents @HTML@ for the @\<li>@ element.
type ListItem = 'TagType.ListItem

-- | This type synonym represents @HTML@ for the @\<link>@ element.
type Link = 'TagType.Link

-- | This type synonym represents @HTML@ for the @\<main>@ element.
type Main = 'TagType.Main

-- | This type synonym represents @HTML@ for the @\<map>@ element.
type Map = 'TagType.Map

-- | This type synonym represents @HTML@ for the @\<mark>@ element.
type Mark = 'TagType.Mark

-- | This type synonym represents @HTML@ for the @\<menu>@ element.
type Menu = 'TagType.Menu

-- | This type synonym represents @HTML@ for the @\<meta>@ element.
type Meta = 'TagType.Meta

-- | This type synonym represents @HTML@ for the @\<meter>@ element.
type Meter = 'TagType.Meter

-- | This type synonym represents @HTML@ for the @\<nav>@ element.
type Nav = 'TagType.Nav

-- | This type synonym represents @HTML@ for the @\<noscript>@ element.
type NoScript = 'TagType.NoScript

-- | This type synonym represents @HTML@ for the @\<object>@ element.
type Object = 'TagType.Object

-- | This type synonym represents @HTML@ for the @\<ol>@ element.
type OrderedList = 'TagType.OrderedList

-- | This type synonym represents @HTML@ for the @\<optgroup>@ element.
type OptionGroup = 'TagType.OptionGroup

-- | This type synonym represents @HTML@ for the @\<option>@ element.
type Option = 'TagType.Option

-- | This type synonym represents @HTML@ for the @\<output>@ element.
type Output = 'TagType.Output

-- | This type synonym represents @HTML@ for the @\<p>@ element.
type Paragraph = 'TagType.Paragraph

-- | This type synonym represents @HTML@ for the @\<picture>@ element.
type Picture = 'TagType.Picture

-- | This type synonym represents @HTML@ for the @\<pre>@ element.
type PreformattedText = 'TagType.PreformattedText

-- | This type synonym represents @HTML@ for the @\<progress>@ element.
type Progress = 'TagType.Progress

-- | This type synonym represents @HTML@ for the @\<q>@ element.
type Quotation = 'TagType.Quotation

-- | This type synonym represents @HTML@ for the @\<rp>@ element.
type RubyParenthesis = 'TagType.RubyParenthesis

-- | This type synonym represents @HTML@ for the @\<rt>@ element.
type RubyText = 'TagType.RubyText

-- | This type synonym represents @HTML@ for the @\<ruby>@ element.
type Ruby = 'TagType.Ruby

-- | This type synonym represents @HTML@ for the @\<samp>@ element.
type Sample = 'TagType.Sample

-- | This type synonym represents @HTML@ for the @\<script>@ element.
type Script = 'TagType.Script

-- | This type synonym represents @HTML@ for the @\<search>@ element.
type Search = 'TagType.Search

-- | This type synonym represents @HTML@ for the @\<section>@ element.
type Section = 'TagType.Section

-- | This type synonym represents @HTML@ for the @\<select>@ element.
type Select = 'TagType.Select

-- | This type synonym represents @HTML@ for the @\<slot>@ element.
type Slot = 'TagType.Slot

-- | This type synonym represents @HTML@ for the @\<small>@ element.
type Small = 'TagType.Small

-- | This type synonym represents @HTML@ for the @\<source>@ element.
type Source = 'TagType.Source

-- | This type synonym represents @HTML@ for the @\<span>@ element.
type Span = 'TagType.Span

-- | This type synonym represents @HTML@ for the @\<s>@ element.
type Strikethrough = 'TagType.Strikethrough

-- | This type synonym represents @HTML@ for the @\<strong>@ element.
type Strong = 'TagType.Strong

-- | This type synonym represents @HTML@ for the @\<style>@ element.
type Style = 'TagType.Style

-- | This type synonym represents @HTML@ for the @\<sub>@ element.
type Subscript = 'TagType.Subscript

-- | This type synonym represents @HTML@ for the @\<summary>@ element.
type Summary = 'TagType.Summary

-- | This type synonym represents @HTML@ for the @\<sup>@ element.
type Superscript = 'TagType.Superscript

-- | This type synonym represents @HTML@ for the @\<table>@ element.
type Table = 'TagType.Table

-- | This type synonym represents @HTML@ for the @\<tbody>@ element.
type TableBody = 'TagType.TableBody

-- | This type synonym represents @HTML@ for the @\<td>@ element.
type TableDataCell = 'TagType.TableDataCell

-- | This type synonym represents @HTML@ for the @\<tfoot>@ element.
type TableFooter = 'TagType.TableFooter

-- | This type synonym represents @HTML@ for the @\<th>@ element.
type TableHeaderCell = 'TagType.TableHeaderCell

-- | This type synonym represents @HTML@ for the @\<thead>@ element.
type TableHeader = 'TagType.TableHeader

-- | This type synonym represents @HTML@ for the @\<tr>@ element.
type TableRow = 'TagType.TableRow

-- | This type synonym represents @HTML@ for the @\<template>@ element.
type Template = 'TagType.Template

-- | This type synonym represents @HTML@ for the @\<textarea>@ element.
type TextArea = 'TagType.TextArea

-- | This type synonym represents @HTML@ for the @\<time>@ element.
type Time = 'TagType.Time

-- | This type synonym represents @HTML@ for the @\<title>@ element.
type Title = 'TagType.Title

-- | This type synonym represents @HTML@ for the @\<track>@ element.
type Track = 'TagType.Track

-- | This type synonym represents @HTML@ for the @\<u>@ element.
type Underline = 'TagType.Underline

-- | This type synonym represents @HTML@ for the @\<ul>@ element.
type UnorderedList = 'TagType.UnorderedList

-- | This type synonym represents @HTML@ for the @\<var>@ element.
type Variable = 'TagType.Variable

-- | This type synonym represents @HTML@ for the @\<video>@ element.
type Video = 'TagType.Video

-- | This type synonym represents @HTML@ for the @\<wbr>@ element.
type WordBreakOpportunity = 'TagType.WordBreakOpportunity
