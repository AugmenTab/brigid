module Data.HTML.Elements.Tags
  ( Anchor
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
  , Comment
  , TextContent
  ) where

-- Other elements that are in the HTML 5 spec, but not in Blaze:
--   - bdi
--   - data
--   - dialog
--   - picture
--   - s
--   - search
--   - slot
--   - svg (maybe in the Blaze SVG library?)
--   - template
--
-- These could perhaps be added in a PR to the `blaze-html` repo.

-- | This data tag represents the @\<a>@ element.
data Anchor

-- | This data tag represents the @\<abbr>@ element.
data Abbreviation

-- | This data tag represents the @\<address>@ element.
data Address

-- | This data tag represents the @\<area>@ element.
data Area

-- | This data tag represents the @\<article>@ element.
data Article

-- | This data tag represents the @\<aside>@ element.
data Aside

-- | This data tag represents the @\<audio>@ element.
data Audio

-- | This data tag represents the @\<b>@ element.
data Bold

-- | This data tag represents the @\<base>@ element.
data Base

-- | This data tag represents the @\<bdi>@ element.
data BiDirectionalIsolation

-- | This data tag represents the @\<bdo>@ element.
data BiDirectionalOverride

-- | This data tag represents the @\<blockquote>@ element.
data Blockquote

-- | This data tag represents the @\<body>@ element.
data Body

-- | This data tag represents the @\<br>@ element.
data Break

-- | This data tag represents the @\<button>@ element.
data Button

-- | This data tag represents the @\<canvas>@ element.
data Canvas

-- | This data tag represents the @\<caption>@ element.
data Caption

-- | This data tag represents the @\<cite>@ element.
data Cite

-- | This data tag represents the @\<code>@ element.
data Code

-- | This data tag represents the @\<col>@ element.
data Column

-- | This data tag represents the @\<colgroup>@ element.
data ColumnGroup

-- | This data tag represents the @\<data>@ element.
data Data

-- | This data tag represents the @\<datalist>@ element.
data DataList

-- | This data tag represents the @\<dd>@ element.
data Description

-- | This data tag represents the @\<del>@ element.
data Deletion

-- | This data tag represents the @\<details>@ element.
data Details

-- | This data tag represents the @\<dfn>@ element.
data Definition

-- | This data tag represents the @\<dialog>@ element.
data Dialog

-- | This data tag represents the @\<div>@ element.
data Division

-- | This data tag represents the @\<dl>@ element.
data DescriptionList

-- | This data tag represents the @\<dt>@ element.
data DescriptionTerm

-- | This data tag represents the @\<em>@ element.
data Emphasis

-- | This data tag represents the @\<embed>@ element.
data Embed

-- | This data tag represents the @\<fieldset>@ element.
data Fieldset

-- | This data tag represents the @\<figcaption>@ element.
data FigureCaption

-- | This data tag represents the @\<figure>@ element.
data Figure

-- | This data tag represents the @\<footer>@ element.
data Footer

-- | This data tag represents the @\<form>@ element.
data Form

-- | This data tag represents the @\<h1>@ element.
data H1

-- | This data tag represents the @\<h2>@ element.
data H2

-- | This data tag represents the @\<h3>@ element.
data H3

-- | This data tag represents the @\<h4>@ element.
data H4

-- | This data tag represents the @\<h5>@ element.
data H5

-- | This data tag represents the @\<h6>@ element.
data H6

-- | This data tag represents the @\<head>@ element.
data Head

-- | This data tag represents the @\<header>@ element.
data Header

-- | This data tag represents the @\<hgroup>@ element.
data HeadingGroup

-- | This data tag represents the @\<hr>@ element.
data HorizontalRule

{-| This data tag represents the @\<html>@ element. It will also apply the
   documentation type declaration (@DOCTYPE HTML@).
-}
data Html

-- | This data tag represents the @\<i>@ element.
data Italic

-- | This data tag represents the @\<iframe>@ element.
data IFrame

-- | This data tag represents the @\<img>@ element.
data Image

-- | This data tag represents the @\<input>@ element.
data Input

-- | This data tag represents the @\<ins>@ element.
data Insertion

-- | This data tag represents the @\<kbd>@ element.
data KeyboardInput

-- | This data tag represents the @\<label>@ element.
data Label

-- | This data tag represents the @\<legend>@ element.
data Legend

-- | This data tag represents the @\<li>@ element.
data ListItem

-- | This data tag represents the @\<link>@ element.
data Link

-- | This data tag represents the @\<main>@ element.
data Main

-- | This data tag represents the @\<map>@ element.
data Map

-- | This data tag represents the @\<mark>@ element.
data Mark

-- | This data tag represents the @\<menu>@ element.
data Menu

-- | This data tag represents the @\<meta>@ element.
data Meta

-- | This data tag represents the @\<meter>@ element.
data Meter

-- | This data tag represents the @\<nav>@ element.
data Nav

-- | This data tag represents the @\<noscript>@ element.
data NoScript

-- | This data tag represents the @\<object>@ element.
data Object

-- | This data tag represents the @\<ol>@ element.
data OrderedList

-- | This data tag represents the @\<optgroup>@ element.
data OptionGroup

-- | This data tag represents the @\<option>@ element.
data Option

-- | This data tag represents the @\<output>@ element.
data Output

-- | This data tag represents the @\<p>@ element.
data Paragraph

-- | This data tag represents the @\<picture>@ element.
data Picture

-- | This data tag represents the @\<pre>@ element.
data PreformattedText

-- | This data tag represents the @\<progress>@ element.
data Progress

-- | This data tag represents the @\<q>@ element.
data Quotation

-- | This data tag represents the @\<rp>@ element.
data RubyParenthesis

-- | This data tag represents the @\<rt>@ element.
data RubyText

-- | This data tag represents the @\<ruby>@ element.
data Ruby

-- | This data tag represents the @\<samp>@ element.
data Sample

-- | This data tag represents the @\<script>@ element.
data Script

-- | This data tag represents the @\<search>@ element.
data Search

-- | This data tag represents the @\<section>@ element.
data Section

-- | This data tag represents the @\<select>@ element.
data Select

-- | This data tag represents the @\<slot>@ element.
data Slot

-- | This data tag represents the @\<small>@ element.
data Small

-- | This data tag represents the @\<source>@ element.
data Source

-- | This data tag represents the @\<span>@ element.
data Span

-- | This data tag represents the @\<s>@ element.
data Strikethrough

-- | This data tag represents the @\<strong>@ element.
data Strong

-- | This data tag represents the @\<style>@ element.
data Style

-- | This data tag represents the @\<sub>@ element.
data Subscript

-- | This data tag represents the @\<summary>@ element.
data Summary

-- | This data tag represents the @\<sup>@ element.
data Superscript

-- | This data tag represents the @\<table>@ element.
data Table

-- | This data tag represents the @\<tbody>@ element.
data TableBody

-- | This data tag represents the @\<td>@ element.
data TableDataCell

-- | This data tag represents the @\<tfoot>@ element.
data TableFooter

-- | This data tag represents the @\<th>@ element.
data TableHeaderCell

-- | This data tag represents the @\<thead>@ element.
data TableHeader

-- | This data tag represents the @\<tr>@ element.
data TableRow

-- | This data tag represents the @\<template>@ element.
data Template

-- | This data tag represents the @\<textarea>@ element.
data TextArea

-- | This data tag represents the @\<time>@ element.
data Time

-- | This data tag represents the @\<title>@ element.
data Title

-- | This data tag represents the @\<track>@ element.
data Track

-- | This data tag represents the @\<u>@ element.
data Underline

-- | This data tag represents the @\<ul>@ element.
data UnorderedList

-- | This data tag represents the @\<var>@ element.
data Variable

-- | This data tag represents the @\<video>@ element.
data Video

-- | This data tag represents the @\<wbr>@ element.
data WordBreakOpportunity

-- | This data tag represents an HTML comment.
data Comment

-- | This data tag represents textual content within an element.
data TextContent
