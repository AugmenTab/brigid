-- This module is designed to make building tables safer by restricting the
-- arguments based on the permitted content for a table as per the HTML
-- documentation.
--
module Brigid.HTML.Elements.Safe.Table
  ( table
  , TableBody, tbody
  , Caption, caption
  , ColumnGroup, colgroup
  , TableFoot, tfoot
  , TableHead, thead
  , TableRow, tr
  ) where

import Prelude hiding (head)
import Data.Maybe (maybeToList)

import Brigid.HTML.Attributes.Internal (Attribute)
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Elements.Internal (ChildHTML(..))

table :: ValidChild Tags.Table parent grandparent
      => [Attribute Tags.Table]
      -> Maybe (Caption parent)
      -> [ColumnGroup parent]
      -> Maybe TableHead
      -> Either [TableBody] [TableRow Tags.Table parent]
      -> Maybe TableFoot
      -> ChildHTML parent grandparent
table attrs mbCaption colgroups mbHead eiBodiesRows mbFoot =
  E.table attrs $
    concat
      [ maybeToList $ unCaption <$> mbCaption
      , unColumnGroup <$> colgroups
      , maybeToList $ unHead <$> mbHead
      , either (fmap unBody) (fmap unRow) eiBodiesRows
      , maybeToList $ unFoot <$> mbFoot
      ]

data TableBody =
  TableBody
    { bodyAttributes :: [Attribute Tags.TableBody]
    , bodyContent    :: [TableRow Tags.TableBody Tags.Table]
    }

tbody :: [Attribute Tags.TableBody]
      -> [TableRow Tags.TableBody Tags.Table]
      -> TableBody
tbody = TableBody

unBody :: TableBody -> ChildHTML Tags.Table grandparent
unBody body =
  E.tbody
    (bodyAttributes body)
    (unRow <$> bodyContent body)

newtype Caption grandparent =
  Caption { unCaption :: ChildHTML Tags.Table grandparent }

caption :: [Attribute Tags.TableCaption]
        -> [ChildHTML Tags.TableCaption Tags.Table]
        -> Caption grandparent
caption attrs content = Caption $ E.caption attrs content

newtype ColumnGroup grandparent =
  ColumnGroup { unColumnGroup :: ChildHTML Tags.Table grandparent }

colgroup :: [Attribute Tags.TableColumnGroup]
         -> [ChildHTML Tags.TableColumnGroup Tags.Table]
         -> ColumnGroup grandparent
colgroup attrs content = ColumnGroup $ E.colgroup attrs content

data TableFoot =
  TableFoot
    { footAttributes :: [Attribute Tags.TableFoot]
    , footContent    :: [TableRow Tags.TableFoot Tags.Table]
    }

tfoot :: [Attribute Tags.TableFoot]
      -> [TableRow Tags.TableFoot Tags.Table]
      -> TableFoot
tfoot = TableFoot

unFoot :: TableFoot -> ChildHTML Tags.Table grandparent
unFoot foot =
  E.tfoot
    (footAttributes foot)
    (unRow <$> footContent foot)

data TableHead =
  TableHead
    { headAttributes :: [Attribute Tags.TableHead]
    , headContent    :: [TableRow Tags.TableHead Tags.Table]
    }

thead :: [Attribute Tags.TableHead]
      -> [TableRow Tags.TableHead Tags.Table]
      -> TableHead
thead = TableHead

unHead :: TableHead -> ChildHTML Tags.Table grandparent
unHead head =
  E.thead
    (headAttributes head)
    (unRow <$> headContent head)

newtype TableRow parent grandparent =
  TableRow { unRow :: ChildHTML parent grandparent }

tr :: ValidChild Tags.TableRow parent grandparent
   => [Attribute Tags.TableRow]
   -> [ChildHTML Tags.TableRow parent]
   -> TableRow parent grandparent
tr attrs content = TableRow $ E.tr attrs content
