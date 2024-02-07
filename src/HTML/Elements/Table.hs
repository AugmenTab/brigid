-- This module is designed to make building tables safer by restricting the
-- arguments based on the permitted content for a table as per the HTML
-- documentation. It is intended that the user imports it qualified, optimally
-- as `Table`.
module HTML.Elements.Table
  ( table
  , Body, body
  , Caption, caption
  , ColumnGroup, colgroup
  , Foot, foot
  , Head, head
  , Row, row
  ) where

import Prelude hiding (head)
import Data.Maybe (maybeToList)

import HTML.Attributes.Internal (Attribute)
import HTML.Elements qualified as E
import HTML.Elements.Children (ValidChild)
import HTML.Elements.Tags qualified as Tags
import HTML.Elements.Internal (ChildHTML(..))

table :: ValidChild Tags.Table parent grandparent
      => [Attribute Tags.Table]
      -> Maybe (Caption parent)
      -> [ColumnGroup parent]
      -> Maybe Head
      -> Either [Body] [Row Tags.Table parent]
      -> Maybe Foot
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

data Body =
  Body
    { bodyAttributes :: [Attribute Tags.TableBody]
    , bodyContent    :: [Row Tags.TableBody Tags.Table]
    }

body :: [Attribute Tags.TableBody]
     -> [Row Tags.TableBody Tags.Table]
     -> Body
body = Body

unBody :: Body -> ChildHTML Tags.Table grandparent
unBody tbody =
  E.tbody
    (bodyAttributes tbody)
    (unRow <$> bodyContent tbody)

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

data Foot =
  Foot
    { footAttributes :: [Attribute Tags.TableFoot]
    , footContent    :: [Row Tags.TableFoot Tags.Table]
    }

foot :: [Attribute Tags.TableFoot]
     -> [Row Tags.TableFoot Tags.Table]
     -> Foot
foot = Foot

unFoot :: Foot -> ChildHTML Tags.Table grandparent
unFoot tfoot =
  E.tfoot
    (footAttributes tfoot)
    (unRow <$> footContent tfoot)

data Head =
  Head
    { headAttributes :: [Attribute Tags.TableHead]
    , headContent    :: [Row Tags.TableHead Tags.Table]
    }

head :: [Attribute Tags.TableHead]
     -> [Row Tags.TableHead Tags.Table]
     -> Head
head = Head

unHead :: Head -> ChildHTML Tags.Table grandparent
unHead thead =
  E.thead
    (headAttributes thead)
    (unRow <$> headContent thead)

newtype Row parent grandparent =
  Row { unRow :: ChildHTML parent grandparent }

row :: ValidChild Tags.TableRow parent grandparent
    => [Attribute Tags.TableRow]
    -> [ChildHTML Tags.TableRow parent]
    -> Row parent grandparent
row attrs content = Row $ E.tr attrs content
