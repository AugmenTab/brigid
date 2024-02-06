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

table :: ValidChild Tags.Table parent
      => [Attribute Tags.Table]
      -> Maybe (Caption grandparent)
      -> [ColumnGroup grandparent]
      -> Maybe (Head grandparent)
      -> Either [Body grandparent] [Row Tags.Table grandparent]
      -> Maybe (Foot grandparent)
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

data Body grandparent =
  Body
    { bodyAttributes :: [Attribute Tags.TableBody]
    , bodyContent    :: [Row Tags.TableBody grandparent]
    }

body :: [Attribute Tags.TableBody]
     -> [Row Tags.TableBody grandparent]
     -> Body grandparent
body = Body

unBody :: Body grandparent -> ChildHTML Tags.Table grandparent
unBody tbody =
  E.tbody
    (bodyAttributes tbody)
    (unRow <$> bodyContent tbody)

newtype Caption grandparent =
  Caption { unCaption :: ChildHTML Tags.Table grandparent }

caption :: [Attribute Tags.TableCaption]
        -> [ChildHTML Tags.TableCaption grandparent]
        -> Caption grandparent
caption attrs content = Caption $ E.caption attrs content

newtype ColumnGroup grandparent =
  ColumnGroup { unColumnGroup :: ChildHTML Tags.Table grandparent }

colgroup :: [Attribute Tags.TableColumnGroup]
         -> [ChildHTML Tags.TableColumnGroup grandparent]
         -> ColumnGroup grandparent
colgroup attrs content = ColumnGroup $ E.colgroup attrs content

data Foot grandparent =
  Foot
    { footAttributes :: [Attribute Tags.TableFoot]
    , footContent    :: [Row Tags.TableFoot grandparent]
    }

foot :: [Attribute Tags.TableFoot]
     -> [Row Tags.TableFoot grandparent]
     -> Foot grandparent
foot = Foot

unFoot :: Foot grandparent -> ChildHTML Tags.Table grandparent
unFoot tfoot =
  E.tfoot
    (footAttributes tfoot)
    (unRow <$> footContent tfoot)

data Head grandparent =
  Head
    { headAttributes :: [Attribute Tags.TableHead]
    , headContent    :: [Row Tags.TableHead grandparent]
    }

head :: [Attribute Tags.TableHead]
     -> [Row Tags.TableHead grandparent]
     -> Head grandparent
head = Head

unHead :: Head grandparent -> ChildHTML Tags.Table grandparent
unHead thead =
  E.thead
    (headAttributes thead)
    (unRow <$> headContent thead)

newtype Row parent grandparent =
  Row { unRow :: ChildHTML parent grandparent }

row :: ValidChild Tags.TableRow parent
    => [Attribute Tags.TableRow]
    -> [ChildHTML Tags.TableRow grandparent]
    -> Row parent grandparent
row attrs content = Row $ E.tr attrs content
