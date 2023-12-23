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
      -> Maybe Caption
      -> [ColumnGroup]
      -> Maybe Head
      -> Either [Body] [Row Tags.Table]
      -> Maybe Foot
      -> ChildHTML parent
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
    , bodyContent    :: [Row Tags.TableBody]
    }

body :: [Attribute Tags.TableBody]
     -> [Row Tags.TableBody]
     -> Body
body = Body

unBody :: Body -> ChildHTML Tags.Table
unBody tbody =
  E.tbody
    (bodyAttributes tbody)
    (unRow <$> bodyContent tbody)

newtype Caption = Caption { unCaption :: ChildHTML Tags.Table }

caption :: [Attribute Tags.TableCaption]
        -> [ChildHTML Tags.TableCaption]
        -> Caption
caption attrs content = Caption $ E.caption attrs content

newtype ColumnGroup = ColumnGroup { unColumnGroup :: ChildHTML Tags.Table }

colgroup :: [Attribute Tags.TableColumnGroup]
         -> [ChildHTML Tags.TableColumnGroup]
         -> ColumnGroup
colgroup attrs content = ColumnGroup $ E.colgroup attrs content

data Foot =
  Foot
    { footAttributes :: [Attribute Tags.TableFoot]
    , footContent    :: [Row Tags.TableFoot]
    }

foot :: [Attribute Tags.TableFoot]
     -> [Row Tags.TableFoot]
     -> Foot
foot = Foot

unFoot :: Foot -> ChildHTML Tags.Table
unFoot tfoot =
  E.tfoot
    (footAttributes tfoot)
    (unRow <$> footContent tfoot)

data Head =
  Head
    { headAttributes :: [Attribute Tags.TableHead]
    , headContent    :: [Row Tags.TableHead]
    }

head :: [Attribute Tags.TableHead]
     -> [Row Tags.TableHead]
     -> Head
head = Head

unHead :: Head -> ChildHTML Tags.Table
unHead thead =
  E.thead
    (headAttributes thead)
    (unRow <$> headContent thead)

newtype Row parent = Row { unRow :: ChildHTML parent }

row :: ValidChild Tags.TableRow parent
    => [Attribute Tags.TableRow]
    -> [ChildHTML Tags.TableRow]
    -> Row parent
row attrs content = Row $ E.tr attrs content
