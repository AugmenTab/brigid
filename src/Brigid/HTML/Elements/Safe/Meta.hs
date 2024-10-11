-- This module is designed to make building meta tags safer by exposing only one
-- combinator that explicitly takes the metadata type, which itself can only
-- contain appropriate values for that metadata type SPECIFICALLY for the meta
-- tag context.
--
module Brigid.HTML.Elements.Safe.Meta
  ( meta
  , Metadata
      -- ( Charset
      -- , HttpEquiv
      -- , Itemprop
      -- , Name
      -- )
  ) where

-- import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Elements qualified as E

data Metadata
  -- = Charset
  -- | HttpEquiv
  -- | Itemprop
  -- | Name

meta :: Metadata -> E.ChildHTML E.Head E.Html
meta _metadata =
  E.meta []
    -- case metadata of
    --   Charset -> [ A.charset UTF8 ]
