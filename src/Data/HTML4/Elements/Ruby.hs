-- This module is designed to make building Ruby symbols safer by exposing only one
-- constructor that explicitly takes the symbol and pronunciation guide, then builds
-- a fully-formed `ruby` tag around them. It is intended that the user imports it
-- qualified, optimally as `Ruby`.
module Data.HTML4.Elements.Ruby
  ( ruby
  ) where

import Data.Text qualified as T

import Data.HTML4.Attributes.Internal (Attribute)
import Data.HTML4.Elements qualified as E
import Data.HTML4.Elements.Children (ValidChild)
import Data.HTML4.Elements.Tags (Ruby)

ruby :: ValidChild Ruby parent
     => [Attribute Ruby]
     -> T.Text
     -> T.Text
     -> E.ChildHTML parent
ruby attrs symbol txt =
  E.ruby attrs
    [ E.text symbol
    , E.rp []
        [ E.text "(" ]
    , E.rt []
        [ E.text txt ]
    , E.rp []
        [ E.text ")" ]
    ]
