module Brigid.HTML.Attributes.Other
  ( hyperscript
  ) where

import Brigid.HTML.Attributes.Internal (Attribute(..))
import Brigid.HTML.Types qualified as Types

hyperscript :: Types.HyperScript -> Attribute tag
hyperscript = Attr_HyperScript
