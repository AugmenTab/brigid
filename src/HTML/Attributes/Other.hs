module HTML.Attributes.Other
  ( hyperscript
  ) where

import HTML.Attributes.Internal (Attribute(..))
import HTML.Types qualified as Types

hyperscript :: Types.HyperScript -> Attribute tag
hyperscript = Attr_HyperScript
