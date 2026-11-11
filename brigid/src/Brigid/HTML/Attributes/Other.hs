module Brigid.HTML.Attributes.Other
  ( AttributeTags.HyperScript, hyperscript
  ) where

import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Attributes.Tags qualified as AttributeTags
import Brigid.HTML.Types qualified as Types

hyperscript :: Types.HyperScript -> Attribute tag
hyperscript = Attr_HyperScript
