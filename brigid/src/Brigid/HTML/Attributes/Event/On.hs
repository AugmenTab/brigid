module Brigid.HTML.Attributes.Event.On
  ( AttributeTags.On, on
  ) where

import Brigid.HTML.Attributes.Event.Event (Event)
import Brigid.HTML.Attributes.Internal (Attribute (..))
import Brigid.HTML.Attributes.Tags qualified as AttributeTags
import Brigid.Types.RawJavaScript (RawJavaScript)

-- | An event attribute combinator that doesn't enforce any constraints
-- regarding which elements it can be on. This is useful for unconventional
-- uses of event attributes, like as selector mechanisms, declarative hooks, or
-- poor man's directives.
--
on :: Event -> RawJavaScript -> Attribute tag
on =
  Attr_On
