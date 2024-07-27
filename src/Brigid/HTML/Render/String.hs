{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HTML.Render.String
  ( renderHTML
  ) where

import Data.Text.Lazy qualified as TL

import Brigid.HTML.Elements.Internal (ChildHTML)
import Brigid.HTML.Render.Text qualified as RenderText

renderHTML :: ChildHTML parent grandparent -> String
renderHTML = TL.unpack . RenderText.renderLazyHTML
