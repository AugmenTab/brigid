{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module HTML.Render.String
  ( renderHTML
  ) where

import Data.Text.Lazy qualified as TL

import HTML.Elements.Internal (ChildHTML)
import HTML.Render.Text qualified as RenderText

renderHTML :: ChildHTML parent grandparent -> String
renderHTML = TL.unpack . RenderText.renderLazyHTML
