{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HXML.Render.String
  ( renderHTML
  ) where

import Data.Text.Lazy qualified as TL

import Brigid.HXML.Elements.Internal (ChildHXML)
import Brigid.HXML.Render.Text qualified as RenderText

renderHTML :: ChildHXML parent -> String
renderHTML = TL.unpack . RenderText.renderLazyHXML
