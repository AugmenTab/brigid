-- | This module contains a number of the safe element modules designed to
-- enforce compliance with the HTML spec, either by restricting arguments or
-- offering smart constructors to handle the building of complex or restrictive
-- element/attribute combinations.
--
-- It is intended that the user import it qualified, preferably as `Safe`. Care
-- has been taken to prevent name collisions.
--
module Brigid.HTML.Elements.Safe
  ( module Export
  ) where

import Brigid.HTML.Elements.Safe.Meta as Export
import Brigid.HTML.Elements.Safe.Ruby as Export
import Brigid.HTML.Elements.Safe.Script as Export
import Brigid.HTML.Elements.Safe.Table as Export
