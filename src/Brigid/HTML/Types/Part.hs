module Brigid.HTML.Types.Part
  ( Part
      ( Part
      )
  , partToBytes
  , partToText
  , ExportPart
      ( ExportPart
      )
  , exportPartToBytes
  , exportPartToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

import Brigid.Internal.Render qualified as Render

newtype Part = Part T.Text

partToBytes :: Part -> LBS.ByteString
partToBytes = Render.textToBytes . partToText

partToText :: Part -> T.Text
partToText (Part part) = part

data ExportPart = ExportPart Part (Maybe T.Text)

exportPartToBytes :: ExportPart -> LBS.ByteString
exportPartToBytes = Render.textToBytes . exportPartToText

exportPartToText :: ExportPart -> T.Text
exportPartToText (ExportPart part mbExposed) =
  partToText part <> maybe "" (":" <>) mbExposed
