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
import Data.Text.Encoding qualified as TE

newtype Part = Part T.Text

partToBytes :: Part -> LBS.ByteString
partToBytes = LBS.fromStrict . TE.encodeUtf8 . partToText

partToText :: Part -> T.Text
partToText (Part part) = part

data ExportPart = ExportPart Part (Maybe T.Text)

exportPartToBytes :: ExportPart -> LBS.ByteString
exportPartToBytes = LBS.fromStrict . TE.encodeUtf8 . exportPartToText

exportPartToText :: ExportPart -> T.Text
exportPartToText (ExportPart part mbExposed) =
  partToText part <> maybe "" (":" <>) mbExposed
