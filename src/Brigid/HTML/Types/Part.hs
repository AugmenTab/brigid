module Brigid.HTML.Types.Part
  ( Part
      ( Part
      )
  , partToBytes
  , partToBytesBuilder
  , partToText
  , ExportPart
      ( ExportPart
      )
  , exportPartToBytes
  , exportPartToBytesBuilder
  , exportPartToText
  ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Brigid.Internal.Render qualified as Render

newtype Part = Part T.Text
  deriving (Eq, Show)

partToBytes :: Part -> LBS.ByteString
partToBytes = Render.textToLazyBytes . partToText

partToBytesBuilder :: Part -> Builder
partToBytesBuilder = TE.encodeUtf8Builder . partToText

partToText :: Part -> T.Text
partToText (Part part) = part

data ExportPart = ExportPart Part (Maybe T.Text)
  deriving (Eq, Show)

exportPartToBytes :: ExportPart -> LBS.ByteString
exportPartToBytes = Render.textToLazyBytes . exportPartToText

exportPartToBytesBuilder :: ExportPart -> Builder
exportPartToBytesBuilder = TE.encodeUtf8Builder . exportPartToText

exportPartToText :: ExportPart -> T.Text
exportPartToText (ExportPart part mbExposed) =
  partToText part <> maybe "" (":" <>) mbExposed
