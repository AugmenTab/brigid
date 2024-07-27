module Brigid.HTML.Types.WebsocketBinaryType
  ( WebsocketBinaryType
      ( Blob
      , ArrayBuffer
      )
  , websocketBinaryTypeToBytes
  , websocketBinaryTypeToText
  , websocketBinaryTypeFromText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data WebsocketBinaryType
  = Blob
  | ArrayBuffer

websocketBinaryTypeToBytes :: WebsocketBinaryType -> LBS.ByteString
websocketBinaryTypeToBytes wbt =
  case wbt of
    Blob        -> "blob"
    ArrayBuffer -> "arraybuffer"

websocketBinaryTypeToText :: WebsocketBinaryType -> T.Text
websocketBinaryTypeToText wbt =
  case wbt of
    Blob        -> "blob"
    ArrayBuffer -> "arraybuffer"

websocketBinaryTypeFromText :: T.Text -> Either String WebsocketBinaryType
websocketBinaryTypeFromText txt =
  case txt of
    "blob"        -> Right Blob
    "arraybuffer" -> Right ArrayBuffer
    _             -> Left $ "Unknown WebsocketBinaryType: " <> T.unpack txt
