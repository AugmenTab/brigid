module Brigid.HTML.Types.Integrity
  ( IntegrityEncoding
      ( SHA256
      , SHA384
      , SHA512
      )
  , integrityToBytes
  , integrityToBytesBuilder
  , integrityToText
  , integrityToTextBuilder
  ) where

import Crypto.Hash qualified as Hash
import Crypto.Hash.Algorithms qualified as Algorithms
import Data.ByteArray.Encoding (convertToBase, Base (Base64))
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

import Brigid.Internal.Render qualified as Render

data IntegrityEncoding
  = SHA256
  | SHA384
  | SHA512
  deriving (Bounded, Enum, Eq, Show)

integrityEncodingToBytes :: IntegrityEncoding -> LBS.ByteString
integrityEncodingToBytes sha =
  case sha of
    SHA256 -> "sha256"
    SHA384 -> "sha384"
    SHA512 -> "sha512"

integrityEncodingToText :: IntegrityEncoding -> T.Text
integrityEncodingToText sha =
  case sha of
    SHA256 -> "sha256"
    SHA384 -> "sha384"
    SHA512 -> "sha512"

integrityToBytes :: IntegrityEncoding -> BS.ByteString -> LBS.ByteString
integrityToBytes sha content =
  mconcat
    [ integrityEncodingToBytes sha
    , "-"
    , Render.bytesToLazyBytes $
        case sha of
          SHA256 -> convertToBase Base64 (Hash.hash content :: Hash.Digest Hash.SHA256)
          SHA384 -> convertToBase Base64 (Hash.hash content :: Hash.Digest Hash.SHA384)
          SHA512 -> convertToBase Base64 (Hash.hash content :: Hash.Digest Hash.SHA512)
    ]

integrityToBytesBuilder :: IntegrityEncoding -> BS.ByteString -> Builder
{-# INLINE integrityToBytesBuilder #-}
integrityToBytesBuilder sha content = lazyByteString (integrityToBytes sha content)

integrityToText :: IntegrityEncoding -> BS.ByteString -> T.Text
integrityToText sha content =
  mconcat
    [ integrityEncodingToText sha
    , "-"
    , Render.bytesToText $
        case sha of
          SHA256 -> convertToBase Base64 (Hash.hash content :: Hash.Digest Algorithms.SHA256)
          SHA384 -> convertToBase Base64 (Hash.hash content :: Hash.Digest Algorithms.SHA384)
          SHA512 -> convertToBase Base64 (Hash.hash content :: Hash.Digest Algorithms.SHA512)
    ]

integrityToTextBuilder :: IntegrityEncoding -> BS.ByteString -> TBL.Builder
integrityToTextBuilder sha content = TBL.fromText (integrityToText sha content)
