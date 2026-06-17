{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.SrcsetCandidate
  ( SrcsetCandidate (..)
  , mkSrcsetCandidate
  , srcsetCandidateToBytes
  , srcsetCandidateToBytesBuilder
  , srcsetCandidateToText
  , SrcsetDescriptor
      ( SrcsetWidth
      , SrcsetDensity
      )
  ) where

import Data.ByteString.Builder (Builder, string8)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import Integer (Positive)
import GHC.TypeLits (KnownNat)
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.Number (Number, numberToBytes, numberToBytesBuilder, numberToText)
import Brigid.Internal.Render qualified as Render
import Brigid.Types.URL (URL, URLTypes, mkURL, urlToBytes, urlToBytesBuilder, urlToText)

data SrcsetCandidate =
  SrcsetCandidate
    { srcsetCandidateURL :: URL
    , srcsetCandidateDescriptor :: SrcsetDescriptor
    } deriving (Eq, Show)

mkSrcsetCandidate :: ( KnownNat branchIndex
                     , branchIndex ~ FirstIndexOf url URLTypes
                     )
                  => url -> SrcsetDescriptor -> SrcsetCandidate
mkSrcsetCandidate url descriptor =
  SrcsetCandidate
    { srcsetCandidateURL = mkURL url
    , srcsetCandidateDescriptor = descriptor
    }

srcsetCandidateToBytes :: SrcsetCandidate -> LBS.ByteString
srcsetCandidateToBytes ssc =
  LBS8.unwords
    [ urlToBytes $ srcsetCandidateURL ssc
    , srcsetDescriptorToBytes $ srcsetCandidateDescriptor ssc
    ]

srcsetCandidateToBytesBuilder :: SrcsetCandidate -> Builder
{-# INLINE srcsetCandidateToBytesBuilder #-}
srcsetCandidateToBytesBuilder ssc =
  urlToBytesBuilder (srcsetCandidateURL ssc)
    <> string8 " "
    <> srcsetDescriptorToBytesBuilder (srcsetCandidateDescriptor ssc)

srcsetCandidateToText :: SrcsetCandidate -> T.Text
srcsetCandidateToText ssc =
  T.unwords
    [ urlToText $ srcsetCandidateURL ssc
    , srcsetDescriptorToText $ srcsetCandidateDescriptor ssc
    ]

data SrcsetDescriptor
  = SrcsetWidth Positive
  | SrcsetDensity Number
  deriving (Eq, Show)

srcsetDescriptorToBytes :: SrcsetDescriptor -> LBS.ByteString
srcsetDescriptorToBytes ssd =
  case ssd of
    SrcsetWidth w -> Render.showBytes w <> "w"
    SrcsetDensity x -> numberToBytes x <> "x"

srcsetDescriptorToBytesBuilder :: SrcsetDescriptor -> Builder
{-# INLINE srcsetDescriptorToBytesBuilder #-}
srcsetDescriptorToBytesBuilder ssd =
  case ssd of
    SrcsetWidth w   -> Render.showIntegerBytesBuilder w <> string8 "w"
    SrcsetDensity x -> numberToBytesBuilder x <> string8 "x"

srcsetDescriptorToText :: SrcsetDescriptor -> T.Text
srcsetDescriptorToText ssd =
  case ssd of
    SrcsetWidth w -> Render.showText w <> "w"
    SrcsetDensity x -> numberToText x <> "x"
