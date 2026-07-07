{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.HxAttributes
  ( HxAttributes
  , HxAttributesTypes
  , mkHxAttributes
  , unHxAttributes
  , hxAttributesToBytes
  , hxAttributesToBytesBuilder
  , hxAttributesToText
  , hxAttributesToTextBuilder
  , AllHxAttributes (AllHxAttributes)
  , InheritableHTMX
      ( HxBoost
      , HxConfirm
      , HxDisable
      , HxDisableElt
      , HxEncoding
      , HxExt
      , HxHeaders
      , HxInclude
      , HxIndicator
      , HxParams
      , HxPrompt
      , HxPushURL
      , HxReplaceURL
      , HxRequest
      , HxSelect
      , HxSelectOOB
      , HxSwap
      , HxSync
      , HxTarget
      , HxVals
      )
  , inheritableHTMXToBytes
  , inheritableHTMXToBytesBuilder
  , inheritableHTMXToText
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

-- | The shared value type for both @hx-disinherit@ and @hx-inherit@, which
-- take the exact same grammar: either @"*"@ (all attributes) or a
-- space-separated list of specific inheritable attribute names.
--
newtype HxAttributes =
  HxAttributes
    { unHxAttributes :: Shrubbery.Union HxAttributesTypes
    } deriving (Eq, Show)

type HxAttributesTypes =
  [ AllHxAttributes
  , NEL.NonEmpty InheritableHTMX
  ]

mkHxAttributes :: ( KnownNat branchIndex
                  , branchIndex ~ FirstIndexOf attributes HxAttributesTypes
                  )
               => attributes -> HxAttributes
mkHxAttributes =
  HxAttributes . Shrubbery.unify

hxAttributesToBytes :: HxAttributes -> LBS.ByteString
hxAttributesToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AllHxAttributes allHxAttributesToBytes
      . Shrubbery.branch @(NEL.NonEmpty InheritableHTMX) (LBS8.unwords . fmap inheritableHTMXToBytes . NEL.toList)
      $ Shrubbery.branchEnd
  ) . unHxAttributes

hxAttributesToBytesBuilder :: HxAttributes -> Builder
{-# INLINABLE hxAttributesToBytesBuilder #-}
hxAttributesToBytesBuilder = lazyByteString . hxAttributesToBytes

hxAttributesToText :: HxAttributes -> T.Text
hxAttributesToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @AllHxAttributes allHxAttributesToText
      . Shrubbery.branch @(NEL.NonEmpty InheritableHTMX) (T.unwords . fmap inheritableHTMXToText . NEL.toList)
      $ Shrubbery.branchEnd
  ) . unHxAttributes

hxAttributesToTextBuilder :: HxAttributes -> TBL.Builder
hxAttributesToTextBuilder = TBL.fromText . hxAttributesToText

data AllHxAttributes = AllHxAttributes
  deriving (Eq, Show)

allHxAttributesToBytes :: AllHxAttributes -> LBS.ByteString
allHxAttributesToBytes = const "*"

allHxAttributesToText :: AllHxAttributes -> T.Text
allHxAttributesToText = const "*"

data InheritableHTMX
  = HxBoost
  | HxConfirm
  | HxDisable
  | HxDisableElt
  | HxEncoding
  | HxExt
  | HxHeaders
  | HxInclude
  | HxIndicator
  | HxParams
  | HxPrompt
  | HxPushURL
  | HxReplaceURL
  | HxRequest
  | HxSelect
  | HxSelectOOB
  | HxSwap
  | HxSync
  | HxTarget
  | HxVals
  deriving (Bounded, Enum, Eq, Show)

inheritableHTMXToBytes :: InheritableHTMX -> LBS.ByteString
inheritableHTMXToBytes htmx =
  case htmx of
    HxBoost      -> "hx-boost"
    HxConfirm    -> "hx-confirm"
    HxDisable    -> "hx-disable"
    HxDisableElt -> "hx-disable-elt"
    HxEncoding   -> "hx-encoding"
    HxExt        -> "hx-ext"
    HxHeaders    -> "hx-headers"
    HxInclude    -> "hx-include"
    HxIndicator  -> "hx-indicator"
    HxParams     -> "hx-params"
    HxPrompt     -> "hx-prompt"
    HxPushURL    -> "hx-push-url"
    HxReplaceURL -> "hx-replace-url"
    HxRequest    -> "hx-request"
    HxSelect     -> "hx-select"
    HxSelectOOB  -> "hx-select-oob"
    HxSwap       -> "hx-swap"
    HxSync       -> "hx-sync"
    HxTarget     -> "hx-target"
    HxVals       -> "hx-vals"

inheritableHTMXToBytesBuilder :: InheritableHTMX -> Builder
{-# INLINE inheritableHTMXToBytesBuilder #-}
inheritableHTMXToBytesBuilder = lazyByteString . inheritableHTMXToBytes

inheritableHTMXToText :: InheritableHTMX -> T.Text
inheritableHTMXToText htmx =
  case htmx of
    HxBoost      -> "hx-boost"
    HxConfirm    -> "hx-confirm"
    HxDisable    -> "hx-disable"
    HxDisableElt -> "hx-disable-elt"
    HxEncoding   -> "hx-encoding"
    HxExt        -> "hx-ext"
    HxHeaders    -> "hx-headers"
    HxInclude    -> "hx-include"
    HxIndicator  -> "hx-indicator"
    HxParams     -> "hx-params"
    HxPrompt     -> "hx-prompt"
    HxPushURL    -> "hx-push-url"
    HxReplaceURL -> "hx-replace-url"
    HxRequest    -> "hx-request"
    HxSelect     -> "hx-select"
    HxSelectOOB  -> "hx-select-oob"
    HxSwap       -> "hx-swap"
    HxSync       -> "hx-sync"
    HxTarget     -> "hx-target"
    HxVals       -> "hx-vals"
