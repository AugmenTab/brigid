{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Disinherit
  ( Disinherit
  , DisinheritTypes
  , mkDisinherit
  , unDisinherit
  , disinheritToBytes
  , disinheritToText
  , DisinheritAll (DisinheritAll)
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
  , inheritableHTMXToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

newtype Disinherit =
  Disinherit
    { unDisinherit :: Shrubbery.Union DisinheritTypes
    }

type DisinheritTypes =
  [ DisinheritAll
  , NEL.NonEmpty InheritableHTMX
  ]

mkDisinherit :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf disinherit DisinheritTypes
                )
             => disinherit -> Disinherit
mkDisinherit =
  Disinherit . Shrubbery.unify

disinheritToBytes :: Disinherit -> LBS.ByteString
disinheritToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @DisinheritAll disinheritAllToBytes
      . Shrubbery.branch @(NEL.NonEmpty InheritableHTMX) (LBS8.unwords . fmap inheritableHTMXToBytes . NEL.toList)
      $ Shrubbery.branchEnd
  ) . unDisinherit

disinheritToText :: Disinherit -> T.Text
disinheritToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @DisinheritAll disinheritAllToText
      . Shrubbery.branch @(NEL.NonEmpty InheritableHTMX) (T.unwords . fmap inheritableHTMXToText . NEL.toList)
      $ Shrubbery.branchEnd
  ) . unDisinherit

data DisinheritAll = DisinheritAll

disinheritAllToBytes :: DisinheritAll -> LBS.ByteString
disinheritAllToBytes = const "*"

disinheritAllToText :: DisinheritAll -> T.Text
disinheritAllToText = const "*"

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
    HxSync       -> "hx-swap"
    HxTarget     -> "hx-target"
    HxVals       -> "hx-vals"

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
    HxSync       -> "hx-swap"
    HxTarget     -> "hx-target"
    HxVals       -> "hx-vals"
