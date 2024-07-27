module Brigid.HTML.Types.Extension
  ( Extension
  , ignore
  , extAjaxHeader
  , extAlpineMorph
  , extClassTools
  , extClientSideTemplates
  , extDebug
  , extEventHeader
  , extHeadSupport
  , extIncludeVals
  , extJsonEnc
  , extIdiomorph
  , extLoadingStates
  , extMethodOverride
  , extMorphdomSwap
  , extMultiSwap
  , extPathDeps
  , extPathParams
  , extPreload
  , extRemoveMe
  , extResponseTargets
  , extRestored
  , extServerSentEvents
  , extWebSockets
  , extCustomExtension
  , extensionToText
  ) where

import Data.Bool qualified as B
import Data.Text qualified as T

data Extension =
  Extension
    { extensionType :: ExtensionType
    , extensionIgnored :: Bool
    }

mkExtension :: ExtensionType -> Extension
mkExtension extType =
  Extension
    { extensionType = extType
    , extensionIgnored = False
    }

ignore :: Extension -> Extension
ignore ext =
  ext { extensionIgnored = True }

extAjaxHeader :: Extension
extAjaxHeader = mkExtension AjaxHeader

extAlpineMorph :: Extension
extAlpineMorph = mkExtension AlpineMorph

extClassTools :: Extension
extClassTools = mkExtension ClassTools

extClientSideTemplates :: Extension
extClientSideTemplates = mkExtension ClientSideTemplates

extDebug :: Extension
extDebug = mkExtension Debug

extEventHeader :: Extension
extEventHeader = mkExtension EventHeader

extHeadSupport :: Extension
extHeadSupport = mkExtension HeadSupport

extIncludeVals :: Extension
extIncludeVals = mkExtension IncludeVals

extJsonEnc :: Extension
extJsonEnc = mkExtension JsonEnc

extIdiomorph :: Extension
extIdiomorph = mkExtension Idiomorph

extLoadingStates :: Extension
extLoadingStates = mkExtension LoadingStates

extMethodOverride :: Extension
extMethodOverride = mkExtension MethodOverride

extMorphdomSwap :: Extension
extMorphdomSwap = mkExtension MorphdomSwap

extMultiSwap :: Extension
extMultiSwap = mkExtension MultiSwap

extPathDeps :: Extension
extPathDeps = mkExtension PathDeps

extPathParams :: Extension
extPathParams = mkExtension PathParams

extPreload :: Extension
extPreload = mkExtension Preload

extRemoveMe :: Extension
extRemoveMe = mkExtension RemoveMe

extResponseTargets :: Extension
extResponseTargets = mkExtension ResponseTargets

extRestored :: Extension
extRestored = mkExtension Restored

extServerSentEvents :: Extension
extServerSentEvents = mkExtension ServerSentEvents

extWebSockets :: Extension
extWebSockets = mkExtension WebSockets

extCustomExtension :: T.Text -> Extension
extCustomExtension = mkExtension . CustomExtension

extensionToText :: Extension -> T.Text
extensionToText ext =
  B.bool "" "ignore:" (extensionIgnored ext)
    <> extensionTypeToText (extensionType ext)

data ExtensionType
  = AjaxHeader
  | AlpineMorph
  | ClassTools
  | ClientSideTemplates
  | Debug
  | EventHeader
  | HeadSupport
  | IncludeVals
  | JsonEnc
  | Idiomorph
  | LoadingStates
  | MethodOverride
  | MorphdomSwap
  | MultiSwap
  | PathDeps
  | PathParams
  | Preload
  | RemoveMe
  | ResponseTargets
  | Restored
  | ServerSentEvents
  | WebSockets
  | CustomExtension T.Text

extensionTypeToText :: ExtensionType -> T.Text
extensionTypeToText ext =
  case ext of
    AjaxHeader          -> "ajax-header"
    AlpineMorph         -> "alpine-morph"
    ClassTools          -> "class-tools"
    ClientSideTemplates -> "client-side-templates"
    Debug               -> "debug"
    EventHeader         -> "event-header"
    HeadSupport         -> "head-support"
    IncludeVals         -> "include-vals"
    JsonEnc             -> "json-enc"
    Idiomorph           -> "idiomorph"
    LoadingStates       -> "loading-states"
    MethodOverride      -> "method-override"
    MorphdomSwap        -> "morphdom-swap"
    MultiSwap           -> "multi-swap"
    PathDeps            -> "path-deps"
    PathParams          -> "path-params"
    Preload             -> "preload"
    RemoveMe            -> "remove-me"
    ResponseTargets     -> "response-targets"
    Restored            -> "restored"
    ServerSentEvents    -> "server-sent-events"
    WebSockets          -> "web-sockets"
    CustomExtension txt -> txt
