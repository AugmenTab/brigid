module HTML.Types.Extension
  ( Extension
  , ignore
  , ajaxHeader
  , alpineMorph
  , classTools
  , clientSideTemplates
  , debug
  , eventHeader
  , headSupport
  , includeVals
  , jsonEnc
  , idiomorph
  , loadingStates
  , methodOverride
  , morphdomSwap
  , multiSwap
  , pathDeps
  , pathParams
  , preload
  , removeMe
  , responseTargets
  , restored
  , serverSentEvents
  , webSockets
  , customExtension
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

ajaxHeader :: Extension
ajaxHeader = mkExtension AjaxHeader

alpineMorph :: Extension
alpineMorph = mkExtension AlpineMorph

classTools :: Extension
classTools = mkExtension ClassTools

clientSideTemplates :: Extension
clientSideTemplates = mkExtension ClientSideTemplates

debug :: Extension
debug = mkExtension Debug

eventHeader :: Extension
eventHeader = mkExtension EventHeader

headSupport :: Extension
headSupport = mkExtension HeadSupport

includeVals :: Extension
includeVals = mkExtension IncludeVals

jsonEnc :: Extension
jsonEnc = mkExtension JsonEnc

idiomorph :: Extension
idiomorph = mkExtension Idiomorph

loadingStates :: Extension
loadingStates = mkExtension LoadingStates

methodOverride :: Extension
methodOverride = mkExtension MethodOverride

morphdomSwap :: Extension
morphdomSwap = mkExtension MorphdomSwap

multiSwap :: Extension
multiSwap = mkExtension MultiSwap

pathDeps :: Extension
pathDeps = mkExtension PathDeps

pathParams :: Extension
pathParams = mkExtension PathParams

preload :: Extension
preload = mkExtension Preload

removeMe :: Extension
removeMe = mkExtension RemoveMe

responseTargets :: Extension
responseTargets = mkExtension ResponseTargets

restored :: Extension
restored = mkExtension Restored

serverSentEvents :: Extension
serverSentEvents = mkExtension ServerSentEvents

webSockets :: Extension
webSockets = mkExtension WebSockets

customExtension :: T.Text -> Extension
customExtension = mkExtension . CustomExtension

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
