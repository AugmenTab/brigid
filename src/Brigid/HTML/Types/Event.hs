{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Event
  ( Event
  , mkEvent
  , unEvent
  , EventTypes
  , eventToBytes
  , eventToText
  , hxOnEventBytes
  , hxOnEventText
  , HTMLEvent
      ( ClickEvent
      , DblClickEvent
      , MouseDownEvent
      , MouseUpEvent
      , MouseOverEvent
      , MouseMoveEvent
      , MouseOutEvent
      , DragStartEvent
      , DragEvent
      , DragEnterEvent
      , DragLeaveEvent
      , DragOverEvent
      , DropEvent
      , DragEndEvent
      , KeyDownEvent
      , KeyPressEvent
      , KeyUpEvent
      , LoadEvent
      , UnloadEvent
      , AbortEvent
      , ErrorEvent
      , ResizeEvent
      , ScrollEvent
      , SelectEvent
      , ChangeEvent
      , SubmitEvent
      , ResetEvent
      , FocusEvent
      , BlurEvent
      , FocusInEvent
      , FocusOutEvent
      , DOMActivateEvent
      , DOMSubtreeModifiedEvent
      , DOMNodeInsertedEvent
      , DOMNodeRemovedEvent
      , DOMNodeRemovedFromDocumentEvent
      , DOMNodeInsertedIntoDocumentEvent
      , DOMAttrModifiedEvent
      , DOMCharacterDataModifiedEvent
      , LoadStartEvent
      , ProgressEvent
      , ProgressErrorEvent
      , ProgressAbortEvent
      , ProgressLoadEvent
      , ProgressLoadEndEvent
      )
  , htmlEventToBytes
  , htmlEventToText
  , TouchEvent
      ( TouchStart
      , TouchEnd
      , TouchMove
      , TouchEnter
      , TouchLeave
      , TouchCancel
      )
  , touchEventToBytes
  , touchEventToText
  , HtmxEvent
      ( HtmxAbort
      ,  HtmxAfterOnLoad
      ,  HtmxAfterProcessNode
      ,  HtmxAfterRequest
      ,  HtmxAfterSettle
      ,  HtmxAfterSwap
      ,  HtmxBeforeCleanupElement
      ,  HtmxBeforeOnLoad
      ,  HtmxBeforeProcessNode
      ,  HtmxBeforeRequest
      ,  HtmxBeforeSwap
      ,  HtmxBeforeSend
      ,  HtmxConfigRequest
      ,  HtmxConfirm
      ,  HtmxHistoryCacheError
      ,  HtmxHistoryCacheMiss
      ,  HtmxHistoryCacheMissError
      ,  HtmxHistoryCacheMissLoad
      ,  HtmxHistoryRestore
      ,  HtmxBeforeHistorySave
      ,  HtmxLoad
      ,  HtmxNoSSESourceError
      ,  HtmxOnLoadError
      ,  HtmxOOBAfterSwap
      ,  HtmxOOBBeforeSwap
      ,  HtmxOOBErrorNoTarget
      ,  HtmxPrompt
      ,  HtmxPushedIntoHistory
      ,  HtmxResponseError
      ,  HtmxSendError
      ,  HtmxSSEError
      ,  HtmxSSEOpen
      ,  HtmxSwapError
      ,  HtmxTargetError
      ,  HtmxTimeout
      ,  HtmxValidate
      ,  HtmxValidationFailed
      ,  HtmxValidationHalted
      ,  HtmxXHRAbort
      ,  HtmxXHRLoadEnd
      ,  HtmxXHRLoadStart
      ,  HtmxXHRProgress
      )
  , htmxEventToBytes
  , htmxEventToText
  , TriggerLoad (TriggerLoad)
  , triggerLoadToBytes
  , triggerLoadToText
  , TriggerRevealed (TriggerRevealed)
  , triggerRevealedToBytes
  , triggerRevealedToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

newtype Event =
  Event
    { unEvent :: Shrubbery.Union EventTypes
    }

type EventTypes =
  [ HTMLEvent
  , TouchEvent
  , HtmxEvent
  ]

mkEvent :: ( KnownNat branchIndex
           , branchIndex ~ FirstIndexOf eventType EventTypes
           )
        => eventType -> Event
mkEvent = Event . Shrubbery.unify

eventToBytes :: Event -> LBS.ByteString
eventToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @HTMLEvent htmlEventToBytes
      . Shrubbery.branch @TouchEvent touchEventToBytes
      . Shrubbery.branch @HtmxEvent htmxEventToBytes
      $ Shrubbery.branchEnd
  ) . unEvent

eventToText :: Event -> T.Text
eventToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @HTMLEvent htmlEventToText
      . Shrubbery.branch @TouchEvent touchEventToText
      . Shrubbery.branch @HtmxEvent htmxEventToText
      $ Shrubbery.branchEnd
  ) . unEvent

hxOnEventBytes :: Event -> LBS.ByteString
hxOnEventBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @HTMLEvent (LBS8.cons '-' . htmlEventToBytes)
      . Shrubbery.branch @TouchEvent (LBS8.cons '-' . touchEventToBytes)
      . Shrubbery.branch @HtmxEvent (("--" <>) . htmxEventToBytes)
      $ Shrubbery.branchEnd
  ) . unEvent

hxOnEventText :: Event -> T.Text
hxOnEventText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @HTMLEvent (T.cons '-' . htmlEventToText)
      . Shrubbery.branch @TouchEvent (T.cons '-' . touchEventToText)
      . Shrubbery.branch @HtmxEvent (("--" <>) . htmxEventToText)
      $ Shrubbery.branchEnd
  ) . unEvent

data HTMLEvent
  = ClickEvent
  | DblClickEvent
  | MouseDownEvent
  | MouseUpEvent
  | MouseOverEvent
  | MouseMoveEvent
  | MouseOutEvent
  | DragStartEvent
  | DragEvent
  | DragEnterEvent
  | DragLeaveEvent
  | DragOverEvent
  | DropEvent
  | DragEndEvent
  | KeyDownEvent
  | KeyPressEvent
  | KeyUpEvent
  | LoadEvent
  | UnloadEvent
  | AbortEvent
  | ErrorEvent
  | ResizeEvent
  | ScrollEvent
  | SelectEvent
  | ChangeEvent
  | SubmitEvent
  | ResetEvent
  | FocusEvent
  | BlurEvent
  | FocusInEvent
  | FocusOutEvent
  | DOMActivateEvent
  | DOMSubtreeModifiedEvent
  | DOMNodeInsertedEvent
  | DOMNodeRemovedEvent
  | DOMNodeRemovedFromDocumentEvent
  | DOMNodeInsertedIntoDocumentEvent
  | DOMAttrModifiedEvent
  | DOMCharacterDataModifiedEvent
  | LoadStartEvent
  | ProgressEvent
  | ProgressErrorEvent
  | ProgressAbortEvent
  | ProgressLoadEvent
  | ProgressLoadEndEvent

htmlEventToBytes :: HTMLEvent -> LBS.ByteString
htmlEventToBytes event =
  case event of
    ClickEvent                       -> "click"
    DblClickEvent                    -> "dblclick"
    MouseDownEvent                   -> "mousedown"
    MouseUpEvent                     -> "mouseup"
    MouseOverEvent                   -> "mouseover"
    MouseMoveEvent                   -> "mousemove"
    MouseOutEvent                    -> "mouseout"
    DragStartEvent                   -> "dragstart"
    DragEvent                        -> "drag"
    DragEnterEvent                   -> "dragenter"
    DragLeaveEvent                   -> "dragleave"
    DragOverEvent                    -> "dragover"
    DropEvent                        -> "drop"
    DragEndEvent                     -> "dragend"
    KeyDownEvent                     -> "keydown"
    KeyPressEvent                    -> "keypress"
    KeyUpEvent                       -> "keyup"
    LoadEvent                        -> "load"
    UnloadEvent                      -> "unload"
    AbortEvent                       -> "abort"
    ErrorEvent                       -> "error"
    ResizeEvent                      -> "resize"
    ScrollEvent                      -> "scroll"
    SelectEvent                      -> "select"
    ChangeEvent                      -> "change"
    SubmitEvent                      -> "submit"
    ResetEvent                       -> "reset"
    FocusEvent                       -> "focus"
    BlurEvent                        -> "blur"
    FocusInEvent                     -> "focusin"
    FocusOutEvent                    -> "focusout"
    DOMActivateEvent                 -> "DOMActivate"
    DOMSubtreeModifiedEvent          -> "DOMSubtreeModified"
    DOMNodeInsertedEvent             -> "DOMNodeInserted"
    DOMNodeRemovedEvent              -> "DOMNodeRemoved"
    DOMNodeRemovedFromDocumentEvent  -> "DOMNodeRemovedFromDocument"
    DOMNodeInsertedIntoDocumentEvent -> "DOMNodeInsertedIntoDocument"
    DOMAttrModifiedEvent             -> "DOMAttrModified"
    DOMCharacterDataModifiedEvent    -> "DOMCharacterDataModified"
    LoadStartEvent                   -> "loadstart"
    ProgressEvent                    -> "progress"
    ProgressErrorEvent               -> "error"
    ProgressAbortEvent               -> "abort"
    ProgressLoadEvent                -> "load"
    ProgressLoadEndEvent             -> "loadend"

htmlEventToText :: HTMLEvent -> T.Text
htmlEventToText event =
  case event of
    ClickEvent                       -> "click"
    DblClickEvent                    -> "dblclick"
    MouseDownEvent                   -> "mousedown"
    MouseUpEvent                     -> "mouseup"
    MouseOverEvent                   -> "mouseover"
    MouseMoveEvent                   -> "mousemove"
    MouseOutEvent                    -> "mouseout"
    DragStartEvent                   -> "dragstart"
    DragEvent                        -> "drag"
    DragEnterEvent                   -> "dragenter"
    DragLeaveEvent                   -> "dragleave"
    DragOverEvent                    -> "dragover"
    DropEvent                        -> "drop"
    DragEndEvent                     -> "dragend"
    KeyDownEvent                     -> "keydown"
    KeyPressEvent                    -> "keypress"
    KeyUpEvent                       -> "keyup"
    LoadEvent                        -> "load"
    UnloadEvent                      -> "unload"
    AbortEvent                       -> "abort"
    ErrorEvent                       -> "error"
    ResizeEvent                      -> "resize"
    ScrollEvent                      -> "scroll"
    SelectEvent                      -> "select"
    ChangeEvent                      -> "change"
    SubmitEvent                      -> "submit"
    ResetEvent                       -> "reset"
    FocusEvent                       -> "focus"
    BlurEvent                        -> "blur"
    FocusInEvent                     -> "focusin"
    FocusOutEvent                    -> "focusout"
    DOMActivateEvent                 -> "DOMActivate"
    DOMSubtreeModifiedEvent          -> "DOMSubtreeModified"
    DOMNodeInsertedEvent             -> "DOMNodeInserted"
    DOMNodeRemovedEvent              -> "DOMNodeRemoved"
    DOMNodeRemovedFromDocumentEvent  -> "DOMNodeRemovedFromDocument"
    DOMNodeInsertedIntoDocumentEvent -> "DOMNodeInsertedIntoDocument"
    DOMAttrModifiedEvent             -> "DOMAttrModified"
    DOMCharacterDataModifiedEvent    -> "DOMCharacterDataModified"
    LoadStartEvent                   -> "loadstart"
    ProgressEvent                    -> "progress"
    ProgressErrorEvent               -> "error"
    ProgressAbortEvent               -> "abort"
    ProgressLoadEvent                -> "load"
    ProgressLoadEndEvent             -> "loadend"

data TouchEvent
  = TouchStart
  | TouchEnd
  | TouchMove
  | TouchEnter
  | TouchLeave
  | TouchCancel

touchEventToBytes :: TouchEvent -> LBS.ByteString
touchEventToBytes event =
  case event of
    TouchStart  -> "touchstart"
    TouchEnd    -> "touchend"
    TouchMove   -> "touchmove"
    TouchEnter  -> "touchenter"
    TouchLeave  -> "touchleave"
    TouchCancel -> "touchcancel"

touchEventToText :: TouchEvent -> T.Text
touchEventToText event =
  case event of
    TouchStart  -> "touchstart"
    TouchEnd    -> "touchend"
    TouchMove   -> "touchmove"
    TouchEnter  -> "touchenter"
    TouchLeave  -> "touchleave"
    TouchCancel -> "touchcancel"

data HtmxEvent
  = HtmxAbort
  | HtmxAfterOnLoad
  | HtmxAfterProcessNode
  | HtmxAfterRequest
  | HtmxAfterSettle
  | HtmxAfterSwap
  | HtmxBeforeCleanupElement
  | HtmxBeforeOnLoad
  | HtmxBeforeProcessNode
  | HtmxBeforeRequest
  | HtmxBeforeSwap
  | HtmxBeforeSend
  | HtmxConfigRequest
  | HtmxConfirm
  | HtmxHistoryCacheError
  | HtmxHistoryCacheMiss
  | HtmxHistoryCacheMissError
  | HtmxHistoryCacheMissLoad
  | HtmxHistoryRestore
  | HtmxBeforeHistorySave
  | HtmxLoad
  | HtmxNoSSESourceError
  | HtmxOnLoadError
  | HtmxOOBAfterSwap
  | HtmxOOBBeforeSwap
  | HtmxOOBErrorNoTarget
  | HtmxPrompt
  | HtmxPushedIntoHistory
  | HtmxResponseError
  | HtmxSendError
  | HtmxSSEError
  | HtmxSSEOpen
  | HtmxSwapError
  | HtmxTargetError
  | HtmxTimeout
  | HtmxValidate
  | HtmxValidationFailed
  | HtmxValidationHalted
  | HtmxXHRAbort
  | HtmxXHRLoadEnd
  | HtmxXHRLoadStart
  | HtmxXHRProgress

htmxEventToBytes :: HtmxEvent -> LBS.ByteString
htmxEventToBytes event =
  case event of
    HtmxAbort                 -> "abort"
    HtmxAfterOnLoad           -> "after-on-load"
    HtmxAfterProcessNode      -> "after-process-node"
    HtmxAfterRequest          -> "after-request"
    HtmxAfterSettle           -> "after-settle"
    HtmxAfterSwap             -> "after-swap"
    HtmxBeforeCleanupElement  -> "before-cleanup-element"
    HtmxBeforeOnLoad          -> "before-on-load"
    HtmxBeforeProcessNode     -> "before-process-node"
    HtmxBeforeRequest         -> "before-request"
    HtmxBeforeSwap            -> "before-swap"
    HtmxBeforeSend            -> "before-send"
    HtmxConfigRequest         -> "config-request"
    HtmxConfirm               -> "confirm"
    HtmxHistoryCacheError     -> "history-cache-error"
    HtmxHistoryCacheMiss      -> "history-cache-miss"
    HtmxHistoryCacheMissError -> "history-cache-miss-error"
    HtmxHistoryCacheMissLoad  -> "history-cache-miss-load"
    HtmxHistoryRestore        -> "history-restore"
    HtmxBeforeHistorySave     -> "before-history-save"
    HtmxLoad                  -> "load"
    HtmxNoSSESourceError      -> "no-sse-source-error"
    HtmxOnLoadError           -> "on-load-error"
    HtmxOOBAfterSwap          -> "oob-after-swap"
    HtmxOOBBeforeSwap         -> "oob-before-swap"
    HtmxOOBErrorNoTarget      -> "oob-error-no-target"
    HtmxPrompt                -> "prompt"
    HtmxPushedIntoHistory     -> "pushed-into-history"
    HtmxResponseError         -> "response-error"
    HtmxSendError             -> "send-error"
    HtmxSSEError              -> "sse-error"
    HtmxSSEOpen               -> "sse-open"
    HtmxSwapError             -> "swap-error"
    HtmxTargetError           -> "target-error"
    HtmxTimeout               -> "timeout"
    HtmxValidate              -> "validation-validate"
    HtmxValidationFailed      -> "validation-failed"
    HtmxValidationHalted      -> "validation-halted"
    HtmxXHRAbort              -> "xhr-abort"
    HtmxXHRLoadEnd            -> "xhr-loadend"
    HtmxXHRLoadStart          -> "xhr-loadstart"
    HtmxXHRProgress           -> "xhr-progress"

htmxEventToText :: HtmxEvent -> T.Text
htmxEventToText event =
  case event of
    HtmxAbort                 -> "abort"
    HtmxAfterOnLoad           -> "after-on-load"
    HtmxAfterProcessNode      -> "after-process-node"
    HtmxAfterRequest          -> "after-request"
    HtmxAfterSettle           -> "after-settle"
    HtmxAfterSwap             -> "after-swap"
    HtmxBeforeCleanupElement  -> "before-cleanup-element"
    HtmxBeforeOnLoad          -> "before-on-load"
    HtmxBeforeProcessNode     -> "before-process-node"
    HtmxBeforeRequest         -> "before-request"
    HtmxBeforeSwap            -> "before-swap"
    HtmxBeforeSend            -> "before-send"
    HtmxConfigRequest         -> "config-request"
    HtmxConfirm               -> "confirm"
    HtmxHistoryCacheError     -> "history-cache-error"
    HtmxHistoryCacheMiss      -> "history-cache-miss"
    HtmxHistoryCacheMissError -> "history-cache-miss-error"
    HtmxHistoryCacheMissLoad  -> "history-cache-miss-load"
    HtmxHistoryRestore        -> "history-restore"
    HtmxBeforeHistorySave     -> "before-history-save"
    HtmxLoad                  -> "load"
    HtmxNoSSESourceError      -> "no-sse-source-error"
    HtmxOnLoadError           -> "on-load-error"
    HtmxOOBAfterSwap          -> "oob-after-swap"
    HtmxOOBBeforeSwap         -> "oob-before-swap"
    HtmxOOBErrorNoTarget      -> "oob-error-no-target"
    HtmxPrompt                -> "prompt"
    HtmxPushedIntoHistory     -> "pushed-into-history"
    HtmxResponseError         -> "response-error"
    HtmxSendError             -> "send-error"
    HtmxSSEError              -> "sse-error"
    HtmxSSEOpen               -> "sse-open"
    HtmxSwapError             -> "swap-error"
    HtmxTargetError           -> "target-error"
    HtmxTimeout               -> "timeout"
    HtmxValidate              -> "validation-validate"
    HtmxValidationFailed      -> "validation-failed"
    HtmxValidationHalted      -> "validation-halted"
    HtmxXHRAbort              -> "xhr-abort"
    HtmxXHRLoadEnd            -> "xhr-loadend"
    HtmxXHRLoadStart          -> "xhr-loadstart"
    HtmxXHRProgress           -> "xhr-progress"

data TriggerLoad = TriggerLoad

triggerLoadToBytes :: TriggerLoad -> LBS.ByteString
triggerLoadToBytes = const "load"

triggerLoadToText :: TriggerLoad -> T.Text
triggerLoadToText = const "load"

data TriggerRevealed = TriggerRevealed

triggerRevealedToBytes :: TriggerRevealed -> LBS.ByteString
triggerRevealedToBytes = const "revealed"

triggerRevealedToText :: TriggerRevealed -> T.Text
triggerRevealedToText = const "revealed"
