module HTML.Types.Event
  ( HTMLEvent
      ( Click
      , DblClick
      , MouseDown
      , MouseUp
      , MouseOver
      , MouseMove
      , MouseOut
      , DragStart
      , Drag
      , DragEnter
      , DragLeave
      , DragOver
      , Drop
      , DragEnd
      , KeyDown
      , KeyPress
      , KeyUp
      , Load
      , Unload
      , Abort
      , Error
      , Resize
      , Scroll
      , Select
      , Change
      , Submit
      , Reset
      , Focus
      , Blur
      , FocusIn
      , FocusOut
      , DOMActivate
      , DOMSubtreeModified
      , DOMNodeInserted
      , DOMNodeRemoved
      , DOMNodeRemovedFromDocument
      , DOMNodeInsertedIntoDocument
      , DOMAttrModified
      , DOMCharacterDataModified
      , LoadStart
      , Progress
      , ProgressError
      , ProgressAbort
      , ProgressLoad
      , ProgressLoadEnd
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
      (
      )
  , htmxEventToBytes
  , htmxEventToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data HTMLEvent
  = Click
  | DblClick
  | MouseDown
  | MouseUp
  | MouseOver
  | MouseMove
  | MouseOut
  | DragStart
  | Drag
  | DragEnter
  | DragLeave
  | DragOver
  | Drop
  | DragEnd
  | KeyDown
  | KeyPress
  | KeyUp
  | Load
  | Unload
  | Abort
  | Error
  | Resize
  | Scroll
  | Select
  | Change
  | Submit
  | Reset
  | Focus
  | Blur
  | FocusIn
  | FocusOut
  | DOMActivate
  | DOMSubtreeModified
  | DOMNodeInserted
  | DOMNodeRemoved
  | DOMNodeRemovedFromDocument
  | DOMNodeInsertedIntoDocument
  | DOMAttrModified
  | DOMCharacterDataModified
  | LoadStart
  | Progress
  | ProgressError
  | ProgressAbort
  | ProgressLoad
  | ProgressLoadEnd

htmlEventToBytes :: HTMLEvent -> LBS.ByteString
htmlEventToBytes event =
  case event of
    Click                       -> "click"
    DblClick                    -> "dblclick"
    MouseDown                   -> "mousedown"
    MouseUp                     -> "mouseup"
    MouseOver                   -> "mouseover"
    MouseMove                   -> "mousemove"
    MouseOut                    -> "mouseout"
    DragStart                   -> "dragstart"
    Drag                        -> "drag"
    DragEnter                   -> "dragenter"
    DragLeave                   -> "dragleave"
    DragOver                    -> "dragover"
    Drop                        -> "drop"
    DragEnd                     -> "dragend"
    KeyDown                     -> "keydown"
    KeyPress                    -> "keypress"
    KeyUp                       -> "keyup"
    Load                        -> "load"
    Unload                      -> "unload"
    Abort                       -> "abort"
    Error                       -> "error"
    Resize                      -> "resize"
    Scroll                      -> "scroll"
    Select                      -> "select"
    Change                      -> "change"
    Submit                      -> "submit"
    Reset                       -> "reset"
    Focus                       -> "focus"
    Blur                        -> "blur"
    FocusIn                     -> "focusin"
    FocusOut                    -> "focusout"
    DOMActivate                 -> "DOMActivate"
    DOMSubtreeModified          -> "DOMSubtreeModified"
    DOMNodeInserted             -> "DOMNodeInserted"
    DOMNodeRemoved              -> "DOMNodeRemoved"
    DOMNodeRemovedFromDocument  -> "DOMNodeRemovedFromDocument"
    DOMNodeInsertedIntoDocument -> "DOMNodeInsertedIntoDocument"
    DOMAttrModified             -> "DOMAttrModified"
    DOMCharacterDataModified    -> "DOMCharacterDataModified"
    LoadStart                   -> "loadstart"
    Progress                    -> "progress"
    ProgressError               -> "error"
    ProgressAbort               -> "abort"
    ProgressLoad                -> "load"
    ProgressLoadEnd             -> "loadend"

htmlEventToText :: HTMLEvent -> T.Text
htmlEventToText event =
  case event of
    Click                       -> "click"
    DblClick                    -> "dblclick"
    MouseDown                   -> "mousedown"
    MouseUp                     -> "mouseup"
    MouseOver                   -> "mouseover"
    MouseMove                   -> "mousemove"
    MouseOut                    -> "mouseout"
    DragStart                   -> "dragstart"
    Drag                        -> "drag"
    DragEnter                   -> "dragenter"
    DragLeave                   -> "dragleave"
    DragOver                    -> "dragover"
    Drop                        -> "drop"
    DragEnd                     -> "dragend"
    KeyDown                     -> "keydown"
    KeyPress                    -> "keypress"
    KeyUp                       -> "keyup"
    Load                        -> "load"
    Unload                      -> "unload"
    Abort                       -> "abort"
    Error                       -> "error"
    Resize                      -> "resize"
    Scroll                      -> "scroll"
    Select                      -> "select"
    Change                      -> "change"
    Submit                      -> "submit"
    Reset                       -> "reset"
    Focus                       -> "focus"
    Blur                        -> "blur"
    FocusIn                     -> "focusin"
    FocusOut                    -> "focusout"
    DOMActivate                 -> "DOMActivate"
    DOMSubtreeModified          -> "DOMSubtreeModified"
    DOMNodeInserted             -> "DOMNodeInserted"
    DOMNodeRemoved              -> "DOMNodeRemoved"
    DOMNodeRemovedFromDocument  -> "DOMNodeRemovedFromDocument"
    DOMNodeInsertedIntoDocument -> "DOMNodeInsertedIntoDocument"
    DOMAttrModified             -> "DOMAttrModified"
    DOMCharacterDataModified    -> "DOMCharacterDataModified"
    LoadStart                   -> "loadstart"
    Progress                    -> "progress"
    ProgressError               -> "error"
    ProgressAbort               -> "abort"
    ProgressLoad                -> "load"
    ProgressLoadEnd             -> "loadend"

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
    HtmxAfterOnLoad           -> "afterOnLoad"
    HtmxAfterProcessNode      -> "afterProcessNode"
    HtmxAfterRequest          -> "afterRequest"
    HtmxAfterSettle           -> "afterSettle"
    HtmxAfterSwap             -> "afterSwap"
    HtmxBeforeCleanupElement  -> "beforeCleanupElement"
    HtmxBeforeOnLoad          -> "beforeOnLoad"
    HtmxBeforeProcessNode     -> "beforeProcessNode"
    HtmxBeforeRequest         -> "beforeRequest"
    HtmxBeforeSwap            -> "beforeSwap"
    HtmxBeforeSend            -> "beforeSend"
    HtmxConfigRequest         -> "configRequest"
    HtmxConfirm               -> "confirm"
    HtmxHistoryCacheError     -> "historyCacheError"
    HtmxHistoryCacheMiss      -> "historyCacheMiss"
    HtmxHistoryCacheMissError -> "historyCacheMissError"
    HtmxHistoryCacheMissLoad  -> "historyCacheMissLoad"
    HtmxHistoryRestore        -> "historyRestore"
    HtmxBeforeHistorySave     -> "beforeHistorySave"
    HtmxLoad                  -> "load"
    HtmxNoSSESourceError      -> "noSSESourceError"
    HtmxOnLoadError           -> "onLoadError"
    HtmxOOBAfterSwap          -> "oobAfterSwap"
    HtmxOOBBeforeSwap         -> "oobBeforeSwap"
    HtmxOOBErrorNoTarget      -> "oobErrorNoTarget"
    HtmxPrompt                -> "prompt"
    HtmxPushedIntoHistory     -> "pushedIntoHistory"
    HtmxResponseError         -> "responseError"
    HtmxSendError             -> "sendError"
    HtmxSSEError              -> "sseError"
    HtmxSSEOpen               -> "sseOpen"
    HtmxSwapError             -> "swapError"
    HtmxTargetError           -> "targetError"
    HtmxTimeout               -> "timeout"
    HtmxValidate              -> "validation:validate"
    HtmxValidationFailed      -> "validation:failed"
    HtmxValidationHalted      -> "validation:halted"
    HtmxXHRAbort              -> "xhr:abort"
    HtmxXHRLoadEnd            -> "xhr:loadend"
    HtmxXHRLoadStart          -> "xhr:loadstart"
    HtmxXHRProgress           -> "xhr:progress"

htmxEventToText :: HtmxEvent -> T.Text
htmxEventToText event =
  case event of
    HtmxAbort                 -> "abort"
    HtmxAfterOnLoad           -> "afterOnLoad"
    HtmxAfterProcessNode      -> "afterProcessNode"
    HtmxAfterRequest          -> "afterRequest"
    HtmxAfterSettle           -> "afterSettle"
    HtmxAfterSwap             -> "afterSwap"
    HtmxBeforeCleanupElement  -> "beforeCleanupElement"
    HtmxBeforeOnLoad          -> "beforeOnLoad"
    HtmxBeforeProcessNode     -> "beforeProcessNode"
    HtmxBeforeRequest         -> "beforeRequest"
    HtmxBeforeSwap            -> "beforeSwap"
    HtmxBeforeSend            -> "beforeSend"
    HtmxConfigRequest         -> "configRequest"
    HtmxConfirm               -> "confirm"
    HtmxHistoryCacheError     -> "historyCacheError"
    HtmxHistoryCacheMiss      -> "historyCacheMiss"
    HtmxHistoryCacheMissError -> "historyCacheMissError"
    HtmxHistoryCacheMissLoad  -> "historyCacheMissLoad"
    HtmxHistoryRestore        -> "historyRestore"
    HtmxBeforeHistorySave     -> "beforeHistorySave"
    HtmxLoad                  -> "load"
    HtmxNoSSESourceError      -> "noSSESourceError"
    HtmxOnLoadError           -> "onLoadError"
    HtmxOOBAfterSwap          -> "oobAfterSwap"
    HtmxOOBBeforeSwap         -> "oobBeforeSwap"
    HtmxOOBErrorNoTarget      -> "oobErrorNoTarget"
    HtmxPrompt                -> "prompt"
    HtmxPushedIntoHistory     -> "pushedIntoHistory"
    HtmxResponseError         -> "responseError"
    HtmxSendError             -> "sendError"
    HtmxSSEError              -> "sseError"
    HtmxSSEOpen               -> "sseOpen"
    HtmxSwapError             -> "swapError"
    HtmxTargetError           -> "targetError"
    HtmxTimeout               -> "timeout"
    HtmxValidate              -> "validation:validate"
    HtmxValidationFailed      -> "validation:failed"
    HtmxValidationHalted      -> "validation:halted"
    HtmxXHRAbort              -> "xhr:abort"
    HtmxXHRLoadEnd            -> "xhr:loadend"
    HtmxXHRLoadStart          -> "xhr:loadstart"
    HtmxXHRProgress           -> "xhr:progress"
