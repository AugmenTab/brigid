module Brigid.HTML.Attributes.Event.Event
  ( Event
      ( AfterPrint
      , BeforePrint
      , BeforeUnload
      , Error
      , HashChange
      , Load
      , Message
      , Offline
      , Online
      , PageHide
      , PageShow
      , PopState
      , Resize
      , Storage
      , Unload
      , Blur
      , Change
      , ContextMenu
      , Focus
      , Input
      , Invalid
      , Reset
      , Select
      , Submit
      , KeyDown
      , KeyPress
      , KeyUp
      , Click
      , DoubleClick
      , MouseDown
      , MouseMove
      , MouseOut
      , MouseOver
      , MouseUp
      , Wheel
      , Drag
      , DragEnd
      , DragEnter
      , DragLeave
      , DragOver
      , DragStart
      , Drop
      , Scroll
      , Copy
      , Cut
      , Paste
      , Abort
      , CanPlay
      , CanPlaythrough
      , CueChange
      , DurationChange
      , Emptied
      , Ended
      , LoadedData
      , LoadedMetadata
      , LoadStart
      , Pause
      , Play
      , Playing
      , Progress
      , RateChange
      , Seeked
      , Seeking
      , Stalled
      , Suspend
      , TimeUpdate
      , VolumeChange
      , Waiting
      , Toggle
      )
  , eventToText
  , eventAttributeToBytes
  , eventAttributeToBytesBuilder
  , eventAttributeToText
  , eventAttributeToTextBuilder
  ) where

import Data.ByteString.Builder (Builder, string8, toLazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL

data Event
  -- Window events
  = AfterPrint
  | BeforePrint
  | BeforeUnload
  | Error
  | HashChange
  | Load
  | Message
  | Offline
  | Online
  | PageHide
  | PageShow
  | PopState
  | Resize
  | Storage
  | Unload

  -- Form events
  | Blur
  | Change
  | ContextMenu
  | Focus
  | Input
  | Invalid
  | Reset
  | Select
  | Submit

  -- Keyboard events
  | KeyDown
  | KeyPress
  | KeyUp

  -- Mouse events
  | Click
  | DoubleClick
  | MouseDown
  | MouseMove
  | MouseOut
  | MouseOver
  | MouseUp
  | Wheel

  -- Drag events
  | Drag
  | DragEnd
  | DragEnter
  | DragLeave
  | DragOver
  | DragStart
  | Drop
  | Scroll

  -- Clipboard events
  | Copy
  | Cut
  | Paste

  -- Media events
  | Abort
  | CanPlay
  | CanPlaythrough
  | CueChange
  | DurationChange
  | Emptied
  | Ended
  -- | Error
  | LoadedData
  | LoadedMetadata
  | LoadStart
  | Pause
  | Play
  | Playing
  | Progress
  | RateChange
  | Seeked
  | Seeking
  | Stalled
  | Suspend
  | TimeUpdate
  | VolumeChange
  | Waiting

  -- Misc events
  | Toggle
  deriving (Bounded, Enum, Eq, Show)

eventToText :: Event -> T.Text
eventToText event =
  T.pack $
    case event of
       AfterPrint -> "afterprint"
       BeforePrint -> "beforeprint"
       BeforeUnload -> "beforeunload"
       Error -> "error"
       HashChange -> "hashchange"
       Load -> "load"
       Message -> "message"
       Offline -> "offline"
       Online -> "online"
       PageHide -> "pagehide"
       PageShow -> "pageshow"
       PopState -> "popstate"
       Resize -> "resize"
       Storage -> "storage"
       Unload -> "unload"
       Blur -> "blur"
       Change -> "change"
       ContextMenu -> "contextmenu"
       Focus -> "focus"
       Input -> "input"
       Invalid -> "invalid"
       Reset -> "reset"
       Select -> "select"
       Submit -> "submit"
       KeyDown -> "keydown"
       KeyPress -> "keypress"
       KeyUp -> "keyup"
       Click -> "click"
       DoubleClick -> "doubleclick"
       MouseDown -> "mousedown"
       MouseMove -> "mousemove"
       MouseOut -> "mouseout"
       MouseOver -> "mouseover"
       MouseUp -> "mouseup"
       Wheel -> "wheel"
       Drag -> "drag"
       DragEnd -> "dragend"
       DragEnter -> "dragenter"
       DragLeave -> "dragleave"
       DragOver -> "dragover"
       DragStart -> "dragstart"
       Drop -> "drop"
       Scroll -> "scroll"
       Copy -> "copy"
       Cut -> "cut"
       Paste -> "paste"
       Abort -> "abort"
       CanPlay -> "canplay"
       CanPlaythrough -> "canplaythrough"
       CueChange -> "cuechange"
       DurationChange -> "durationchange"
       Emptied -> "emptied"
       Ended -> "ended"
       LoadedData -> "loadeddata"
       LoadedMetadata -> "loadedmetadata"
       LoadStart -> "loadstart"
       Pause -> "pause"
       Play -> "play"
       Playing -> "playing"
       Progress -> "progress"
       RateChange -> "ratechange"
       Seeked -> "seeked"
       Seeking -> "seeking"
       Stalled -> "stalled"
       Suspend -> "suspend"
       TimeUpdate -> "timeupdate"
       VolumeChange -> "volumechange"
       Waiting -> "waiting"
       Toggle -> "toggle"

eventAttributeToBytesBuilder :: Event -> Builder
eventAttributeToBytesBuilder event =
  string8 $
    case event of
       AfterPrint -> "onafterprint"
       BeforePrint -> "onbeforeprint"
       BeforeUnload -> "onbeforeunload"
       Error -> "onerror"
       HashChange -> "onhashchange"
       Load -> "onload"
       Message -> "onmessage"
       Offline -> "onoffline"
       Online -> "ononline"
       PageHide -> "onpagehide"
       PageShow -> "onpageshow"
       PopState -> "onpopstate"
       Resize -> "onresize"
       Storage -> "onstorage"
       Unload -> "onunload"
       Blur -> "onblur"
       Change -> "onchange"
       ContextMenu -> "oncontextmenu"
       Focus -> "onfocus"
       Input -> "oninput"
       Invalid -> "oninvalid"
       Reset -> "onreset"
       Select -> "onselect"
       Submit -> "onsubmit"
       KeyDown -> "onkeydown"
       KeyPress -> "onkeypress"
       KeyUp -> "onkeyup"
       Click -> "onclick"
       DoubleClick -> "ondoubleclick"
       MouseDown -> "onmousedown"
       MouseMove -> "onmousemove"
       MouseOut -> "onmouseout"
       MouseOver -> "onmouseover"
       MouseUp -> "onmouseup"
       Wheel -> "onwheel"
       Drag -> "ondrag"
       DragEnd -> "ondragend"
       DragEnter -> "ondragenter"
       DragLeave -> "ondragleave"
       DragOver -> "ondragover"
       DragStart -> "ondragstart"
       Drop -> "ondrop"
       Scroll -> "onscroll"
       Copy -> "oncopy"
       Cut -> "oncut"
       Paste -> "onpaste"
       Abort -> "onabort"
       CanPlay -> "oncanplay"
       CanPlaythrough -> "oncanplaythrough"
       CueChange -> "oncuechange"
       DurationChange -> "ondurationchange"
       Emptied -> "onemptied"
       Ended -> "onended"
       LoadedData -> "onloadeddata"
       LoadedMetadata -> "onloadedmetadata"
       LoadStart -> "onloadstart"
       Pause -> "onpause"
       Play -> "onplay"
       Playing -> "onplaying"
       Progress -> "onprogress"
       RateChange -> "onratechange"
       Seeked -> "onseeked"
       Seeking -> "onseeking"
       Stalled -> "onstalled"
       Suspend -> "onsuspend"
       TimeUpdate -> "ontimeupdate"
       VolumeChange -> "onvolumechange"
       Waiting -> "onwaiting"
       Toggle -> "ontoggle"

eventAttributeToBytes :: Event -> LBS.ByteString
eventAttributeToBytes =
  toLazyByteString . eventAttributeToBytesBuilder

eventAttributeToText :: Event -> T.Text
eventAttributeToText e =
  T.pack "on" <> eventToText e

eventAttributeToTextBuilder :: Event -> TBL.Builder
eventAttributeToTextBuilder = TBL.fromText . eventAttributeToText
