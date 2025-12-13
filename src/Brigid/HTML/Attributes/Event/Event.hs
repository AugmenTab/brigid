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
  , eventToBytes
  , eventToText
  , eventAttributeToBytes
  , eventAttributeToText
  ) where

import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

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

eventToBytes :: Event -> LBS.ByteString
eventToBytes event =
  LBS8.pack $
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

eventAttributeToBytes :: Event -> LBS.ByteString
eventAttributeToBytes e =
  LBS8.pack "on" <> eventToBytes e

eventAttributeToText :: Event -> T.Text
eventAttributeToText e =
  T.pack "on" <> eventToText e
