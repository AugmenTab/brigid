module HTML.Types
  ( AutocapitalizeOption
      ( NoAutocapitalization
      , Sentences
      , Words
      , Characters
      )
  , autocapitalizeOptionToBytes
  , autocapitalizeOptionToText
  , ContentEditableOption
      ( Editable
      , NotEditable
      , PlaintextOnly
      )
  , contentEditableOptionToBytes
  , contentEditableOptionToText
  , CrossOriginFetch
      ( Anonymous
      , UseCredentials
      )
  , crossoriginFetchToBytes
  , crossoriginFetchToText
  , Directionality
      ( LeftToRight
      , RightToLeft
      , Auto
      )
  , directionalityToBytes
  , directionalityToText
  , ExportPart (ExportPart)
  , exportPartToBytes
  , exportPartToText
  , KeyHintOption
      ( Enter
      , Done
      , Go
      , Next
      , Previous
      , Search
      , Send
      )
  , keyHintOptionToBytes
  , keyHintOptionToText
  , InputMode
      ( NoInputMode
      , TextMode
      , DecimalMode
      , NumericMode
      , TelephoneMode
      , SearchMode
      , EmailMode
      , URLMode
      )
  , inputModeToBytes
  , inputModeToText
  , Part (Part)
  , partToBytes
  , partToText
  , PopoverState
      ( AutoPopover
      , ManualPopover
      )
  , popoverStateToBytes
  , popoverStateToText
  , Reachability
      ( Reachable
      , NotReachable
      )
  , reachabilityToInt
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

data AutocapitalizeOption
  = NoAutocapitalization
  | Sentences
  | Words
  | Characters

autocapitalizeOptionToBytes :: AutocapitalizeOption -> LBS.ByteString
autocapitalizeOptionToBytes option =
  case option of
    NoAutocapitalization -> "none"
    Sentences            -> "sentences"
    Words                -> "words"
    Characters           -> "characters"

autocapitalizeOptionToText :: AutocapitalizeOption -> T.Text
autocapitalizeOptionToText option =
  case option of
    NoAutocapitalization -> "none"
    Sentences            -> "sentences"
    Words                -> "words"
    Characters           -> "characters"

data ContentEditableOption
  = Editable
  | NotEditable
  | PlaintextOnly

contentEditableOptionToBytes :: ContentEditableOption -> LBS.ByteString
contentEditableOptionToBytes option =
  case option of
    Editable      -> "true"
    NotEditable   -> "false"
    PlaintextOnly -> "plaintext-only"

contentEditableOptionToText :: ContentEditableOption -> T.Text
contentEditableOptionToText option =
  case option of
    Editable      -> "true"
    NotEditable   -> "false"
    PlaintextOnly -> "plaintext-only"

{-|
   Represents the options available for the 'crossorigin' attribute.
-}
data CrossOriginFetch
  -- | A cross-origin request (i.e. with an @Origin@ HTTP header) is performed,
  -- but no credential is sent (i.e. no cookie, X.509 certificate, or HTTP
  -- Basic authentication). If the server does not give credentials to the
  -- origin site (by not setting the @Access-Control-Allow-Origin@ HTTP header)
  -- the resource will be tainted and its usage restricted.
  = Anonymous
  -- | A cross-origin request (i.e. with an @Origin@ HTTP header) is performed
  -- along with a credential sent (i.e. a cookie, certificate, and/or HTTP
  -- Basic authentication is performed). If the server does not give
  -- credentials to the origin site (through @Access-Control-Allow-Credentials@
  -- HTTP header), the resource will be tainted and its usage restricted.
  | UseCredentials

crossoriginFetchToBytes :: CrossOriginFetch -> LBS.ByteString
crossoriginFetchToBytes cors =
  case cors of
    Anonymous      -> "anonymous"
    UseCredentials -> "use-credentials"

crossoriginFetchToText :: CrossOriginFetch -> T.Text
crossoriginFetchToText cors =
  case cors of
    Anonymous      -> "anonymous"
    UseCredentials -> "use-credentials"

data Directionality
  = LeftToRight
  | RightToLeft
  | Auto

directionalityToBytes :: Directionality -> LBS.ByteString
directionalityToBytes option =
  case option of
    LeftToRight -> "ltr"
    RightToLeft -> "rtl"
    Auto        -> "auto"

directionalityToText :: Directionality -> T.Text
directionalityToText option =
  case option of
    LeftToRight -> "ltr"
    RightToLeft -> "rtl"
    Auto        -> "auto"

data ExportPart = ExportPart Part (Maybe T.Text)

exportPartToBytes :: ExportPart -> LBS.ByteString
exportPartToBytes = LBS.fromStrict . TE.encodeUtf8 . exportPartToText

exportPartToText :: ExportPart -> T.Text
exportPartToText (ExportPart part mbExposed) =
  partToText part <> maybe "" (":" <>) mbExposed

data KeyHintOption
  = Enter
  | Done
  | Go
  | Next
  | Previous
  | Search
  | Send

keyHintOptionToBytes :: KeyHintOption -> LBS.ByteString
keyHintOptionToBytes option =
  case option of
    Enter    -> "enter"
    Done     -> "done"
    Go       -> "go"
    Next     -> "next"
    Previous -> "previous"
    Search   -> "search"
    Send     -> "send"

keyHintOptionToText :: KeyHintOption -> T.Text
keyHintOptionToText option =
  case option of
    Enter    -> "enter"
    Done     -> "done"
    Go       -> "go"
    Next     -> "next"
    Previous -> "previous"
    Search   -> "search"
    Send     -> "send"

data InputMode
  = NoInputMode
  | TextMode
  | DecimalMode
  | NumericMode
  | TelephoneMode
  | SearchMode
  | EmailMode
  | URLMode

inputModeToBytes :: InputMode -> LBS.ByteString
inputModeToBytes mode =
  case mode of
    NoInputMode   -> "none"
    TextMode      -> "text"
    DecimalMode   -> "decimal"
    NumericMode   -> "numeric"
    TelephoneMode -> "tel"
    SearchMode    -> "search"
    EmailMode     -> "email"
    URLMode       -> "url"

inputModeToText :: InputMode -> T.Text
inputModeToText mode =
  case mode of
    NoInputMode   -> "none"
    TextMode      -> "text"
    DecimalMode   -> "decimal"
    NumericMode   -> "numeric"
    TelephoneMode -> "tel"
    SearchMode    -> "search"
    EmailMode     -> "email"
    URLMode       -> "url"

newtype Part = Part T.Text

partToBytes :: Part -> LBS.ByteString
partToBytes = LBS.fromStrict . TE.encodeUtf8 . partToText

partToText :: Part -> T.Text
partToText (Part part) = part

data PopoverState
  = AutoPopover
  | ManualPopover

popoverStateToBytes :: PopoverState -> LBS.ByteString
popoverStateToBytes pos =
  case pos of
    AutoPopover   -> "auto"
    ManualPopover -> "manual"

popoverStateToText :: PopoverState -> T.Text
popoverStateToText pos =
  case pos of
    AutoPopover   -> "auto"
    ManualPopover -> "manual"

data Reachability
  = Reachable
  | NotReachable

reachabilityToInt :: Reachability -> Int
reachabilityToInt option =
  case option of
    Reachable    -> 0
    NotReachable -> negate 1
