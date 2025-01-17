module Brigid.HTML.Types.SandboxToken
  ( SandboxToken
      ( AllowForms
      , AllowModals
      , AllowOrientationLock
      , AllowPointerLock
      , AllowPopups
      , AllowPopupsToEscapeSandbox
      , AllowPresentation
      , AllowSameOrigin
      , AllowScripts
      , AllowStorageAccessByUserActivation
      , AllowTopNavigation
      , AllowTopNavigationByUserActivation
      )
  , sandboxTokenToBytes
  , sandboxTokenToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data SandboxToken
  = AllowForms
  -- ^ Allows the iframe to submit forms.
  | AllowModals
  -- ^ Allows the iframe to open modal dialogs (e.g., via window.alert or window.confirm).
  | AllowOrientationLock
  -- ^ Allows the iframe to lock the screen orientation.
  | AllowPointerLock
  -- ^ Allows the iframe to use the pointer lock API.
  | AllowPopups
  -- ^ Allows the iframe to open new browser windows or tabs via scripts (e.g., window.open).
  | AllowPopupsToEscapeSandbox
  -- ^ Allows popups opened by the iframe to escape the sandbox and act as regular browser windows.
  | AllowPresentation
  -- ^ Allows the iframe to start a presentation session.
  | AllowSameOrigin
  -- ^ Allows the iframe to access content from the same origin as the parent page.
  | AllowScripts
  -- ^ Allows the iframe to execute JavaScript. Does not override allow-same-origin.
  | AllowStorageAccessByUserActivation
  -- ^ Allows storage access triggered by user interaction within the iframe.
  | AllowTopNavigation
  -- ^ Allows the iframe to navigate the top-level browsing context.
  | AllowTopNavigationByUserActivation
  -- ^ Allows top-level navigation only when triggered by user interaction.
  deriving (Bounded, Enum, Eq, Show)

sandboxTokenToBytes :: SandboxToken -> LBS.ByteString
sandboxTokenToBytes token =
  case token of
    AllowForms                         -> "allow-forms"
    AllowModals                        -> "allow-modals"
    AllowOrientationLock               -> "allow-orientation-lock"
    AllowPointerLock                   -> "allow-pointer-lock"
    AllowPopups                        -> "allow-popups"
    AllowPopupsToEscapeSandbox         -> "allow-popups-to-escape-sandbox"
    AllowPresentation                  -> "allow-presentation"
    AllowSameOrigin                    -> "allow-same-origin"
    AllowScripts                       -> "allow-scripts"
    AllowStorageAccessByUserActivation -> "allow-storage-access-by-user-activation"
    AllowTopNavigation                 -> "allow-top-navigation"
    AllowTopNavigationByUserActivation -> "allow-top-navigation-by-user-activation"

sandboxTokenToText :: SandboxToken -> T.Text
sandboxTokenToText token =
  case token of
    AllowForms                         -> "allow-forms"
    AllowModals                        -> "allow-modals"
    AllowOrientationLock               -> "allow-orientation-lock"
    AllowPointerLock                   -> "allow-pointer-lock"
    AllowPopups                        -> "allow-popups"
    AllowPopupsToEscapeSandbox         -> "allow-popups-to-escape-sandbox"
    AllowPresentation                  -> "allow-presentation"
    AllowSameOrigin                    -> "allow-same-origin"
    AllowScripts                       -> "allow-scripts"
    AllowStorageAccessByUserActivation -> "allow-storage-access-by-user-activation"
    AllowTopNavigation                 -> "allow-top-navigation"
    AllowTopNavigationByUserActivation -> "allow-top-navigation-by-user-activation"
