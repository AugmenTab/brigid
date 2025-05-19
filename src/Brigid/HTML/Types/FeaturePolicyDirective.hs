module Brigid.HTML.Types.FeaturePolicyDirective
  ( FeaturePolicyDirective
      ( Accelerometer
      , AmbientLightSensor
      , Autoplay
      , Battery
      , Camera
      , DisplayCapture
      , DocumentDomain
      , EncryptedMedia
      , Fullscreen
      , Geolocation
      , Gyroscope
      , LayoutAnimations
      , LegacyImageFormats
      , Magnetometer
      , Microphone
      , Midi
      , NavigationOverride
      , OversizedImages
      , Payment
      , PictureInPicture
      , PublickeyCredentialsGet
      , ScreenWakeLock
      , SyncScript
      , SyncXHR
      , UnoptimizedImages
      , UnsizedMedia
      , USB
      , WebShare
      , XRSpatialTracking
      )
  , featurePolicyDirectiveToBytes
  , featurePolicyDirectiveToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data FeaturePolicyDirective
  = Accelerometer
  | AmbientLightSensor
  | Autoplay
  | Battery
  | Camera
  | DisplayCapture
  | DocumentDomain
  | EncryptedMedia
  | Fullscreen
  | Geolocation
  | Gyroscope
  | LayoutAnimations
  | LegacyImageFormats
  | Magnetometer
  | Microphone
  | Midi
  | NavigationOverride
  | OversizedImages
  | Payment
  | PictureInPicture
  | PublickeyCredentialsGet
  | ScreenWakeLock
  | SyncScript
  | SyncXHR
  | UnoptimizedImages
  | UnsizedMedia
  | USB
  | WebShare
  | XRSpatialTracking
  deriving (Bounded, Enum, Eq, Show)

featurePolicyDirectiveToBytes :: FeaturePolicyDirective -> LBS.ByteString
featurePolicyDirectiveToBytes directive =
  case directive of
    Accelerometer           -> "accelerometer"
    AmbientLightSensor      -> "ambient-light-sensor"
    Autoplay                -> "autoplay"
    Battery                 -> "battery"
    Camera                  -> "camera"
    DisplayCapture          -> "display-capture"
    DocumentDomain          -> "document-domain"
    EncryptedMedia          -> "encrypted-media"
    Fullscreen              -> "fullscreen"
    Geolocation             -> "geolocation"
    Gyroscope               -> "gyroscope"
    LayoutAnimations        -> "layout-animations"
    LegacyImageFormats      -> "legacy-image-formats"
    Magnetometer            -> "magnetometer"
    Microphone              -> "microphone"
    Midi                    -> "midi"
    NavigationOverride      -> "navigation-override"
    OversizedImages         -> "oversized-images"
    Payment                 -> "payment"
    PictureInPicture        -> "picture-in-picture"
    PublickeyCredentialsGet -> "publickey-credentials-get"
    ScreenWakeLock          -> "screen-wake-lock"
    SyncScript              -> "sync-script"
    SyncXHR                 -> "sync-xhr"
    UnoptimizedImages       -> "unoptimized-images"
    UnsizedMedia            -> "unsized-media"
    USB                     -> "usb"
    WebShare                -> "web-share"
    XRSpatialTracking       -> "xr-spatial-tracking"

featurePolicyDirectiveToText :: FeaturePolicyDirective -> T.Text
featurePolicyDirectiveToText directive =
  case directive of
    Accelerometer           -> "accelerometer"
    AmbientLightSensor      -> "ambient-light-sensor"
    Autoplay                -> "autoplay"
    Battery                 -> "battery"
    Camera                  -> "camera"
    DisplayCapture          -> "display-capture"
    DocumentDomain          -> "document-domain"
    EncryptedMedia          -> "encrypted-media"
    Fullscreen              -> "fullscreen"
    Geolocation             -> "geolocation"
    Gyroscope               -> "gyroscope"
    LayoutAnimations        -> "layout-animations"
    LegacyImageFormats      -> "legacy-image-formats"
    Magnetometer            -> "magnetometer"
    Microphone              -> "microphone"
    Midi                    -> "midi"
    NavigationOverride      -> "navigation-override"
    OversizedImages         -> "oversized-images"
    Payment                 -> "payment"
    PictureInPicture        -> "picture-in-picture"
    PublickeyCredentialsGet -> "publickey-credentials-get"
    ScreenWakeLock          -> "screen-wake-lock"
    SyncScript              -> "sync-script"
    SyncXHR                 -> "sync-xhr"
    UnoptimizedImages       -> "unoptimized-images"
    UnsizedMedia            -> "unsized-media"
    USB                     -> "usb"
    WebShare                -> "web-share"
    XRSpatialTracking       -> "xr-spatial-tracking"
