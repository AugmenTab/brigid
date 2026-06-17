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
  , featurePolicyDirectiveToBytesBuilder
  , featurePolicyDirectiveToText
  ) where

import Data.ByteString.Builder (Builder, string8)
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

featurePolicyDirectiveToBytesBuilder :: FeaturePolicyDirective -> Builder
featurePolicyDirectiveToBytesBuilder directive =
  case directive of
    Accelerometer           -> string8 "accelerometer"
    AmbientLightSensor      -> string8 "ambient-light-sensor"
    Autoplay                -> string8 "autoplay"
    Battery                 -> string8 "battery"
    Camera                  -> string8 "camera"
    DisplayCapture          -> string8 "display-capture"
    DocumentDomain          -> string8 "document-domain"
    EncryptedMedia          -> string8 "encrypted-media"
    Fullscreen              -> string8 "fullscreen"
    Geolocation             -> string8 "geolocation"
    Gyroscope               -> string8 "gyroscope"
    LayoutAnimations        -> string8 "layout-animations"
    LegacyImageFormats      -> string8 "legacy-image-formats"
    Magnetometer            -> string8 "magnetometer"
    Microphone              -> string8 "microphone"
    Midi                    -> string8 "midi"
    NavigationOverride      -> string8 "navigation-override"
    OversizedImages         -> string8 "oversized-images"
    Payment                 -> string8 "payment"
    PictureInPicture        -> string8 "picture-in-picture"
    PublickeyCredentialsGet -> string8 "publickey-credentials-get"
    ScreenWakeLock          -> string8 "screen-wake-lock"
    SyncScript              -> string8 "sync-script"
    SyncXHR                 -> string8 "sync-xhr"
    UnoptimizedImages       -> string8 "unoptimized-images"
    UnsizedMedia            -> string8 "unsized-media"
    USB                     -> string8 "usb"
    WebShare                -> string8 "web-share"
    XRSpatialTracking       -> string8 "xr-spatial-tracking"

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
