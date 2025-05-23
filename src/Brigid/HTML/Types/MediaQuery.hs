module Brigid.HTML.Types.MediaQuery
  ( MediaQuery (..)
  , mediaQueryToBytes
  , mediaQueryToText
  , MediaQueryModifier
      ( MediaNot
      , MediaOnly
      )
  , MediaQueryType
      ( MediaAll
      , MediaScreen
      , MediaPrint
      , MediaSpeech
      )
  , MediaFeature
      ( Width
      , Height
      , DeviceWidth
      , DeviceHeight
      , AspectRatio
      , DeviceAspectRatio
      , Orientation
      , Resolution
      , Scan
      , Grid
      , Color
      , ColorIndex
      , Monochrome
      , InvertedColors
      , ForcedColors
      , PrefersColorScheme
      , PrefersReducedMotion
      , PrefersReducedTransparency
      , PrefersContrast
      , PrefersReducedData
      , Hover
      , Pointer
      , AnyHover
      , AnyPointer
      , Update
      )
  , mediaFeatureToBytes
  , mediaFeatureToText
  , MediaFeatureType
      ( Min
      , Max
      , Exact
      )
  , MediaLength
      ( MediaPx
      , MediaIn
      , MediaCm
      , MediaMm
      , MediaPt
      , MediaPc
      )
  , MediaOrientation
      ( Portrait
      , Landscape
      )
  , MediaResolution
      ( MediaDpi
      , MediaDpcm
      , MediaDppx
      )
  , MediaScanType
      ( Interlace
      , Progressive
      )
  , MediaColorScheme
      ( ColorNoPreference
      , ColorLight
      , ColorDark
      )
  , MediaPrefersContrast
      ( ContrastNoPreference
      , ContrastMore
      , ConstastLess
      , ContrastCustom
      )
  , MediaPointerAccuracy
      ( PointerNone
      , PointerCoarse
      , PointerFine
      )
  , MediaUpdateFrequency
      ( UpdateNone
      , UpdateSlow
      , UpdateFast
      )
  ) where

import Data.Bool qualified as B
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe (catMaybes)
import Data.Ratio (Ratio, denominator, numerator)
import Data.Text qualified as T
import Integer (Positive)

import Brigid.HTML.Types.Number (Number, numberToBytes, numberToText)
import Brigid.Internal.Render qualified as Render

data MediaQuery =
  MediaQuery
    { mediaQueryModifier :: Maybe MediaQueryModifier
    , mediaQueryType :: Maybe MediaQueryType
    , mediaQueryConditions :: [MediaFeature]
    } deriving (Eq, Show)

mediaQueryToBytes :: MediaQuery -> LBS.ByteString
mediaQueryToBytes query =
  LBS8.unwords $
    catMaybes
      [ mediaQueryModifierToBytes <$> mediaQueryModifier query
      , Just $
          Render.foldToBytesWithSeparator id " and " $
            catMaybes
              [ mediaQueryTypeToBytes <$> mediaQueryType query
              , case mediaQueryConditions query of
                  [] -> Nothing
                  conds ->
                    Just $
                      Render.foldToBytesWithSeparator
                        mediaFeatureToBytes
                        " and "
                        conds
              ]
      ]

mediaQueryToText :: MediaQuery -> T.Text
mediaQueryToText query =
  T.unwords $
    catMaybes
      [ mediaQueryModifierToText <$> mediaQueryModifier query
      , Just $
          Render.foldToTextWithSeparator id " and " $
            catMaybes
              [ mediaQueryTypeToText <$> mediaQueryType query
              , case mediaQueryConditions query of
                  [] -> Nothing
                  conds ->
                    Just $
                      Render.foldToTextWithSeparator
                        mediaFeatureToText
                        " and "
                        conds
              ]
      ]

data MediaQueryModifier
  = MediaNot
  | MediaOnly
  deriving (Bounded, Enum, Eq, Show)

mediaQueryModifierToBytes :: MediaQueryModifier -> LBS.ByteString
mediaQueryModifierToBytes mqm =
  case mqm of
    MediaNot -> "not"
    MediaOnly -> "only"

mediaQueryModifierToText :: MediaQueryModifier -> T.Text
mediaQueryModifierToText mqm =
  case mqm of
    MediaNot -> "not"
    MediaOnly -> "only"

data MediaQueryType
  = MediaAll
  | MediaScreen
  | MediaPrint
  | MediaSpeech
  deriving (Bounded, Enum, Eq, Show)

mediaQueryTypeToBytes :: MediaQueryType -> LBS.ByteString
mediaQueryTypeToBytes mt =
  case mt of
    MediaAll -> "all"
    MediaScreen -> "screen"
    MediaPrint -> "print"
    MediaSpeech -> "speech"

mediaQueryTypeToText :: MediaQueryType -> T.Text
mediaQueryTypeToText mt =
  case mt of
    MediaAll -> "all"
    MediaScreen -> "screen"
    MediaPrint -> "print"
    MediaSpeech -> "speech"

data MediaFeature
  = Width MediaFeatureType MediaLength
  | Height MediaFeatureType MediaLength
  | DeviceWidth MediaFeatureType MediaLength
  | DeviceHeight MediaFeatureType MediaLength
  | AspectRatio MediaFeatureType (Ratio Positive)
  | DeviceAspectRatio MediaFeatureType (Ratio Positive)
  | Orientation MediaOrientation
  | Resolution MediaFeatureType MediaResolution
  | Scan MediaScanType
  | Grid
  | Color (Maybe (MediaFeatureType, Positive)) -- Nothing means treat it as boolean type
  | ColorIndex (Maybe (MediaFeatureType, Positive)) -- Nothing means treat it as boolean type
  | Monochrome (Maybe (MediaFeatureType, Positive)) -- Nothing means treat it as boolean type
  | InvertedColors Bool
  | ForcedColors Bool
  | PrefersColorScheme MediaColorScheme
  | PrefersReducedMotion Bool
  | PrefersReducedTransparency Bool
  | PrefersContrast MediaPrefersContrast
  | PrefersReducedData Bool
  | Hover Bool
  | Pointer MediaPointerAccuracy
  | AnyHover Bool
  | AnyPointer MediaPointerAccuracy
  | Update MediaUpdateFrequency
  deriving (Eq, Show)

mediaFeatureToBytes :: MediaFeature -> LBS.ByteString
mediaFeatureToBytes mf =
  mconcat
    [ "("
    , case mf of
        Width typ l ->
          mediaFeatureTypeToBytes typ <> "width: " <> mediaLengthToBytes l

        Height typ l ->
          mediaFeatureTypeToBytes typ <> "height: " <> mediaLengthToBytes l

        DeviceWidth typ l ->
          mediaFeatureTypeToBytes typ
            <> "device-width: "
            <> mediaLengthToBytes l

        DeviceHeight typ l ->
          mediaFeatureTypeToBytes typ
            <> "device-height: "
            <> mediaLengthToBytes l

        AspectRatio typ ratio ->
          mediaFeatureTypeToBytes typ
            <> "aspect-ratio: "
            <> Render.showBytes (numerator ratio)
            <> "/"
            <> Render.showBytes (denominator ratio)

        DeviceAspectRatio typ ratio ->
          mediaFeatureTypeToBytes typ
            <> "device-aspect-ratio: "
            <> Render.showBytes (numerator ratio)
            <> "/"
            <> Render.showBytes (denominator ratio)

        Orientation o ->
          "orientation: " <> mediaOrientationToBytes o

        Resolution typ res ->
          mediaFeatureTypeToBytes typ
            <> "resolution: "
            <> mediaResolutionToBytes res

        Scan scan ->
          "scan: " <> mediaScanTypeToBytes scan

        Grid ->
          "grid"

        Color (Just (typ, val)) ->
          mediaFeatureTypeToBytes typ
            <> "color: "
            <> Render.showBytes val

        Color Nothing ->
          "color"

        ColorIndex (Just (typ, val)) ->
          mediaFeatureTypeToBytes typ
            <> "color-index: "
            <> Render.showBytes val

        ColorIndex Nothing ->
          "color-index"

        Monochrome (Just (typ, val)) ->
          mediaFeatureTypeToBytes typ
            <> "monochrome: "
            <> Render.showBytes val

        Monochrome Nothing ->
          "monochrome"

        InvertedColors invert ->
          "inverted-colors: " <> B.bool "none" "inverted" invert

        ForcedColors force ->
          "forced-colors: " <> B.bool "none" "active" force

        PrefersColorScheme scheme ->
          "prefers-color-scheme: " <> mediaColorSchemeToBytes scheme

        PrefersReducedMotion reduce ->
          "prefers-reduced-motion: " <> B.bool "no-preference" "reduce" reduce

        PrefersReducedTransparency reduce ->
          "prefers-reduced-transparency: "
            <> B.bool "no-preference" "reduce" reduce

        PrefersContrast contrast ->
          "prefers-contrast: " <> mediaPrefersContrastToBytes contrast

        PrefersReducedData reduce ->
          "prefers-reduced-data: " <> B.bool "no-preference" "reduce" reduce

        Hover hover ->
          "hover: " <> B.bool "none" "hover" hover

        Pointer pointer ->
          "pointer: " <> mediaPointerAccuracyToBytes pointer

        AnyHover hover ->
          "any-hover: " <> B.bool "none" "hover" hover

        AnyPointer pointer ->
          "any-pointer: " <> mediaPointerAccuracyToBytes pointer

        Update update ->
          "update: " <> mediaUpdateFrequencyToBytes update
    , ")"
    ]

mediaFeatureToText :: MediaFeature -> T.Text
mediaFeatureToText mf =
  mconcat
    [ "("
    , case mf of
        Width typ l ->
          mediaFeatureTypeToText typ <> "width: " <> mediaLengthToText l

        Height typ l ->
          mediaFeatureTypeToText typ <> "height: " <> mediaLengthToText l

        DeviceWidth typ l ->
          mediaFeatureTypeToText typ
            <> "device-width: "
            <> mediaLengthToText l

        DeviceHeight typ l ->
          mediaFeatureTypeToText typ
            <> "device-height: "
            <> mediaLengthToText l

        AspectRatio typ ratio ->
          mediaFeatureTypeToText typ
            <> "aspect-ratio: "
            <> Render.showText (numerator ratio)
            <> "/"
            <> Render.showText (denominator ratio)

        DeviceAspectRatio typ ratio ->
          mediaFeatureTypeToText typ
            <> "device-aspect-ratio: "
            <> Render.showText (numerator ratio)
            <> "/"
            <> Render.showText (denominator ratio)

        Orientation o ->
          "orientation: " <> mediaOrientationToText o

        Resolution typ res ->
          mediaFeatureTypeToText typ
            <> "resolution: "
            <> mediaResolutionToText res

        Scan scan ->
          "scan: " <> mediaScanTypeToText scan

        Grid ->
          "grid"

        Color (Just (typ, val)) ->
          mediaFeatureTypeToText typ
            <> "color: "
            <> Render.showText val

        Color Nothing ->
          "color"

        ColorIndex (Just (typ, val)) ->
          mediaFeatureTypeToText typ
            <> "color-index: "
            <> Render.showText val

        ColorIndex Nothing ->
          "color-index"

        Monochrome (Just (typ, val)) ->
          mediaFeatureTypeToText typ
            <> "monochrome: "
            <> Render.showText val

        Monochrome Nothing ->
          "monochrome"

        InvertedColors invert ->
          "inverted-colors: " <> B.bool "none" "inverted" invert

        ForcedColors force ->
          "forced-colors: " <> B.bool "none" "active" force

        PrefersColorScheme scheme ->
          "prefers-color-scheme: " <> mediaColorSchemeToText scheme

        PrefersReducedMotion reduce ->
          "prefers-reduced-motion: " <> B.bool "no-preference" "reduce" reduce

        PrefersReducedTransparency reduce ->
          "prefers-reduced-transparency: "
            <> B.bool "no-preference" "reduce" reduce

        PrefersContrast contrast ->
          "prefers-contrast: " <> mediaPrefersContrastToText contrast

        PrefersReducedData reduce ->
          "prefers-reduced-data: " <> B.bool "no-preference" "reduce" reduce

        Hover hover ->
          "hover: " <> B.bool "none" "hover" hover

        Pointer pointer ->
          "pointer: " <> mediaPointerAccuracyToText pointer

        AnyHover hover ->
          "any-hover: " <> B.bool "none" "hover" hover

        AnyPointer pointer ->
          "any-pointer: " <> mediaPointerAccuracyToText pointer

        Update update ->
          "update: " <> mediaUpdateFrequencyToText update
    , ")"
    ]

data MediaFeatureType
  = Min
  | Max
  | Exact
  deriving (Bounded, Enum, Eq, Show)

mediaFeatureTypeToBytes :: MediaFeatureType -> LBS.ByteString
mediaFeatureTypeToBytes mft =
  case mft of
    Min -> "min-"
    Max -> "max-"
    Exact -> LBS.empty

mediaFeatureTypeToText :: MediaFeatureType -> T.Text
mediaFeatureTypeToText mft =
  case mft of
    Min -> "min-"
    Max -> "max-"
    Exact -> T.empty

data MediaLength
  = MediaPx Number
  | MediaIn Number
  | MediaCm Number
  | MediaMm Number
  | MediaPt Number
  | MediaPc Number
  deriving (Eq, Show)

mediaLengthToBytes :: MediaLength -> LBS.ByteString
mediaLengthToBytes ml =
  case ml of
    MediaPx n -> numberToBytes n <> "px"
    MediaIn n -> numberToBytes n <> "in"
    MediaCm n -> numberToBytes n <> "cm"
    MediaMm n -> numberToBytes n <> "mm"
    MediaPt n -> numberToBytes n <> "pt"
    MediaPc n -> numberToBytes n <> "pc"

mediaLengthToText :: MediaLength -> T.Text
mediaLengthToText ml =
  case ml of
    MediaPx n -> numberToText n <> "px"
    MediaIn n -> numberToText n <> "in"
    MediaCm n -> numberToText n <> "cm"
    MediaMm n -> numberToText n <> "mm"
    MediaPt n -> numberToText n <> "pt"
    MediaPc n -> numberToText n <> "pc"

data MediaOrientation
  = Portrait
  | Landscape
  deriving (Bounded, Enum, Eq, Show)

mediaOrientationToBytes :: MediaOrientation -> LBS.ByteString
mediaOrientationToBytes mo =
  case mo of
    Portrait -> "portrait"
    Landscape -> "landscape"

mediaOrientationToText :: MediaOrientation -> T.Text
mediaOrientationToText mo =
  case mo of
    Portrait -> "portrait"
    Landscape -> "landscape"

data MediaResolution
  = MediaDpi Number
  | MediaDpcm Number
  | MediaDppx Number
  deriving (Eq, Show)

mediaResolutionToBytes :: MediaResolution -> LBS.ByteString
mediaResolutionToBytes mr =
  case mr of
    MediaDpi n -> numberToBytes n <> "dpi"
    MediaDpcm n -> numberToBytes n <> "dpcm"
    MediaDppx n -> numberToBytes n <> "dppx"

mediaResolutionToText :: MediaResolution -> T.Text
mediaResolutionToText mr =
  case mr of
    MediaDpi n -> numberToText n <> "dpi"
    MediaDpcm n -> numberToText n <> "dpcm"
    MediaDppx n -> numberToText n <> "dppx"

data MediaScanType
  = Interlace
  | Progressive
  deriving (Bounded, Enum, Eq, Show)

mediaScanTypeToBytes :: MediaScanType -> LBS.ByteString
mediaScanTypeToBytes mst =
  case mst of
    Interlace -> "interlace"
    Progressive -> "progressive"

mediaScanTypeToText :: MediaScanType -> T.Text
mediaScanTypeToText mst =
  case mst of
    Interlace -> "interlace"
    Progressive -> "progressive"

data MediaColorScheme
  = ColorNoPreference
  | ColorLight
  | ColorDark
  deriving (Bounded, Enum, Eq, Show)

mediaColorSchemeToBytes :: MediaColorScheme -> LBS.ByteString
mediaColorSchemeToBytes mcs =
  case mcs of
    ColorNoPreference -> "no-preference"
    ColorLight -> "light"
    ColorDark -> "dark"

mediaColorSchemeToText :: MediaColorScheme -> T.Text
mediaColorSchemeToText mcs =
  case mcs of
    ColorNoPreference -> "no-preference"
    ColorLight -> "light"
    ColorDark -> "dark"

data MediaPrefersContrast
  = ContrastNoPreference
  | ContrastMore
  | ConstastLess
  | ContrastCustom
  deriving (Bounded, Enum, Eq, Show)

mediaPrefersContrastToBytes :: MediaPrefersContrast -> LBS.ByteString
mediaPrefersContrastToBytes mpc =
  case mpc of
    ContrastNoPreference -> "no-preference"
    ContrastMore -> "more"
    ConstastLess -> "less"
    ContrastCustom -> "custom"

mediaPrefersContrastToText :: MediaPrefersContrast -> T.Text
mediaPrefersContrastToText mpc =
  case mpc of
    ContrastNoPreference -> "no-preference"
    ContrastMore -> "more"
    ConstastLess -> "less"
    ContrastCustom -> "custom"

data MediaPointerAccuracy
  = PointerNone
  | PointerCoarse
  | PointerFine
  deriving (Bounded, Enum, Eq, Show)

mediaPointerAccuracyToBytes :: MediaPointerAccuracy -> LBS.ByteString
mediaPointerAccuracyToBytes mpa =
  case mpa of
    PointerNone -> "none"
    PointerCoarse -> "coarse"
    PointerFine -> "fine"

mediaPointerAccuracyToText :: MediaPointerAccuracy -> T.Text
mediaPointerAccuracyToText mpa =
  case mpa of
    PointerNone -> "none"
    PointerCoarse -> "coarse"
    PointerFine -> "fine"

data MediaUpdateFrequency
  = UpdateNone
  | UpdateSlow
  | UpdateFast
  deriving (Bounded, Enum, Eq, Show)

mediaUpdateFrequencyToBytes :: MediaUpdateFrequency -> LBS.ByteString
mediaUpdateFrequencyToBytes muf =
  case muf of
    UpdateNone -> "none"
    UpdateSlow -> "slow"
    UpdateFast -> "fast"

mediaUpdateFrequencyToText :: MediaUpdateFrequency -> T.Text
mediaUpdateFrequencyToText muf =
  case muf of
    UpdateNone -> "none"
    UpdateSlow -> "slow"
    UpdateFast -> "fast"
