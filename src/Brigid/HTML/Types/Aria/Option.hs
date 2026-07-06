module Brigid.HTML.Types.Aria.Option
  ( AriaAutocompleteOption
      ( AutocompleteInline
      , AutocompleteList
      , AutocompleteBoth
      , AutocompleteNone
      )
  , ariaAutocompleteOptionToBytes
  , ariaAutocompleteOptionToBytesBuilder
  , ariaAutocompleteOptionToText
  , AriaLiveOption
      ( LiveAssertive
      , LivePolite
      , LiveOff
      )
  , ariaLiveOptionToBytes
  , ariaLiveOptionToBytesBuilder
  , ariaLiveOptionToText
  , AriaRelevantOption
      ( RelevantAll
      , RelevantAdditions
      , RelevantRemovals
      , RelevantText
      )
  , ariaRelevantOptionToBytes
  , ariaRelevantOptionToBytesBuilder
  , ariaRelevantOptionToText
  , AriaSortOption
      ( SortAscending
      , SortDescending
      , SortNone
      , SortOther
      )
  , ariaSortOptionToBytes
  , ariaSortOptionToBytesBuilder
  , ariaSortOptionToText
  ) where

import Data.ByteString.Builder (Builder, lazyByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T

data AriaAutocompleteOption
  = AutocompleteInline
  | AutocompleteList
  | AutocompleteBoth
  | AutocompleteNone
  deriving (Bounded, Enum, Eq, Show)

ariaAutocompleteOptionToBytes :: AriaAutocompleteOption -> LBS.ByteString
ariaAutocompleteOptionToBytes option =
  case option of
    AutocompleteInline -> "inline"
    AutocompleteList -> "list"
    AutocompleteBoth -> "both"
    AutocompleteNone -> "none"

ariaAutocompleteOptionToBytesBuilder :: AriaAutocompleteOption -> Builder
{-# INLINE ariaAutocompleteOptionToBytesBuilder #-}
ariaAutocompleteOptionToBytesBuilder = lazyByteString . ariaAutocompleteOptionToBytes

ariaAutocompleteOptionToText :: AriaAutocompleteOption -> T.Text
ariaAutocompleteOptionToText option =
  case option of
    AutocompleteInline -> "inline"
    AutocompleteList -> "list"
    AutocompleteBoth -> "both"
    AutocompleteNone -> "none"

data AriaLiveOption
  = LiveAssertive
  | LivePolite
  | LiveOff
  deriving (Bounded, Enum, Eq, Show)

ariaLiveOptionToBytes :: AriaLiveOption -> LBS.ByteString
ariaLiveOptionToBytes option =
  case option of
    LiveAssertive -> "assertive"
    LivePolite -> "polite"
    LiveOff -> "off"

ariaLiveOptionToBytesBuilder :: AriaLiveOption -> Builder
{-# INLINE ariaLiveOptionToBytesBuilder #-}
ariaLiveOptionToBytesBuilder = lazyByteString . ariaLiveOptionToBytes

ariaLiveOptionToText :: AriaLiveOption -> T.Text
ariaLiveOptionToText option =
  case option of
    LiveAssertive -> "assertive"
    LivePolite -> "polite"
    LiveOff -> "off"

data AriaRelevantOption
  = RelevantAll
  | RelevantAdditions
  | RelevantRemovals
  | RelevantText
  deriving (Bounded, Enum, Eq, Show)

ariaRelevantOptionToBytes :: AriaRelevantOption -> LBS.ByteString
ariaRelevantOptionToBytes option =
  case option of
    RelevantAll -> "all"
    RelevantAdditions -> "additions"
    RelevantRemovals -> "removals"
    RelevantText -> "text"

ariaRelevantOptionToBytesBuilder :: AriaRelevantOption -> Builder
{-# INLINE ariaRelevantOptionToBytesBuilder #-}
ariaRelevantOptionToBytesBuilder = lazyByteString . ariaRelevantOptionToBytes

ariaRelevantOptionToText :: AriaRelevantOption -> T.Text
ariaRelevantOptionToText option =
  case option of
    RelevantAll -> "all"
    RelevantAdditions -> "additions"
    RelevantRemovals -> "removals"
    RelevantText -> "text"

data AriaSortOption
  = SortAscending
  | SortDescending
  | SortNone
  | SortOther
  deriving (Bounded, Enum, Eq, Show)

ariaSortOptionToBytes :: AriaSortOption -> LBS.ByteString
ariaSortOptionToBytes option =
  case option of
    SortAscending -> "ascending"
    SortDescending -> "descending"
    SortNone -> "none"
    SortOther -> "other"

ariaSortOptionToBytesBuilder :: AriaSortOption -> Builder
{-# INLINE ariaSortOptionToBytesBuilder #-}
ariaSortOptionToBytesBuilder = lazyByteString . ariaSortOptionToBytes

ariaSortOptionToText :: AriaSortOption -> T.Text
ariaSortOptionToText option =
  case option of
    SortAscending -> "ascending"
    SortDescending -> "descending"
    SortNone -> "none"
    SortOther -> "other"
