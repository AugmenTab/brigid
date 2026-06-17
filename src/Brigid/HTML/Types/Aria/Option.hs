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

import Data.ByteString.Builder (Builder, string8)
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
ariaAutocompleteOptionToBytesBuilder option =
  case option of
    AutocompleteInline -> string8 "inline"
    AutocompleteList   -> string8 "list"
    AutocompleteBoth   -> string8 "both"
    AutocompleteNone   -> string8 "none"

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
ariaLiveOptionToBytesBuilder option =
  case option of
    LiveAssertive -> string8 "assertive"
    LivePolite    -> string8 "polite"
    LiveOff       -> string8 "off"

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
ariaRelevantOptionToBytesBuilder option =
  case option of
    RelevantAll       -> string8 "all"
    RelevantAdditions -> string8 "additions"
    RelevantRemovals  -> string8 "removals"
    RelevantText      -> string8 "text"

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
ariaSortOptionToBytesBuilder option =
  case option of
    SortAscending  -> string8 "ascending"
    SortDescending -> string8 "descending"
    SortNone       -> string8 "none"
    SortOther      -> string8 "other"

ariaSortOptionToText :: AriaSortOption -> T.Text
ariaSortOptionToText option =
  case option of
    SortAscending -> "ascending"
    SortDescending -> "descending"
    SortNone -> "none"
    SortOther -> "other"
