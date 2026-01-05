{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Brigid.HTML.Types.Aria.Current
  ( AriaCurrent (..)
  , AriaCurrentTypes
  , ariaCurrentToBytes
  , ariaCurrentToText
  , CurrentPage (CurrentPage)
  , CurrentStep (CurrentStep)
  , CurrentLocation (CurrentLocation)
  , CurrentDate (CurrentDate)
  , CurrentTime (CurrentTime)
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Shrubbery qualified

import Brigid.Internal.Render qualified as Render

newtype AriaCurrent =
  AriaCurrent
    { unAriaCurrent :: Shrubbery.Union AriaCurrentTypes
    } deriving (Eq, Show)

type AriaCurrentTypes =
  [ CurrentPage
  , CurrentStep
  , CurrentLocation
  , CurrentDate
  , CurrentTime
  , Bool
  ]

ariaCurrentToBytes :: AriaCurrent -> LBS.ByteString
ariaCurrentToBytes (AriaCurrent aria) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @CurrentPage ariaPageToBytes
      . Shrubbery.branch @CurrentStep ariaStepToBytes
      . Shrubbery.branch @CurrentLocation ariaLocationToBytes
      . Shrubbery.branch @CurrentDate ariaDateToBytes
      . Shrubbery.branch @CurrentTime ariaTimeToBytes
      . Shrubbery.branch @Bool Render.enumBoolToBytes
      $ Shrubbery.branchEnd
  ) aria

ariaCurrentToText :: AriaCurrent -> T.Text
ariaCurrentToText (AriaCurrent aria) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @CurrentPage ariaPageToText
      . Shrubbery.branch @CurrentStep ariaStepToText
      . Shrubbery.branch @CurrentLocation ariaLocationToText
      . Shrubbery.branch @CurrentDate ariaDateToText
      . Shrubbery.branch @CurrentTime ariaTimeToText
      . Shrubbery.branch @Bool Render.enumBoolToText
      $ Shrubbery.branchEnd
  ) aria

data CurrentDate = CurrentDate
  deriving (Eq, Show)

ariaDateToBytes :: CurrentDate -> LBS.ByteString
ariaDateToBytes CurrentDate = "date"

ariaDateToText :: CurrentDate -> T.Text
ariaDateToText CurrentDate = "date"

data CurrentLocation = CurrentLocation
  deriving (Eq, Show)

ariaLocationToBytes :: CurrentLocation -> LBS.ByteString
ariaLocationToBytes CurrentLocation = "location"

ariaLocationToText :: CurrentLocation -> T.Text
ariaLocationToText CurrentLocation = "location"

data CurrentPage = CurrentPage
  deriving (Eq, Show)

ariaPageToBytes :: CurrentPage -> LBS.ByteString
ariaPageToBytes CurrentPage = "page"

ariaPageToText :: CurrentPage -> T.Text
ariaPageToText CurrentPage = "page"

data CurrentStep = CurrentStep
  deriving (Eq, Show)

ariaStepToBytes :: CurrentStep -> LBS.ByteString
ariaStepToBytes CurrentStep = "step"

ariaStepToText :: CurrentStep -> T.Text
ariaStepToText CurrentStep = "step"

data CurrentTime = CurrentTime
  deriving (Eq, Show)

ariaTimeToBytes :: CurrentTime -> LBS.ByteString
ariaTimeToBytes CurrentTime = "time"

ariaTimeToText :: CurrentTime -> T.Text
ariaTimeToText CurrentTime = "time"
