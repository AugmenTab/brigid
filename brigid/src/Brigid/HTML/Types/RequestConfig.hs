{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.RequestConfig
  ( HtmxRequest
  , HtmxRequestTypes
  , mkHtmxRequest
  , htmxRequestToBytes
  , htmxRequestToText
  , htmxRequestToTextBuilder
  , RequestConfig (..)
  , emptyRequestConfig
  , requestConfigToBytes
  , requestConfigToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import GHC.TypeLits (KnownNat)
import Numeric.Natural (Natural)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.Internal.Render qualified as Render
import Brigid.Types.RawJavaScript qualified as JS

newtype HtmxRequest =
  HtmxRequest (Shrubbery.Union HtmxRequestTypes)
    deriving (Eq, Show)

type HtmxRequestTypes =
  [ RequestConfig
  , JS.RawJavaScript
  ]

mkHtmxRequest :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf request HtmxRequestTypes
                 )
              => request -> HtmxRequest
mkHtmxRequest =
  HtmxRequest . Shrubbery.unify

htmxRequestToBytes :: HtmxRequest -> LBS.ByteString
htmxRequestToBytes (HtmxRequest request) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @RequestConfig    requestConfigToBytes
      . Shrubbery.branch @JS.RawJavaScript JS.rawJavaScriptToBytes
      $ Shrubbery.branchEnd
  ) request

htmxRequestToText :: HtmxRequest -> T.Text
htmxRequestToText (HtmxRequest request) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @RequestConfig    requestConfigToText
      . Shrubbery.branch @JS.RawJavaScript JS.rawJavaScriptToText
      $ Shrubbery.branchEnd
  ) request

htmxRequestToTextBuilder :: HtmxRequest -> TBL.Builder
htmxRequestToTextBuilder = TBL.fromText . htmxRequestToText

data RequestConfig =
  RequestConfig
    { requestConfigTimeout     :: Maybe Natural
    , requestConfigCredentials :: Maybe Bool
    , requestConfigNoHeaders   :: Maybe Bool
    } deriving (Eq, Show)

emptyRequestConfig :: RequestConfig
emptyRequestConfig =
  RequestConfig
    { requestConfigTimeout     = Nothing
    , requestConfigCredentials = Nothing
    , requestConfigNoHeaders   = Nothing
    }

requestConfigToBytes :: RequestConfig -> LBS.ByteString
requestConfigToBytes config =
  LBS.concat
    [ "{"
    , LBS.intercalate ","
        $ catMaybes
            [ (\timeout -> "\"timeout\":" <> Render.showBytes timeout)
                <$> requestConfigTimeout config
            , (\credentials -> "\"credentials\":" <> Render.enumBoolToBytes credentials)
                <$> requestConfigCredentials config
            , (\noHeaders -> "\"noHeaders\":" <> Render.enumBoolToBytes noHeaders)
                <$> requestConfigNoHeaders config
            ]
    , "}"
    ]

requestConfigToText :: RequestConfig -> T.Text
requestConfigToText config =
  T.concat
    [ "{"
    , T.intercalate ","
        $ catMaybes
            [ (\timeout -> "\"timeout\":" <> Render.showText timeout)
                <$> requestConfigTimeout config
            , (\credentials -> "\"credentials\":" <> Render.enumBoolToText credentials)
                <$> requestConfigCredentials config
            , (\noHeaders -> "\"noHeaders\":" <> Render.enumBoolToText noHeaders)
                <$> requestConfigNoHeaders config
            ]
    , "}"
    ]
