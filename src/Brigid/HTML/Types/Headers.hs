{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.Headers
  ( HtmxHeaders
  , HtmxHeadersTypes
  , mkHtmxHeaders
  , htmxHeadersToBytes
  , htmxHeadersToText
  , RequestHeaders (..)
  , emptyRequestHeaders
  , requestHeadersToBytes
  , requestHeadersToText
  , CustomRequestHeader (..)
  ) where

import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.RawJavaScript qualified as JS
import Brigid.Internal.Render qualified as Render

newtype HtmxHeaders = HtmxHeaders (Shrubbery.Union HtmxHeadersTypes)

type HtmxHeadersTypes =
  [ RequestHeaders
  , JS.RawJavaScript
  ]

mkHtmxHeaders :: ( KnownNat branchIndex
                 , branchIndex ~ FirstIndexOf headers HtmxHeadersTypes
                 )
              => headers -> HtmxHeaders
mkHtmxHeaders =
  HtmxHeaders . Shrubbery.unify

htmxHeadersToBytes :: HtmxHeaders -> LBS.ByteString
htmxHeadersToBytes (HtmxHeaders headers) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @RequestHeaders   requestHeadersToBytes
      . Shrubbery.branch @JS.RawJavaScript (("js:" <>) . JS.rawJavaScriptToBytes)
      $ Shrubbery.branchEnd
  ) headers

htmxHeadersToText :: HtmxHeaders -> T.Text
htmxHeadersToText (HtmxHeaders headers) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @RequestHeaders   requestHeadersToText
      . Shrubbery.branch @JS.RawJavaScript (("js:" <>) . JS.rawJavaScriptToText)
      $ Shrubbery.branchEnd
  ) headers

data RequestHeaders =
  RequestHeaders
    { customHeaders               :: Maybe (NEL.NonEmpty CustomRequestHeader)
 -- , aIM                         :: Maybe _
 -- , accept                      :: Maybe _
 -- , acceptCharset               :: Maybe _
 -- , acceptEncoding              :: Maybe _
 -- , acceptLanguage              :: Maybe _
 -- , acceptDatetime              :: Maybe _
 -- , accessControlRequestMethod  :: Maybe _
 -- , accessControlRequestHeaders :: Maybe _
 -- , authorization               :: Maybe _
 -- , cacheControl                :: Maybe _
 -- , connection                  :: Maybe _
 -- , contentLength               :: Maybe _
 -- , contentType                 :: Maybe _
 -- , cookie                      :: Maybe _
 -- , date                        :: Maybe _
 -- , expect                      :: Maybe _
 -- , forwarded                   :: Maybe _
 -- , from                        :: Maybe _
 -- , host                        :: Maybe _
 -- , ifMatch                     :: Maybe _
 -- , ifModifiedSince             :: Maybe _
 -- , ifNoneMatch                 :: Maybe _
 -- , ifRange                     :: Maybe _
 -- , ifUnmodifiedSince           :: Maybe _
 -- , maxForwards                 :: Maybe _
 -- , origin                      :: Maybe _
 -- , pragma                      :: Maybe _
 -- , proxyAuthorization          :: Maybe _
 -- , range                       :: Maybe _
 -- , referrer                    :: Maybe _
 -- , tE                          :: Maybe _
 -- , userAgent                   :: Maybe _
 -- , upgrade                     :: Maybe _
 -- , via                         :: Maybe _
 -- , warning                     :: Maybe _
 -- , dnt                         :: Maybe _
 -- , xRequestedWith              :: Maybe _
 -- , xCSRFToken                  :: Maybe _
 -- , hXBoosted                   :: Maybe _
 -- , hXCurrentURL                :: Maybe _
 -- , hXHistoryRestoreRequest     :: Maybe _
 -- , hXPrompt                    :: Maybe _
 -- , hXRequest                   :: Maybe _
 -- , hXTarget                    :: Maybe _
 -- , hXTriggerName               :: Maybe _
 -- , hXTrigger                   :: Maybe _
    }

emptyRequestHeaders :: RequestHeaders
emptyRequestHeaders =
  RequestHeaders
    { customHeaders               = Nothing
 -- , aIM                         = Nothing
 -- , accept                      = Nothing
 -- , acceptCharset               = Nothing
 -- , acceptEncoding              = Nothing
 -- , acceptLanguage              = Nothing
 -- , acceptDatetime              = Nothing
 -- , accessControlRequestMethod  = Nothing
 -- , accessControlRequestHeaders = Nothing
 -- , authorization               = Nothing
 -- , cacheControl                = Nothing
 -- , connection                  = Nothing
 -- , contentLength               = Nothing
 -- , contentType                 = Nothing
 -- , cookie                      = Nothing
 -- , date                        = Nothing
 -- , expect                      = Nothing
 -- , forwarded                   = Nothing
 -- , from                        = Nothing
 -- , host                        = Nothing
 -- , ifMatch                     = Nothing
 -- , ifModifiedSince             = Nothing
 -- , ifNoneMatch                 = Nothing
 -- , ifRange                     = Nothing
 -- , ifUnmodifiedSince           = Nothing
 -- , maxForwards                 = Nothing
 -- , origin                      = Nothing
 -- , pragma                      = Nothing
 -- , proxyAuthorization          = Nothing
 -- , range                       = Nothing
 -- , referrer                    = Nothing
 -- , tE                          = Nothing
 -- , userAgent                   = Nothing
 -- , upgrade                     = Nothing
 -- , via                         = Nothing
 -- , warning                     = Nothing
 -- , dnt                         = Nothing
 -- , xRequestedWith              = Nothing
 -- , xCSRFToken                  = Nothing
 -- , hXBoosted                   = Nothing
 -- , hXCurrentURL                = Nothing
 -- , hXHistoryRestoreRequest     = Nothing
 -- , hXPrompt                    = Nothing
 -- , hXRequest                   = Nothing
 -- , hXTarget                    = Nothing
 -- , hXTriggerName               = Nothing
 -- , hXTrigger                   = Nothing
    }

requestHeadersToByteStringBuilder :: RequestHeaders -> BSB.Builder
requestHeadersToByteStringBuilder headers =
  let empty = BSB.lazyByteString LBS.empty
      wrap b = BSB.lazyByteString "{" <> b <> BSB.lazyByteString "}"
      renderCustomHeaders =
        fold
          . intersperse (BSB.lazyByteString ", ")
          . fmap (BSB.lazyByteString . customRequestHeaderToBytes)
          . NEL.toList

   in wrap
        . fold
        . intersperse (BSB.lazyByteString ", ")
        $ [ maybe empty renderCustomHeaders $ customHeaders headers
          ]

requestHeadersToBytes :: RequestHeaders -> LBS.ByteString
requestHeadersToBytes =
  BSB.toLazyByteString . requestHeadersToByteStringBuilder

requestHeadersToTextBuilder :: RequestHeaders -> TLB.Builder
requestHeadersToTextBuilder headers =
  let empty = TLB.fromText T.empty
      wrap b = TLB.fromText "{" <> b <> TLB.fromText "}"
      renderCustomHeaders =
        fold
          . intersperse (TLB.fromText ", ")
          . fmap (TLB.fromText . customRequestHeaderToText)
          . NEL.toList

   in wrap
        . fold
        . intersperse (TLB.fromText ", ")
        $ [ maybe empty renderCustomHeaders $ customHeaders headers
          ]

requestHeadersToText :: RequestHeaders -> T.Text
requestHeadersToText =
  TL.toStrict . TLB.toLazyText . requestHeadersToTextBuilder

data CustomRequestHeader =
  CustomRequestHeader
    { customRequestHeaderName  :: T.Text
    , customRequestHeaderValue :: T.Text
    }

customRequestHeaderToBytes :: CustomRequestHeader -> LBS.ByteString
customRequestHeaderToBytes header =
  LBS.concat
    [ "\""
    , Render.textToBytes $ customRequestHeaderName header
    , "\" : \""
    , Render.textToBytes $ customRequestHeaderValue header
    , "\""
    ]
customRequestHeaderToText :: CustomRequestHeader -> T.Text
customRequestHeaderToText header =
  T.concat
    [ T.singleton '"'
    , customRequestHeaderName header
    , "\": \""
    , customRequestHeaderValue header
    , "\""
    ]
