module HTML.Types.URL
  ( AbsoluteURL
  , absoluteURLFromText
  , absoluteURLToText
  , RelativeURL
  , relativeURLFromRoute
  , relativeURLToText
  , relativeURLMethod
  ) where

import Beeline.HTTP.Client qualified as B
import Beeline.Routing qualified as R
import Data.Text qualified as T
import Network.HTTP.Types qualified as HTTP

newtype AbsoluteURL = AbsoluteURL B.BaseURI

absoluteURLFromText :: T.Text -> Either String AbsoluteURL
absoluteURLFromText =
  fmap AbsoluteURL . B.parseBaseURI . T.unpack

absoluteURLToText :: AbsoluteURL -> T.Text
absoluteURLToText (AbsoluteURL url) =
  T.pack $ B.renderBaseURI url

data RelativeURL =
  RelativeURL
    { relativeURLMethod :: HTTP.StdMethod
    , relativeURLPath   :: T.Text
    }

relativeURLFromRoute :: R.RouteGenerator route -> route -> RelativeURL
relativeURLFromRoute generator =
  uncurry RelativeURL . R.generateRoute generator

relativeURLToText :: RelativeURL -> T.Text
relativeURLToText =
  relativeURLPath
