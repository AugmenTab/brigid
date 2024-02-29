module HTML.Types.URL
  ( AbsoluteURL
  , absoluteURLFromText
  , absoluteURLToText
  ) where

import Beeline.HTTP.Client qualified as B
import Data.Text qualified as T

newtype AbsoluteURL = AbsoluteURL B.BaseURI

absoluteURLFromText :: T.Text -> Either String AbsoluteURL
absoluteURLFromText =
  fmap AbsoluteURL . B.parseBaseURI . T.unpack

absoluteURLToText :: AbsoluteURL -> T.Text
absoluteURLToText (AbsoluteURL url) =
  T.pack $ B.renderBaseURI url
