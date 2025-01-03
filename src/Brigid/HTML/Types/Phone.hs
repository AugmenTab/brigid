module Brigid.HTML.Types.Phone
  ( PhoneNumber
  , phoneNumberFromText
  , phoneNumberToBytes
  , phoneNumberToText
  ) where

import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.ISO3166_CountryCodes (CountryCode)
import Data.PhoneNumber.Number (PhoneNumber)
import Data.PhoneNumber.Util qualified as PhoneUtil
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

phoneNumberFromText :: Maybe CountryCode -> T.Text -> Either String PhoneNumber
phoneNumberFromText mbCountry txt =
  let mbRegion = PhoneUtil.Region . BS8.pack . show <$> mbCountry
   in case PhoneUtil.parseNumber PhoneUtil.Canonicalize mbRegion $ TE.encodeUtf8 txt of
        Left err ->
          Left $
            case err of
              PhoneUtil.InvalidCountryCodeError ->
                unwords
                  [ "The number did not contain a country code and there was no"
                  , "default region supplied, or the number contained an"
                  , "invalid country code."
                  ]

              PhoneUtil.NotANumber ->
                "Does not look like a phone number."

              PhoneUtil.TooShortAfterIdd ->
                unwords
                  [ "Input starts with an International Direct Dialing prefix,"
                  , "but ends too shortly thereafter."
                  ]

              PhoneUtil.TooShortNsn ->
                "The National Significant Number is too short."

              PhoneUtil.TooLongNsn ->
                "The National Significant Number is too long."

        Right phone ->
          Right phone

phoneNumberToBytes :: PhoneNumber -> LBS.ByteString
phoneNumberToBytes =
  LBS.fromStrict . PhoneUtil.formatNumber PhoneUtil.International

phoneNumberToText :: PhoneNumber -> T.Text
phoneNumberToText =
  TE.decodeUtf8 . LBS.toStrict . phoneNumberToBytes
