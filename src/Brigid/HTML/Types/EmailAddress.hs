module Brigid.HTML.Types.EmailAddress
  ( EmailAddress
  , emailAddressFromText
  , emailAddressToBytes
  , emailAddressToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.Html.Email.Validate qualified as Validate

newtype EmailAddress =
  EmailAddress
    { emailAddressToText :: T.Text
    } deriving (Eq, Show)

emailAddressFromText :: T.Text -> Either String EmailAddress
emailAddressFromText txt =
  case Validate.parseEmail txt of
    Left _err   -> Left $ "Invalid email address: " <> T.unpack txt
    Right email -> Right . EmailAddress $ Validate.emailToText email

emailAddressToBytes :: EmailAddress -> LBS.ByteString
emailAddressToBytes = LBS.fromStrict . TE.encodeUtf8 . emailAddressToText
