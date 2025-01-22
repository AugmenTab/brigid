module Brigid.HTML.Types.EmailAddress
  ( EmailAddress
  , emailAddressFromText
  , emailAddressToBytes
  , emailAddressToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Text.Html.Email.Validate qualified as Validate

import Brigid.Internal.Render qualified as Render

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
emailAddressToBytes = Render.textToBytes . emailAddressToText
