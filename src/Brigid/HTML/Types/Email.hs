module Brigid.HTML.Types.Email
  ( Email
  , emailFromText
  , emailToBytes
  , emailToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.Html.Email.Validate qualified as Validate

newtype Email = Email T.Text
  deriving (Eq, Show)

emailFromText :: T.Text -> Either String Email
emailFromText txt =
  case Validate.parseEmail txt of
    Left _err -> Left $ "Invalid email address: " <> T.unpack txt
    Right email -> Right . Email $ Validate.emailToText email

emailToBytes :: Email -> LBS.ByteString
emailToBytes = LBS.fromStrict . TE.encodeUtf8 . emailToText

emailToText :: Email -> T.Text
emailToText (Email email) = email
