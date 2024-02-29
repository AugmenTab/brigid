module HTML.Types.Email
  ( Email
  , emailFromText
  , emailToText
  ) where

import Data.Text qualified as T
import Text.Html.Email.Validate qualified as Validate

newtype Email = Email T.Text

emailFromText :: T.Text -> Either String Email
emailFromText txt =
  case Validate.parseEmail txt of
    Left _err -> Left $ "Invalid email address: " <> T.unpack txt
    Right email -> Right . Email $ Validate.emailToText email

emailToText :: Email -> T.Text
emailToText (Email email) = email
