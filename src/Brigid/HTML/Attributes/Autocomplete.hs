{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Brigid.HTML.Attributes.Autocomplete
  ( ValidAutocomplete
  ) where

import Data.Kind (Type)
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Brigid.HTML.Elements.TagGroups qualified as TagGroups
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Elements.TagType (TagErrorMessage, TagType)
import Brigid.HTML.Internal.TagOperations (Add, Elem, Remove)
import Brigid.HTML.Types.AutocompleteToken qualified as ACT
import Brigid.HTML.Types.OnOff (OnOff (..))

type ValidAutocomplete token tag =
  AlertToken (Elem tag (ValidTagsFor token)) token tag ~ 'True

type family ValidTagsFor (token :: Type) :: [TagType] where
  ValidTagsFor OnOff                         = TagGroups.AutocompletableTags
  ValidTagsFor (ACT.Modifier nested)         = ValidTagsFor nested
  ValidTagsFor ACT.FullName                  = TextFields
  ValidTagsFor ACT.HonorificPrefix           = TextFields
  ValidTagsFor ACT.GivenName                 = TextFields
  ValidTagsFor ACT.AdditionalName            = TextFields
  ValidTagsFor ACT.FamilyName                = TextFields
  ValidTagsFor ACT.HonorificSuffix           = TextFields
  ValidTagsFor ACT.Nickname                  = TextFields
  ValidTagsFor ACT.Username                  = [ Tags.Input, Tags.InputEmail, Tags.InputText ]
  ValidTagsFor ACT.NewPassword               = [ Tags.Input, Tags.InputPassword ]
  ValidTagsFor ACT.CurrentPassword           = [ Tags.Input, Tags.InputPassword ]
  ValidTagsFor ACT.OneTimeCode               = Add Tags.InputNumber TelephoneFields
  ValidTagsFor ACT.Email                     = Add Tags.InputEmail TextFields
  ValidTagsFor ACT.InstantMessagingProtocol  = Add Tags.InputUrl TextFields
  ValidTagsFor ACT.Telephone                 = TelephoneFields
  ValidTagsFor ACT.TelephoneCountryCode      = TelephoneFields
  ValidTagsFor ACT.TelephoneNational         = TelephoneFields
  ValidTagsFor ACT.TelephoneAreaCode         = TelephoneFields
  ValidTagsFor ACT.TelephoneLocal            = TelephoneFields
  ValidTagsFor ACT.TelephoneLocalPrefix      = TelephoneFields
  ValidTagsFor ACT.TelephoneLocalSuffix      = TelephoneFields
  ValidTagsFor ACT.TelephoneExtension        = TelephoneFields
  ValidTagsFor ACT.Organization              = TextFields
  ValidTagsFor ACT.OrganizationTitle         = TextFields
  ValidTagsFor ACT.StreetAddress             = TextFields
  ValidTagsFor ACT.AddressLine1              = TextFields
  ValidTagsFor ACT.AddressLine2              = TextFields
  ValidTagsFor ACT.AddressLine3              = TextFields
  ValidTagsFor ACT.AddressLevel4             = TextFields
  ValidTagsFor ACT.AddressLevel3             = TextFields
  ValidTagsFor ACT.AddressLevel2             = TextFields
  ValidTagsFor ACT.AddressLevel1             = TextFields
  ValidTagsFor ACT.Country                   = Add Tags.Select TextFields
  ValidTagsFor ACT.CountryName               = Add Tags.Select TextFields
  ValidTagsFor ACT.PostalCode                = Add Tags.InputNumber TextFields
  ValidTagsFor ACT.CreditCardFullName        = TextFields
  ValidTagsFor ACT.CreditCardGivenName       = TextFields
  ValidTagsFor ACT.CreditCardAdditionalName  = TextFields
  ValidTagsFor ACT.CreditCardFamilyName      = TextFields
  ValidTagsFor ACT.CreditCardNumber          = [ Tags.Input, Tags.InputNumber, Tags.InputTel, Tags.InputText ]
  ValidTagsFor ACT.CreditCardExpiration      = [ Tags.Input, Tags.InputMonth, Tags.InputText ]
  ValidTagsFor ACT.CreditCardExpirationMonth = [ Tags.Input, Tags.InputNumber, Tags.InputText ]
  ValidTagsFor ACT.CreditCardExpirationYear  = [ Tags.Input, Tags.InputNumber, Tags.InputText ]
  ValidTagsFor ACT.CreditCardSecurityCode    = [ Tags.Input, Tags.InputPassword, Tags.InputText ]
  ValidTagsFor ACT.CreditCardType            = [ Tags.Input, Tags.InputText, Tags.Select ]
  ValidTagsFor ACT.TransactionCurrency       = Remove Tags.TextArea TextFields
  ValidTagsFor ACT.TransactionAmount         = [ Tags.Input, Tags.InputNumber, Tags.InputText ]
  ValidTagsFor ACT.Birthday                  = Remove Tags.InputNumber BirthdayFields
  ValidTagsFor ACT.BirthdayDay               = BirthdayFields
  ValidTagsFor ACT.BirthdayMonth             = BirthdayFields
  ValidTagsFor ACT.BirthdayYear              = BirthdayFields
  ValidTagsFor ACT.Language                  = Remove Tags.TextArea TextFields
  ValidTagsFor ACT.Sex                       = [ Tags.Input, Tags.InputText, Tags.Select ]
  ValidTagsFor ACT.Url                       = UrlFields
  ValidTagsFor ACT.Photo                     = UrlFields

type family AlertToken (member :: Bool) (token :: Type) (tag :: TagType) :: Bool where
  AlertToken 'True token tag =
    'True

  AlertToken 'False token tag =
    TypeError
      ( 'Text "The "
          ':<>: TokenTypeErrorMessage token
          ':<>: 'Text " autocomplete token type is not valid for the "
          ':<>: TagErrorMessage tag
          ':<>: 'Text " element."
      )

type family TokenTypeErrorMessage (token :: Type) :: ErrorMessage where
  TokenTypeErrorMessage OnOff                         = 'Text "On/Off"
  TokenTypeErrorMessage (ACT.Modifier nested)         = TokenTypeErrorMessage nested
  TokenTypeErrorMessage ACT.FullName                  = 'Text "FullName"
  TokenTypeErrorMessage ACT.HonorificPrefix           = 'Text "HonorificPrefix"
  TokenTypeErrorMessage ACT.GivenName                 = 'Text "GivenName"
  TokenTypeErrorMessage ACT.AdditionalName            = 'Text "AdditionalName"
  TokenTypeErrorMessage ACT.FamilyName                = 'Text "FamilyName"
  TokenTypeErrorMessage ACT.HonorificSuffix           = 'Text "HonorificSuffix"
  TokenTypeErrorMessage ACT.Nickname                  = 'Text "Nickname"
  TokenTypeErrorMessage ACT.Username                  = 'Text "Username"
  TokenTypeErrorMessage ACT.NewPassword               = 'Text "NewPassword"
  TokenTypeErrorMessage ACT.CurrentPassword           = 'Text "CurrentPassword"
  TokenTypeErrorMessage ACT.OneTimeCode               = 'Text "OneTimeCode"
  TokenTypeErrorMessage ACT.Email                     = 'Text "Email"
  TokenTypeErrorMessage ACT.InstantMessagingProtocol  = 'Text "InstantMessagingProtocol"
  TokenTypeErrorMessage ACT.Telephone                 = 'Text "Telephone"
  TokenTypeErrorMessage ACT.TelephoneCountryCode      = 'Text "TelephoneCountryCode"
  TokenTypeErrorMessage ACT.TelephoneNational         = 'Text "TelephoneNational"
  TokenTypeErrorMessage ACT.TelephoneAreaCode         = 'Text "TelephoneAreaCode"
  TokenTypeErrorMessage ACT.TelephoneLocal            = 'Text "TelephoneLocal"
  TokenTypeErrorMessage ACT.TelephoneLocalPrefix      = 'Text "TelephoneLocalPrefix"
  TokenTypeErrorMessage ACT.TelephoneLocalSuffix      = 'Text "TelephoneLocalSuffix"
  TokenTypeErrorMessage ACT.TelephoneExtension        = 'Text "TelephoneExtension"
  TokenTypeErrorMessage ACT.Organization              = 'Text "Organization"
  TokenTypeErrorMessage ACT.OrganizationTitle         = 'Text "OrganizationTitle"
  TokenTypeErrorMessage ACT.StreetAddress             = 'Text "StreetAddress"
  TokenTypeErrorMessage ACT.AddressLine1              = 'Text "AddressLine1"
  TokenTypeErrorMessage ACT.AddressLine2              = 'Text "AddressLine2"
  TokenTypeErrorMessage ACT.AddressLine3              = 'Text "AddressLine3"
  TokenTypeErrorMessage ACT.AddressLevel4             = 'Text "AddressLevel4"
  TokenTypeErrorMessage ACT.AddressLevel3             = 'Text "AddressLevel3"
  TokenTypeErrorMessage ACT.AddressLevel2             = 'Text "AddressLevel2"
  TokenTypeErrorMessage ACT.AddressLevel1             = 'Text "AddressLevel1"
  TokenTypeErrorMessage ACT.Country                   = 'Text "Country"
  TokenTypeErrorMessage ACT.CountryName               = 'Text "CountryName"
  TokenTypeErrorMessage ACT.PostalCode                = 'Text "PostalCode"
  TokenTypeErrorMessage ACT.CreditCardFullName        = 'Text "CreditCardFullName"
  TokenTypeErrorMessage ACT.CreditCardGivenName       = 'Text "CreditCardGivenName"
  TokenTypeErrorMessage ACT.CreditCardAdditionalName  = 'Text "CreditCardAdditionalName"
  TokenTypeErrorMessage ACT.CreditCardFamilyName      = 'Text "CreditCardFamilyName"
  TokenTypeErrorMessage ACT.CreditCardNumber          = 'Text "CreditCardNumber"
  TokenTypeErrorMessage ACT.CreditCardExpiration      = 'Text "CreditCardExpiration"
  TokenTypeErrorMessage ACT.CreditCardExpirationMonth = 'Text "CreditCardExpirationMonth"
  TokenTypeErrorMessage ACT.CreditCardExpirationYear  = 'Text "CreditCardExpirationYear"
  TokenTypeErrorMessage ACT.CreditCardSecurityCode    = 'Text "CreditCardSecurityCode"
  TokenTypeErrorMessage ACT.CreditCardType            = 'Text "CreditCardType"
  TokenTypeErrorMessage ACT.TransactionCurrency       = 'Text "TransactionCurrency"
  TokenTypeErrorMessage ACT.TransactionAmount         = 'Text "TransactionAmount"
  TokenTypeErrorMessage ACT.Birthday                  = 'Text "Birthday"
  TokenTypeErrorMessage ACT.BirthdayDay               = 'Text "BirthdayDay"
  TokenTypeErrorMessage ACT.BirthdayMonth             = 'Text "BirthdayMonth"
  TokenTypeErrorMessage ACT.BirthdayYear              = 'Text "BirthdayYear"
  TokenTypeErrorMessage ACT.Language                  = 'Text "Language"
  TokenTypeErrorMessage ACT.Sex                       = 'Text "Sex"
  TokenTypeErrorMessage ACT.Url                       = 'Text "Url"
  TokenTypeErrorMessage ACT.Photo                     = 'Text "Photo"

type BirthdayFields =
  [ Tags.Input
  , Tags.InputNumber
  , Tags.InputText
  ]

type TelephoneFields =
  [ Tags.Input
  , Tags.InputTel
  , Tags.InputText
  ]

type TextFields =
  [ Tags.Input
  , Tags.InputText
  , Tags.TextArea
  ]

type UrlFields =
  [ Tags.Input
  , Tags.InputText
  , Tags.InputUrl
  ]
