{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.AutocompleteToken
  ( AutocompleteToken
  , AutocompleteTokenTypes
  , mkAutocompleteToken
  , autocompleteTokenToBytes
  , autocompleteTokenToText
  , Off (Off)
  , On (On)
  , Section
  , section
  , Home
  , home
  , Work
  , work
  , Shipping
  , shipping
  , Billing
  , billing
  , Mobile
  , mobile
  , Fax
  , fax
  , Pager
  , pager
  , FullName (FullName)
  , HonorificPrefix (HonorificPrefix)
  , GivenName (GivenName)
  , AdditionalName (AdditionalName)
  , FamilyName (FamilyName)
  , HonorificSuffix (HonorificSuffix)
  , Nickname (Nickname)
  , Username (Username)
  , NewPassword (NewPassword)
  , CurrentPassword (CurrentPassword)
  , OneTimeCode (OneTimeCode)
  , Email (Email)
  , InstantMessagingProtocol (InstantMessagingProtocol)
  , Telephone (Telephone)
  , TelephoneCountryCode (TelephoneCountryCode)
  , TelephoneNational (TelephoneNational)
  , TelephoneAreaCode (TelephoneAreaCode)
  , TelephoneLocal (TelephoneLocal)
  , TelephoneLocalPrefix (TelephoneLocalPrefix)
  , TelephoneLocalSuffix (TelephoneLocalSuffix)
  , TelephoneExtension (TelephoneExtension)
  , Organization (Organization)
  , OrganizationTitle (OrganizationTitle)
  , StreetAddress (StreetAddress)
  , AddressLine1 (AddressLine1)
  , AddressLine2 (AddressLine2)
  , AddressLine3 (AddressLine3)
  , AddressLevel4 (AddressLevel4)
  , AddressLevel3 (AddressLevel3)
  , AddressLevel2 (AddressLevel2)
  , AddressLevel1 (AddressLevel1)
  , Country (Country)
  , CountryName (CountryName)
  , PostalCode (PostalCode)
  , CreditCardFullName (CreditCardFullName)
  , CreditCardGivenName (CreditCardGivenName)
  , CreditCardAdditionalName (CreditCardAdditionalName)
  , CreditCardFamilyName (CreditCardFamilyName)
  , CreditCardNumber (CreditCardNumber)
  , CreditCardExpiration (CreditCardExpiration)
  , CreditCardExpirationMonth (CreditCardExpirationMonth)
  , CreditCardExpirationYear (CreditCardExpirationYear)
  , CreditCardSecurityCode (CreditCardSecurityCode)
  , CreditCardType (CreditCardType)
  , TransactionCurrency (TransactionCurrency)
  , TransactionAmount (TransactionAmount)
  , Birthday (Birthday)
  , BirthdayDay (BirthdayDay)
  , BirthdayMonth (BirthdayMonth)
  , BirthdayYear (BirthdayYear)
  , Language (Language)
  , Sex (Sex)
  , Url (Url)
  , Photo (Photo)
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

newtype AutocompleteToken =
  AutocompleteToken (Shrubbery.Union AutocompleteTokenTypes)

instance Show AutocompleteToken where
  show (AutocompleteToken token) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @Off                                 show
        . Shrubbery.branch @On                                  show
        . Shrubbery.branch @(Section Home)                      show
        . Shrubbery.branch @(Section Work)                      show
        . Shrubbery.branch @(Section Shipping)                  show
        . Shrubbery.branch @(Section Billing)                   show
        . Shrubbery.branch @(Section Mobile)                    show
        . Shrubbery.branch @(Section Fax)                       show
        . Shrubbery.branch @(Section Pager)                     show
        . Shrubbery.branch @(Section FullName)                  show
        . Shrubbery.branch @(Section HonorificPrefix)           show
        . Shrubbery.branch @(Section GivenName)                 show
        . Shrubbery.branch @(Section AdditionalName)            show
        . Shrubbery.branch @(Section FamilyName)                show
        . Shrubbery.branch @(Section HonorificSuffix)           show
        . Shrubbery.branch @(Section Nickname)                  show
        . Shrubbery.branch @(Section Username)                  show
        . Shrubbery.branch @(Section NewPassword)               show
        . Shrubbery.branch @(Section CurrentPassword)           show
        . Shrubbery.branch @(Section OneTimeCode)               show
        . Shrubbery.branch @(Section Email)                     show
        . Shrubbery.branch @(Section InstantMessagingProtocol)  show
        . Shrubbery.branch @(Section Telephone)                 show
        . Shrubbery.branch @(Section TelephoneCountryCode)      show
        . Shrubbery.branch @(Section TelephoneNational)         show
        . Shrubbery.branch @(Section TelephoneAreaCode)         show
        . Shrubbery.branch @(Section TelephoneLocal)            show
        . Shrubbery.branch @(Section TelephoneLocalPrefix)      show
        . Shrubbery.branch @(Section TelephoneLocalSuffix)      show
        . Shrubbery.branch @(Section TelephoneExtension)        show
        . Shrubbery.branch @(Section Organization)              show
        . Shrubbery.branch @(Section OrganizationTitle)         show
        . Shrubbery.branch @(Section StreetAddress)             show
        . Shrubbery.branch @(Section AddressLine1)              show
        . Shrubbery.branch @(Section AddressLine2)              show
        . Shrubbery.branch @(Section AddressLine3)              show
        . Shrubbery.branch @(Section AddressLevel4)             show
        . Shrubbery.branch @(Section AddressLevel3)             show
        . Shrubbery.branch @(Section AddressLevel2)             show
        . Shrubbery.branch @(Section AddressLevel1)             show
        . Shrubbery.branch @(Section Country)                   show
        . Shrubbery.branch @(Section CountryName)               show
        . Shrubbery.branch @(Section PostalCode)                show
        . Shrubbery.branch @(Section CreditCardFullName)        show
        . Shrubbery.branch @(Section CreditCardGivenName)       show
        . Shrubbery.branch @(Section CreditCardAdditionalName)  show
        . Shrubbery.branch @(Section CreditCardFamilyName)      show
        . Shrubbery.branch @(Section CreditCardNumber)          show
        . Shrubbery.branch @(Section CreditCardExpiration)      show
        . Shrubbery.branch @(Section CreditCardExpirationMonth) show
        . Shrubbery.branch @(Section CreditCardExpirationYear)  show
        . Shrubbery.branch @(Section CreditCardSecurityCode)    show
        . Shrubbery.branch @(Section CreditCardType)            show
        . Shrubbery.branch @(Section TransactionCurrency)       show
        . Shrubbery.branch @(Section TransactionAmount)         show
        . Shrubbery.branch @(Section Birthday)                  show
        . Shrubbery.branch @(Section BirthdayDay)               show
        . Shrubbery.branch @(Section BirthdayMonth)             show
        . Shrubbery.branch @(Section BirthdayYear)              show
        . Shrubbery.branch @(Section Language)                  show
        . Shrubbery.branch @(Section Sex)                       show
        . Shrubbery.branch @(Section Url)                       show
        . Shrubbery.branch @(Section Photo)                     show
        . Shrubbery.branch @Home                                show
        . Shrubbery.branch @Work                                show
        . Shrubbery.branch @Shipping                            show
        . Shrubbery.branch @Billing                             show
        . Shrubbery.branch @Mobile                              show
        . Shrubbery.branch @Fax                                 show
        . Shrubbery.branch @Pager                               show
        . Shrubbery.branch @FullName                            show
        . Shrubbery.branch @HonorificPrefix                     show
        . Shrubbery.branch @GivenName                           show
        . Shrubbery.branch @AdditionalName                      show
        . Shrubbery.branch @FamilyName                          show
        . Shrubbery.branch @HonorificSuffix                     show
        . Shrubbery.branch @Nickname                            show
        . Shrubbery.branch @Username                            show
        . Shrubbery.branch @NewPassword                         show
        . Shrubbery.branch @CurrentPassword                     show
        . Shrubbery.branch @OneTimeCode                         show
        . Shrubbery.branch @Email                               show
        . Shrubbery.branch @InstantMessagingProtocol            show
        . Shrubbery.branch @Telephone                           show
        . Shrubbery.branch @TelephoneCountryCode                show
        . Shrubbery.branch @TelephoneNational                   show
        . Shrubbery.branch @TelephoneAreaCode                   show
        . Shrubbery.branch @TelephoneLocal                      show
        . Shrubbery.branch @TelephoneLocalPrefix                show
        . Shrubbery.branch @TelephoneLocalSuffix                show
        . Shrubbery.branch @TelephoneExtension                  show
        . Shrubbery.branch @Organization                        show
        . Shrubbery.branch @OrganizationTitle                   show
        . Shrubbery.branch @StreetAddress                       show
        . Shrubbery.branch @AddressLine1                        show
        . Shrubbery.branch @AddressLine2                        show
        . Shrubbery.branch @AddressLine3                        show
        . Shrubbery.branch @AddressLevel4                       show
        . Shrubbery.branch @AddressLevel3                       show
        . Shrubbery.branch @AddressLevel2                       show
        . Shrubbery.branch @AddressLevel1                       show
        . Shrubbery.branch @Country                             show
        . Shrubbery.branch @CountryName                         show
        . Shrubbery.branch @PostalCode                          show
        . Shrubbery.branch @CreditCardFullName                  show
        . Shrubbery.branch @CreditCardGivenName                 show
        . Shrubbery.branch @CreditCardAdditionalName            show
        . Shrubbery.branch @CreditCardFamilyName                show
        . Shrubbery.branch @CreditCardNumber                    show
        . Shrubbery.branch @CreditCardExpiration                show
        . Shrubbery.branch @CreditCardExpirationMonth           show
        . Shrubbery.branch @CreditCardExpirationYear            show
        . Shrubbery.branch @CreditCardSecurityCode              show
        . Shrubbery.branch @CreditCardType                      show
        . Shrubbery.branch @TransactionCurrency                 show
        . Shrubbery.branch @TransactionAmount                   show
        . Shrubbery.branch @Birthday                            show
        . Shrubbery.branch @BirthdayDay                         show
        . Shrubbery.branch @BirthdayMonth                       show
        . Shrubbery.branch @BirthdayYear                        show
        . Shrubbery.branch @Language                            show
        . Shrubbery.branch @Sex                                 show
        . Shrubbery.branch @Url                                 show
        . Shrubbery.branch @Photo                               show
        $ Shrubbery.branchEnd
    ) token

type AutocompleteTokenTypes =
  [ Off
  , On
  , Section Home
  , Section Work
  , Section Shipping
  , Section Billing
  , Section Mobile
  , Section Fax
  , Section Pager
  , Section FullName
  , Section HonorificPrefix
  , Section GivenName
  , Section AdditionalName
  , Section FamilyName
  , Section HonorificSuffix
  , Section Nickname
  , Section Username
  , Section NewPassword
  , Section CurrentPassword
  , Section OneTimeCode
  , Section Email
  , Section InstantMessagingProtocol
  , Section Telephone
  , Section TelephoneCountryCode
  , Section TelephoneNational
  , Section TelephoneAreaCode
  , Section TelephoneLocal
  , Section TelephoneLocalPrefix
  , Section TelephoneLocalSuffix
  , Section TelephoneExtension
  , Section Organization
  , Section OrganizationTitle
  , Section StreetAddress
  , Section AddressLine1
  , Section AddressLine2
  , Section AddressLine3
  , Section AddressLevel4
  , Section AddressLevel3
  , Section AddressLevel2
  , Section AddressLevel1
  , Section Country
  , Section CountryName
  , Section PostalCode
  , Section CreditCardFullName
  , Section CreditCardGivenName
  , Section CreditCardAdditionalName
  , Section CreditCardFamilyName
  , Section CreditCardNumber
  , Section CreditCardExpiration
  , Section CreditCardExpirationMonth
  , Section CreditCardExpirationYear
  , Section CreditCardSecurityCode
  , Section CreditCardType
  , Section TransactionCurrency
  , Section TransactionAmount
  , Section Birthday
  , Section BirthdayDay
  , Section BirthdayMonth
  , Section BirthdayYear
  , Section Language
  , Section Sex
  , Section Url
  , Section Photo
  , Home
  , Work
  , Shipping
  , Billing
  , Mobile
  , Fax
  , Pager
  , FullName
  , HonorificPrefix
  , GivenName
  , AdditionalName
  , FamilyName
  , HonorificSuffix
  , Nickname
  , Username
  , NewPassword
  , CurrentPassword
  , OneTimeCode
  , Email
  , InstantMessagingProtocol
  , Telephone
  , TelephoneCountryCode
  , TelephoneNational
  , TelephoneAreaCode
  , TelephoneLocal
  , TelephoneLocalPrefix
  , TelephoneLocalSuffix
  , TelephoneExtension
  , Organization
  , OrganizationTitle
  , StreetAddress
  , AddressLine1
  , AddressLine2
  , AddressLine3
  , AddressLevel4
  , AddressLevel3
  , AddressLevel2
  , AddressLevel1
  , Country
  , CountryName
  , PostalCode
  , CreditCardFullName
  , CreditCardGivenName
  , CreditCardAdditionalName
  , CreditCardFamilyName
  , CreditCardNumber
  , CreditCardExpiration
  , CreditCardExpirationMonth
  , CreditCardExpirationYear
  , CreditCardSecurityCode
  , CreditCardType
  , TransactionCurrency
  , TransactionAmount
  , Birthday
  , BirthdayDay
  , BirthdayMonth
  , BirthdayYear
  , Language
  , Sex
  , Url
  , Photo
  ]

mkAutocompleteToken :: ( KnownNat branchIndex
                       , branchIndex ~ FirstIndexOf token AutocompleteTokenTypes
                       )
                    => token -> AutocompleteToken
mkAutocompleteToken =
  AutocompleteToken . Shrubbery.unify

autocompleteTokenToBytes :: AutocompleteToken -> LBS.ByteString
autocompleteTokenToBytes (AutocompleteToken token) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Off                                 offToBytes
      . Shrubbery.branch @On                                  onToBytes
      . Shrubbery.branch @(Section Home)                      (sectionToBytes homeToBytes)
      . Shrubbery.branch @(Section Work)                      (sectionToBytes workToBytes)
      . Shrubbery.branch @(Section Shipping)                  (sectionToBytes shippingToBytes)
      . Shrubbery.branch @(Section Billing)                   (sectionToBytes billingToBytes)
      . Shrubbery.branch @(Section Mobile)                    (sectionToBytes mobileToBytes)
      . Shrubbery.branch @(Section Fax)                       (sectionToBytes faxToBytes)
      . Shrubbery.branch @(Section Pager)                     (sectionToBytes pagerToBytes)
      . Shrubbery.branch @(Section FullName)                  (sectionToBytes fullNameToBytes)
      . Shrubbery.branch @(Section HonorificPrefix)           (sectionToBytes honorificPrefixToBytes)
      . Shrubbery.branch @(Section GivenName)                 (sectionToBytes givenNameToBytes)
      . Shrubbery.branch @(Section AdditionalName)            (sectionToBytes additionalNameToBytes)
      . Shrubbery.branch @(Section FamilyName)                (sectionToBytes familyNameToBytes)
      . Shrubbery.branch @(Section HonorificSuffix)           (sectionToBytes honorificSuffixToBytes)
      . Shrubbery.branch @(Section Nickname)                  (sectionToBytes nicknameToBytes)
      . Shrubbery.branch @(Section Username)                  (sectionToBytes usernameToBytes)
      . Shrubbery.branch @(Section NewPassword)               (sectionToBytes newPasswordToBytes)
      . Shrubbery.branch @(Section CurrentPassword)           (sectionToBytes currentPasswordToBytes)
      . Shrubbery.branch @(Section OneTimeCode)               (sectionToBytes oneTimeCodeToBytes)
      . Shrubbery.branch @(Section Email)                     (sectionToBytes emailToBytes)
      . Shrubbery.branch @(Section InstantMessagingProtocol)  (sectionToBytes instantMessagingProtocolToBytes)
      . Shrubbery.branch @(Section Telephone)                 (sectionToBytes telephoneToBytes)
      . Shrubbery.branch @(Section TelephoneCountryCode)      (sectionToBytes telephoneCountryCodeToBytes)
      . Shrubbery.branch @(Section TelephoneNational)         (sectionToBytes telephoneNationalToBytes)
      . Shrubbery.branch @(Section TelephoneAreaCode)         (sectionToBytes telephoneAreaCodeToBytes)
      . Shrubbery.branch @(Section TelephoneLocal)            (sectionToBytes telephoneLocalToBytes)
      . Shrubbery.branch @(Section TelephoneLocalPrefix)      (sectionToBytes telephoneLocalPrefixToBytes)
      . Shrubbery.branch @(Section TelephoneLocalSuffix)      (sectionToBytes telephoneLocalSuffixToBytes)
      . Shrubbery.branch @(Section TelephoneExtension)        (sectionToBytes telephoneExtensionToBytes)
      . Shrubbery.branch @(Section Organization)              (sectionToBytes organizationToBytes)
      . Shrubbery.branch @(Section OrganizationTitle)         (sectionToBytes organizationTitleToBytes)
      . Shrubbery.branch @(Section StreetAddress)             (sectionToBytes streetAddressToBytes)
      . Shrubbery.branch @(Section AddressLine1)              (sectionToBytes addressLine1ToBytes)
      . Shrubbery.branch @(Section AddressLine2)              (sectionToBytes addressLine2ToBytes)
      . Shrubbery.branch @(Section AddressLine3)              (sectionToBytes addressLine3ToBytes)
      . Shrubbery.branch @(Section AddressLevel4)             (sectionToBytes addressLevel4ToBytes)
      . Shrubbery.branch @(Section AddressLevel3)             (sectionToBytes addressLevel3ToBytes)
      . Shrubbery.branch @(Section AddressLevel2)             (sectionToBytes addressLevel2ToBytes)
      . Shrubbery.branch @(Section AddressLevel1)             (sectionToBytes addressLevel1ToBytes)
      . Shrubbery.branch @(Section Country)                   (sectionToBytes countryToBytes)
      . Shrubbery.branch @(Section CountryName)               (sectionToBytes countryNameToBytes)
      . Shrubbery.branch @(Section PostalCode)                (sectionToBytes postalCodeToBytes)
      . Shrubbery.branch @(Section CreditCardFullName)        (sectionToBytes creditCardFullNameToBytes)
      . Shrubbery.branch @(Section CreditCardGivenName)       (sectionToBytes creditCardGivenNameToBytes)
      . Shrubbery.branch @(Section CreditCardAdditionalName)  (sectionToBytes creditCardAdditionalNameToBytes)
      . Shrubbery.branch @(Section CreditCardFamilyName)      (sectionToBytes creditCardFamilyNameToBytes)
      . Shrubbery.branch @(Section CreditCardNumber)          (sectionToBytes creditCardNumberToBytes)
      . Shrubbery.branch @(Section CreditCardExpiration)      (sectionToBytes creditCardExpirationToBytes)
      . Shrubbery.branch @(Section CreditCardExpirationMonth) (sectionToBytes creditCardExpirationMonthToBytes)
      . Shrubbery.branch @(Section CreditCardExpirationYear)  (sectionToBytes creditCardExpirationYearToBytes)
      . Shrubbery.branch @(Section CreditCardSecurityCode)    (sectionToBytes creditCardSecurityCodeToBytes)
      . Shrubbery.branch @(Section CreditCardType)            (sectionToBytes creditCardTypeToBytes)
      . Shrubbery.branch @(Section TransactionCurrency)       (sectionToBytes transactionCurrencyToBytes)
      . Shrubbery.branch @(Section TransactionAmount)         (sectionToBytes transactionAmountToBytes)
      . Shrubbery.branch @(Section Birthday)                  (sectionToBytes birthdayToBytes)
      . Shrubbery.branch @(Section BirthdayDay)               (sectionToBytes birthdayDayToBytes)
      . Shrubbery.branch @(Section BirthdayMonth)             (sectionToBytes birthdayMonthToBytes)
      . Shrubbery.branch @(Section BirthdayYear)              (sectionToBytes birthdayYearToBytes)
      . Shrubbery.branch @(Section Language)                  (sectionToBytes languageToBytes)
      . Shrubbery.branch @(Section Sex)                       (sectionToBytes sexToBytes)
      . Shrubbery.branch @(Section Url)                       (sectionToBytes urlToBytes)
      . Shrubbery.branch @(Section Photo)                     (sectionToBytes photoToBytes)
      . Shrubbery.branch @Home                                homeToBytes
      . Shrubbery.branch @Work                                workToBytes
      . Shrubbery.branch @Shipping                            shippingToBytes
      . Shrubbery.branch @Billing                             billingToBytes
      . Shrubbery.branch @Mobile                              mobileToBytes
      . Shrubbery.branch @Fax                                 faxToBytes
      . Shrubbery.branch @Pager                               pagerToBytes
      . Shrubbery.branch @FullName                            fullNameToBytes
      . Shrubbery.branch @HonorificPrefix                     honorificPrefixToBytes
      . Shrubbery.branch @GivenName                           givenNameToBytes
      . Shrubbery.branch @AdditionalName                      additionalNameToBytes
      . Shrubbery.branch @FamilyName                          familyNameToBytes
      . Shrubbery.branch @HonorificSuffix                     honorificSuffixToBytes
      . Shrubbery.branch @Nickname                            nicknameToBytes
      . Shrubbery.branch @Username                            usernameToBytes
      . Shrubbery.branch @NewPassword                         newPasswordToBytes
      . Shrubbery.branch @CurrentPassword                     currentPasswordToBytes
      . Shrubbery.branch @OneTimeCode                         oneTimeCodeToBytes
      . Shrubbery.branch @Email                               emailToBytes
      . Shrubbery.branch @InstantMessagingProtocol            instantMessagingProtocolToBytes
      . Shrubbery.branch @Telephone                           telephoneToBytes
      . Shrubbery.branch @TelephoneCountryCode                telephoneCountryCodeToBytes
      . Shrubbery.branch @TelephoneNational                   telephoneNationalToBytes
      . Shrubbery.branch @TelephoneAreaCode                   telephoneAreaCodeToBytes
      . Shrubbery.branch @TelephoneLocal                      telephoneLocalToBytes
      . Shrubbery.branch @TelephoneLocalPrefix                telephoneLocalPrefixToBytes
      . Shrubbery.branch @TelephoneLocalSuffix                telephoneLocalSuffixToBytes
      . Shrubbery.branch @TelephoneExtension                  telephoneExtensionToBytes
      . Shrubbery.branch @Organization                        organizationToBytes
      . Shrubbery.branch @OrganizationTitle                   organizationTitleToBytes
      . Shrubbery.branch @StreetAddress                       streetAddressToBytes
      . Shrubbery.branch @AddressLine1                        addressLine1ToBytes
      . Shrubbery.branch @AddressLine2                        addressLine2ToBytes
      . Shrubbery.branch @AddressLine3                        addressLine3ToBytes
      . Shrubbery.branch @AddressLevel4                       addressLevel4ToBytes
      . Shrubbery.branch @AddressLevel3                       addressLevel3ToBytes
      . Shrubbery.branch @AddressLevel2                       addressLevel2ToBytes
      . Shrubbery.branch @AddressLevel1                       addressLevel1ToBytes
      . Shrubbery.branch @Country                             countryToBytes
      . Shrubbery.branch @CountryName                         countryNameToBytes
      . Shrubbery.branch @PostalCode                          postalCodeToBytes
      . Shrubbery.branch @CreditCardFullName                  creditCardFullNameToBytes
      . Shrubbery.branch @CreditCardGivenName                 creditCardGivenNameToBytes
      . Shrubbery.branch @CreditCardAdditionalName            creditCardAdditionalNameToBytes
      . Shrubbery.branch @CreditCardFamilyName                creditCardFamilyNameToBytes
      . Shrubbery.branch @CreditCardNumber                    creditCardNumberToBytes
      . Shrubbery.branch @CreditCardExpiration                creditCardExpirationToBytes
      . Shrubbery.branch @CreditCardExpirationMonth           creditCardExpirationMonthToBytes
      . Shrubbery.branch @CreditCardExpirationYear            creditCardExpirationYearToBytes
      . Shrubbery.branch @CreditCardSecurityCode              creditCardSecurityCodeToBytes
      . Shrubbery.branch @CreditCardType                      creditCardTypeToBytes
      . Shrubbery.branch @TransactionCurrency                 transactionCurrencyToBytes
      . Shrubbery.branch @TransactionAmount                   transactionAmountToBytes
      . Shrubbery.branch @Birthday                            birthdayToBytes
      . Shrubbery.branch @BirthdayDay                         birthdayDayToBytes
      . Shrubbery.branch @BirthdayMonth                       birthdayMonthToBytes
      . Shrubbery.branch @BirthdayYear                        birthdayYearToBytes
      . Shrubbery.branch @Language                            languageToBytes
      . Shrubbery.branch @Sex                                 sexToBytes
      . Shrubbery.branch @Url                                 urlToBytes
      . Shrubbery.branch @Photo                               photoToBytes
      $ Shrubbery.branchEnd
  ) token

autocompleteTokenToText :: AutocompleteToken -> T.Text
autocompleteTokenToText (AutocompleteToken token) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Off                                 offToText
      . Shrubbery.branch @On                                  onToText
      . Shrubbery.branch @(Section Home)                      (sectionToText homeToText)
      . Shrubbery.branch @(Section Work)                      (sectionToText workToText)
      . Shrubbery.branch @(Section Shipping)                  (sectionToText shippingToText)
      . Shrubbery.branch @(Section Billing)                   (sectionToText billingToText)
      . Shrubbery.branch @(Section Mobile)                    (sectionToText mobileToText)
      . Shrubbery.branch @(Section Fax)                       (sectionToText faxToText)
      . Shrubbery.branch @(Section Pager)                     (sectionToText pagerToText)
      . Shrubbery.branch @(Section FullName)                  (sectionToText fullNameToText)
      . Shrubbery.branch @(Section HonorificPrefix)           (sectionToText honorificPrefixToText)
      . Shrubbery.branch @(Section GivenName)                 (sectionToText givenNameToText)
      . Shrubbery.branch @(Section AdditionalName)            (sectionToText additionalNameToText)
      . Shrubbery.branch @(Section FamilyName)                (sectionToText familyNameToText)
      . Shrubbery.branch @(Section HonorificSuffix)           (sectionToText honorificSuffixToText)
      . Shrubbery.branch @(Section Nickname)                  (sectionToText nicknameToText)
      . Shrubbery.branch @(Section Username)                  (sectionToText usernameToText)
      . Shrubbery.branch @(Section NewPassword)               (sectionToText newPasswordToText)
      . Shrubbery.branch @(Section CurrentPassword)           (sectionToText currentPasswordToText)
      . Shrubbery.branch @(Section OneTimeCode)               (sectionToText oneTimeCodeToText)
      . Shrubbery.branch @(Section Email)                     (sectionToText emailToText)
      . Shrubbery.branch @(Section InstantMessagingProtocol)  (sectionToText instantMessagingProtocolToText)
      . Shrubbery.branch @(Section Telephone)                 (sectionToText telephoneToText)
      . Shrubbery.branch @(Section TelephoneCountryCode)      (sectionToText telephoneCountryCodeToText)
      . Shrubbery.branch @(Section TelephoneNational)         (sectionToText telephoneNationalToText)
      . Shrubbery.branch @(Section TelephoneAreaCode)         (sectionToText telephoneAreaCodeToText)
      . Shrubbery.branch @(Section TelephoneLocal)            (sectionToText telephoneLocalToText)
      . Shrubbery.branch @(Section TelephoneLocalPrefix)      (sectionToText telephoneLocalPrefixToText)
      . Shrubbery.branch @(Section TelephoneLocalSuffix)      (sectionToText telephoneLocalSuffixToText)
      . Shrubbery.branch @(Section TelephoneExtension)        (sectionToText telephoneExtensionToText)
      . Shrubbery.branch @(Section Organization)              (sectionToText organizationToText)
      . Shrubbery.branch @(Section OrganizationTitle)         (sectionToText organizationTitleToText)
      . Shrubbery.branch @(Section StreetAddress)             (sectionToText streetAddressToText)
      . Shrubbery.branch @(Section AddressLine1)              (sectionToText addressLine1ToText)
      . Shrubbery.branch @(Section AddressLine2)              (sectionToText addressLine2ToText)
      . Shrubbery.branch @(Section AddressLine3)              (sectionToText addressLine3ToText)
      . Shrubbery.branch @(Section AddressLevel4)             (sectionToText addressLevel4ToText)
      . Shrubbery.branch @(Section AddressLevel3)             (sectionToText addressLevel3ToText)
      . Shrubbery.branch @(Section AddressLevel2)             (sectionToText addressLevel2ToText)
      . Shrubbery.branch @(Section AddressLevel1)             (sectionToText addressLevel1ToText)
      . Shrubbery.branch @(Section Country)                   (sectionToText countryToText)
      . Shrubbery.branch @(Section CountryName)               (sectionToText countryNameToText)
      . Shrubbery.branch @(Section PostalCode)                (sectionToText postalCodeToText)
      . Shrubbery.branch @(Section CreditCardFullName)        (sectionToText creditCardFullNameToText)
      . Shrubbery.branch @(Section CreditCardGivenName)       (sectionToText creditCardGivenNameToText)
      . Shrubbery.branch @(Section CreditCardAdditionalName)  (sectionToText creditCardAdditionalNameToText)
      . Shrubbery.branch @(Section CreditCardFamilyName)      (sectionToText creditCardFamilyNameToText)
      . Shrubbery.branch @(Section CreditCardNumber)          (sectionToText creditCardNumberToText)
      . Shrubbery.branch @(Section CreditCardExpiration)      (sectionToText creditCardExpirationToText)
      . Shrubbery.branch @(Section CreditCardExpirationMonth) (sectionToText creditCardExpirationMonthToText)
      . Shrubbery.branch @(Section CreditCardExpirationYear)  (sectionToText creditCardExpirationYearToText)
      . Shrubbery.branch @(Section CreditCardSecurityCode)    (sectionToText creditCardSecurityCodeToText)
      . Shrubbery.branch @(Section CreditCardType)            (sectionToText creditCardTypeToText)
      . Shrubbery.branch @(Section TransactionCurrency)       (sectionToText transactionCurrencyToText)
      . Shrubbery.branch @(Section TransactionAmount)         (sectionToText transactionAmountToText)
      . Shrubbery.branch @(Section Birthday)                  (sectionToText birthdayToText)
      . Shrubbery.branch @(Section BirthdayDay)               (sectionToText birthdayDayToText)
      . Shrubbery.branch @(Section BirthdayMonth)             (sectionToText birthdayMonthToText)
      . Shrubbery.branch @(Section BirthdayYear)              (sectionToText birthdayYearToText)
      . Shrubbery.branch @(Section Language)                  (sectionToText languageToText)
      . Shrubbery.branch @(Section Sex)                       (sectionToText sexToText)
      . Shrubbery.branch @(Section Url)                       (sectionToText urlToText)
      . Shrubbery.branch @(Section Photo)                     (sectionToText photoToText)
      . Shrubbery.branch @Home                                homeToText
      . Shrubbery.branch @Work                                workToText
      . Shrubbery.branch @Shipping                            shippingToText
      . Shrubbery.branch @Billing                             billingToText
      . Shrubbery.branch @Mobile                              mobileToText
      . Shrubbery.branch @Fax                                 faxToText
      . Shrubbery.branch @Pager                               pagerToText
      . Shrubbery.branch @FullName                            fullNameToText
      . Shrubbery.branch @HonorificPrefix                     honorificPrefixToText
      . Shrubbery.branch @GivenName                           givenNameToText
      . Shrubbery.branch @AdditionalName                      additionalNameToText
      . Shrubbery.branch @FamilyName                          familyNameToText
      . Shrubbery.branch @HonorificSuffix                     honorificSuffixToText
      . Shrubbery.branch @Nickname                            nicknameToText
      . Shrubbery.branch @Username                            usernameToText
      . Shrubbery.branch @NewPassword                         newPasswordToText
      . Shrubbery.branch @CurrentPassword                     currentPasswordToText
      . Shrubbery.branch @OneTimeCode                         oneTimeCodeToText
      . Shrubbery.branch @Email                               emailToText
      . Shrubbery.branch @InstantMessagingProtocol            instantMessagingProtocolToText
      . Shrubbery.branch @Telephone                           telephoneToText
      . Shrubbery.branch @TelephoneCountryCode                telephoneCountryCodeToText
      . Shrubbery.branch @TelephoneNational                   telephoneNationalToText
      . Shrubbery.branch @TelephoneAreaCode                   telephoneAreaCodeToText
      . Shrubbery.branch @TelephoneLocal                      telephoneLocalToText
      . Shrubbery.branch @TelephoneLocalPrefix                telephoneLocalPrefixToText
      . Shrubbery.branch @TelephoneLocalSuffix                telephoneLocalSuffixToText
      . Shrubbery.branch @TelephoneExtension                  telephoneExtensionToText
      . Shrubbery.branch @Organization                        organizationToText
      . Shrubbery.branch @OrganizationTitle                   organizationTitleToText
      . Shrubbery.branch @StreetAddress                       streetAddressToText
      . Shrubbery.branch @AddressLine1                        addressLine1ToText
      . Shrubbery.branch @AddressLine2                        addressLine2ToText
      . Shrubbery.branch @AddressLine3                        addressLine3ToText
      . Shrubbery.branch @AddressLevel4                       addressLevel4ToText
      . Shrubbery.branch @AddressLevel3                       addressLevel3ToText
      . Shrubbery.branch @AddressLevel2                       addressLevel2ToText
      . Shrubbery.branch @AddressLevel1                       addressLevel1ToText
      . Shrubbery.branch @Country                             countryToText
      . Shrubbery.branch @CountryName                         countryNameToText
      . Shrubbery.branch @PostalCode                          postalCodeToText
      . Shrubbery.branch @CreditCardFullName                  creditCardFullNameToText
      . Shrubbery.branch @CreditCardGivenName                 creditCardGivenNameToText
      . Shrubbery.branch @CreditCardAdditionalName            creditCardAdditionalNameToText
      . Shrubbery.branch @CreditCardFamilyName                creditCardFamilyNameToText
      . Shrubbery.branch @CreditCardNumber                    creditCardNumberToText
      . Shrubbery.branch @CreditCardExpiration                creditCardExpirationToText
      . Shrubbery.branch @CreditCardExpirationMonth           creditCardExpirationMonthToText
      . Shrubbery.branch @CreditCardExpirationYear            creditCardExpirationYearToText
      . Shrubbery.branch @CreditCardSecurityCode              creditCardSecurityCodeToText
      . Shrubbery.branch @CreditCardType                      creditCardTypeToText
      . Shrubbery.branch @TransactionCurrency                 transactionCurrencyToText
      . Shrubbery.branch @TransactionAmount                   transactionAmountToText
      . Shrubbery.branch @Birthday                            birthdayToText
      . Shrubbery.branch @BirthdayDay                         birthdayDayToText
      . Shrubbery.branch @BirthdayMonth                       birthdayMonthToText
      . Shrubbery.branch @BirthdayYear                        birthdayYearToText
      . Shrubbery.branch @Language                            languageToText
      . Shrubbery.branch @Sex                                 sexToText
      . Shrubbery.branch @Url                                 urlToText
      . Shrubbery.branch @Photo                               photoToText
      $ Shrubbery.branchEnd
  ) token

-- TODO: This mechanism needs to exist for this.
--
type family Elem (a :: Type) :: () where

data Section token =
  Section
    { sectionName  :: String
    , sectionValue :: token
    }

instance Show token => Show (Section token) where
  show = show

section :: ValidSection token => String -> token -> Section token
section = Section

sectionToBytes :: (token -> LBS.ByteString) -> Section token -> LBS.ByteString
sectionToBytes toBytes s =
  LBS8.unwords
    [ LBS8.pack $ sectionName s
    , toBytes $ sectionValue s
    ]

sectionToText :: (token -> T.Text) -> Section token -> T.Text
sectionToText toText s =
  T.unwords
    [ T.pack $ sectionName s
    , toText $ sectionValue s
    ]

-- On / Off
--
data Off = Off
  deriving Show

offToBytes :: Off -> LBS.ByteString
offToBytes Off = "off"

offToText :: Off -> T.Text
offToText Off = "off"

data On = On
  deriving Show

onToBytes :: On -> LBS.ByteString
onToBytes On = "on"

onToText :: On -> T.Text
onToText On = "on"

-- Modifiers
--
newtype Home token = Home token

home :: ValidHome token => token -> Home
home = Home

type HomeTypes =
  [ FullName
  , HonorificPrefix
  , GivenName
  , AdditionalName
  , FamilyName
  , HonorificSuffix
  , Email
  , Telephone
  , StreetAddress
  , AddressLine1
  , AddressLine2
  , AddressLine3
  , AddressLevel4
  , AddressLevel3
  , AddressLevel2
  , AddressLevel1
  , Country
  , CountryName
  , PostalCode
  ]

instance Show Home where
  show (Home a) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @FullName        show
        . Shrubbery.branch @HonorificPrefix show
        . Shrubbery.branch @GivenName       show
        . Shrubbery.branch @AdditionalName  show
        . Shrubbery.branch @FamilyName      show
        . Shrubbery.branch @HonorificSuffix show
        . Shrubbery.branch @Email           show
        . Shrubbery.branch @Telephone       show
        . Shrubbery.branch @StreetAddress   show
        . Shrubbery.branch @AddressLine1    show
        . Shrubbery.branch @AddressLine2    show
        . Shrubbery.branch @AddressLine3    show
        . Shrubbery.branch @AddressLevel4   show
        . Shrubbery.branch @AddressLevel3   show
        . Shrubbery.branch @AddressLevel2   show
        . Shrubbery.branch @AddressLevel1   show
        . Shrubbery.branch @Country         show
        . Shrubbery.branch @CountryName     show
        . Shrubbery.branch @PostalCode      show
        $ Shrubbery.branchEnd
    ) a

home :: (KnownNat branchIndex, branchIndex ~ FirstIndexOf home HomeTypes)
     => home -> Home
home =
  Home . Shrubbery.unify

homeToBytes :: Home -> LBS.ByteString
homeToBytes (Home a) =
  LBS8.unwords
    [ "home"
    , ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @FullName        fullNameToBytes
          . Shrubbery.branch @HonorificPrefix honorificPrefixToBytes
          . Shrubbery.branch @GivenName       givenNameToBytes
          . Shrubbery.branch @AdditionalName  additionalNameToBytes
          . Shrubbery.branch @FamilyName      familyNameToBytes
          . Shrubbery.branch @HonorificSuffix honorificSuffixToBytes
          . Shrubbery.branch @Email           emailToBytes
          . Shrubbery.branch @Telephone       telephoneToBytes
          . Shrubbery.branch @StreetAddress   streetAddressToBytes
          . Shrubbery.branch @AddressLine1    addressLine1ToBytes
          . Shrubbery.branch @AddressLine2    addressLine2ToBytes
          . Shrubbery.branch @AddressLine3    addressLine3ToBytes
          . Shrubbery.branch @AddressLevel4   addressLevel4ToBytes
          . Shrubbery.branch @AddressLevel3   addressLevel3ToBytes
          . Shrubbery.branch @AddressLevel2   addressLevel2ToBytes
          . Shrubbery.branch @AddressLevel1   addressLevel1ToBytes
          . Shrubbery.branch @Country         countryToBytes
          . Shrubbery.branch @CountryName     countryNameToBytes
          . Shrubbery.branch @PostalCode      postalCodeToBytes
          $ Shrubbery.branchEnd
      ) a
    ]

homeToText :: Home -> T.Text
homeToText (Home a) =
  T.unwords
    [ "home"
    , ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @FullName        fullNameToText
          . Shrubbery.branch @HonorificPrefix honorificPrefixToText
          . Shrubbery.branch @GivenName       givenNameToText
          . Shrubbery.branch @AdditionalName  additionalNameToText
          . Shrubbery.branch @FamilyName      familyNameToText
          . Shrubbery.branch @HonorificSuffix honorificSuffixToText
          . Shrubbery.branch @Email           emailToText
          . Shrubbery.branch @Telephone       telephoneToText
          . Shrubbery.branch @StreetAddress   streetAddressToText
          . Shrubbery.branch @AddressLine1    addressLine1ToText
          . Shrubbery.branch @AddressLine2    addressLine2ToText
          . Shrubbery.branch @AddressLine3    addressLine3ToText
          . Shrubbery.branch @AddressLevel4   addressLevel4ToText
          . Shrubbery.branch @AddressLevel3   addressLevel3ToText
          . Shrubbery.branch @AddressLevel2   addressLevel2ToText
          . Shrubbery.branch @AddressLevel1   addressLevel1ToText
          . Shrubbery.branch @Country         countryToText
          . Shrubbery.branch @CountryName     countryNameToText
          . Shrubbery.branch @PostalCode      postalCodeToText
          $ Shrubbery.branchEnd
      ) a
    ]

newtype Work = Work (Shrubbery.Union WorkTypes)

type WorkTypes =
  [ FullName
  , HonorificPrefix
  , GivenName
  , AdditionalName
  , FamilyName
  , HonorificSuffix
  , Email
  , Telephone
  , StreetAddress
  , AddressLine1
  , AddressLine2
  , AddressLine3
  , AddressLevel4
  , AddressLevel3
  , AddressLevel2
  , AddressLevel1
  , Country
  , CountryName
  , PostalCode
  ]

instance Show Work where
  show (Work a) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @FullName        show
        . Shrubbery.branch @HonorificPrefix show
        . Shrubbery.branch @GivenName       show
        . Shrubbery.branch @AdditionalName  show
        . Shrubbery.branch @FamilyName      show
        . Shrubbery.branch @HonorificSuffix show
        . Shrubbery.branch @Email           show
        . Shrubbery.branch @Telephone       show
        . Shrubbery.branch @StreetAddress   show
        . Shrubbery.branch @AddressLine1    show
        . Shrubbery.branch @AddressLine2    show
        . Shrubbery.branch @AddressLine3    show
        . Shrubbery.branch @AddressLevel4   show
        . Shrubbery.branch @AddressLevel3   show
        . Shrubbery.branch @AddressLevel2   show
        . Shrubbery.branch @AddressLevel1   show
        . Shrubbery.branch @Country         show
        . Shrubbery.branch @CountryName     show
        . Shrubbery.branch @PostalCode      show
        $ Shrubbery.branchEnd
    ) a

work :: (KnownNat branchIndex, branchIndex ~ FirstIndexOf work WorkTypes)
     => work -> Work
work =
  Work . Shrubbery.unify

workToBytes :: Work -> LBS.ByteString
workToBytes (Work a) =
  LBS8.unwords
    [ "work"
    , ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @FullName        fullNameToBytes
          . Shrubbery.branch @HonorificPrefix honorificPrefixToBytes
          . Shrubbery.branch @GivenName       givenNameToBytes
          . Shrubbery.branch @AdditionalName  additionalNameToBytes
          . Shrubbery.branch @FamilyName      familyNameToBytes
          . Shrubbery.branch @HonorificSuffix honorificSuffixToBytes
          . Shrubbery.branch @Email           emailToBytes
          . Shrubbery.branch @Telephone       telephoneToBytes
          . Shrubbery.branch @StreetAddress   streetAddressToBytes
          . Shrubbery.branch @AddressLine1    addressLine1ToBytes
          . Shrubbery.branch @AddressLine2    addressLine2ToBytes
          . Shrubbery.branch @AddressLine3    addressLine3ToBytes
          . Shrubbery.branch @AddressLevel4   addressLevel4ToBytes
          . Shrubbery.branch @AddressLevel3   addressLevel3ToBytes
          . Shrubbery.branch @AddressLevel2   addressLevel2ToBytes
          . Shrubbery.branch @AddressLevel1   addressLevel1ToBytes
          . Shrubbery.branch @Country         countryToBytes
          . Shrubbery.branch @CountryName     countryNameToBytes
          . Shrubbery.branch @PostalCode      postalCodeToBytes
          $ Shrubbery.branchEnd
      ) a
    ]

workToText :: Work -> T.Text
workToText (Work a) =
  T.unwords
    [ "work"
    , ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @FullName        fullNameToText
          . Shrubbery.branch @HonorificPrefix honorificPrefixToText
          . Shrubbery.branch @GivenName       givenNameToText
          . Shrubbery.branch @AdditionalName  additionalNameToText
          . Shrubbery.branch @FamilyName      familyNameToText
          . Shrubbery.branch @HonorificSuffix honorificSuffixToText
          . Shrubbery.branch @Email           emailToText
          . Shrubbery.branch @Telephone       telephoneToText
          . Shrubbery.branch @StreetAddress   streetAddressToText
          . Shrubbery.branch @AddressLine1    addressLine1ToText
          . Shrubbery.branch @AddressLine2    addressLine2ToText
          . Shrubbery.branch @AddressLine3    addressLine3ToText
          . Shrubbery.branch @AddressLevel4   addressLevel4ToText
          . Shrubbery.branch @AddressLevel3   addressLevel3ToText
          . Shrubbery.branch @AddressLevel2   addressLevel2ToText
          . Shrubbery.branch @AddressLevel1   addressLevel1ToText
          . Shrubbery.branch @Country         countryToText
          . Shrubbery.branch @CountryName     countryNameToText
          . Shrubbery.branch @PostalCode      postalCodeToText
          $ Shrubbery.branchEnd
      ) a
    ]

newtype Shipping = Shipping (Shrubbery.Union ShippingTypes)

type ShippingTypes =
  [ FullName
  , StreetAddress
  , AddressLine1
  , AddressLine2
  , AddressLine3
  , AddressLevel4
  , AddressLevel3
  , AddressLevel2
  , AddressLevel1
  , Country
  , CountryName
  , PostalCode
  ]

instance Show Shipping where
  show (Shipping a) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @FullName      show
        . Shrubbery.branch @StreetAddress show
        . Shrubbery.branch @AddressLine1  show
        . Shrubbery.branch @AddressLine2  show
        . Shrubbery.branch @AddressLine3  show
        . Shrubbery.branch @AddressLevel4 show
        . Shrubbery.branch @AddressLevel3 show
        . Shrubbery.branch @AddressLevel2 show
        . Shrubbery.branch @AddressLevel1 show
        . Shrubbery.branch @Country       show
        . Shrubbery.branch @CountryName   show
        . Shrubbery.branch @PostalCode    show
        $ Shrubbery.branchEnd
    ) a

shipping :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf shipping ShippingTypes
            )
         => shipping -> Shipping
shipping =
  Shipping . Shrubbery.unify

shippingToBytes :: Shipping -> LBS.ByteString
shippingToBytes (Shipping a) =
  LBS8.unwords
    [ "shipping"
    , ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @FullName      fullNameToBytes
          . Shrubbery.branch @StreetAddress streetAddressToBytes
          . Shrubbery.branch @AddressLine1  addressLine1ToBytes
          . Shrubbery.branch @AddressLine2  addressLine2ToBytes
          . Shrubbery.branch @AddressLine3  addressLine3ToBytes
          . Shrubbery.branch @AddressLevel4 addressLevel4ToBytes
          . Shrubbery.branch @AddressLevel3 addressLevel3ToBytes
          . Shrubbery.branch @AddressLevel2 addressLevel2ToBytes
          . Shrubbery.branch @AddressLevel1 addressLevel1ToBytes
          . Shrubbery.branch @Country       countryToBytes
          . Shrubbery.branch @CountryName   countryNameToBytes
          . Shrubbery.branch @PostalCode    postalCodeToBytes
          $ Shrubbery.branchEnd
      ) a
    ]

shippingToText :: Shipping -> T.Text
shippingToText (Shipping a) =
  T.unwords
    [ "shipping"
    , ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @FullName      fullNameToText
          . Shrubbery.branch @StreetAddress streetAddressToText
          . Shrubbery.branch @AddressLine1  addressLine1ToText
          . Shrubbery.branch @AddressLine2  addressLine2ToText
          . Shrubbery.branch @AddressLine3  addressLine3ToText
          . Shrubbery.branch @AddressLevel4 addressLevel4ToText
          . Shrubbery.branch @AddressLevel3 addressLevel3ToText
          . Shrubbery.branch @AddressLevel2 addressLevel2ToText
          . Shrubbery.branch @AddressLevel1 addressLevel1ToText
          . Shrubbery.branch @Country       countryToText
          . Shrubbery.branch @CountryName   countryNameToText
          . Shrubbery.branch @PostalCode    postalCodeToText
          $ Shrubbery.branchEnd
      ) a
    ]

newtype Billing = Billing (Shrubbery.Union BillingTypes)

type BillingTypes =
  [ FullName
  , StreetAddress
  , AddressLine1
  , AddressLine2
  , AddressLine3
  , AddressLevel4
  , AddressLevel3
  , AddressLevel2
  , AddressLevel1
  , Country
  , CountryName
  , PostalCode
  ]

instance Show Billing where
  show (Billing a) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @FullName      show
        . Shrubbery.branch @StreetAddress show
        . Shrubbery.branch @AddressLine1  show
        . Shrubbery.branch @AddressLine2  show
        . Shrubbery.branch @AddressLine3  show
        . Shrubbery.branch @AddressLevel4 show
        . Shrubbery.branch @AddressLevel3 show
        . Shrubbery.branch @AddressLevel2 show
        . Shrubbery.branch @AddressLevel1 show
        . Shrubbery.branch @Country       show
        . Shrubbery.branch @CountryName   show
        . Shrubbery.branch @PostalCode    show
        $ Shrubbery.branchEnd
    ) a

billing :: ( KnownNat branchIndex
           , branchIndex ~ FirstIndexOf billing BillingTypes
           )
        => billing -> Billing
billing =
  Billing . Shrubbery.unify

billingToBytes :: Billing -> LBS.ByteString
billingToBytes (Billing a) =
  LBS8.unwords
    [ "billing"
    , ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @FullName      fullNameToBytes
          . Shrubbery.branch @StreetAddress streetAddressToBytes
          . Shrubbery.branch @AddressLine1  addressLine1ToBytes
          . Shrubbery.branch @AddressLine2  addressLine2ToBytes
          . Shrubbery.branch @AddressLine3  addressLine3ToBytes
          . Shrubbery.branch @AddressLevel4 addressLevel4ToBytes
          . Shrubbery.branch @AddressLevel3 addressLevel3ToBytes
          . Shrubbery.branch @AddressLevel2 addressLevel2ToBytes
          . Shrubbery.branch @AddressLevel1 addressLevel1ToBytes
          . Shrubbery.branch @Country       countryToBytes
          . Shrubbery.branch @CountryName   countryNameToBytes
          . Shrubbery.branch @PostalCode    postalCodeToBytes
          $ Shrubbery.branchEnd
      ) a
    ]

billingToText :: Billing -> T.Text
billingToText (Billing a) =
  T.unwords
    [ "billing"
    , ( Shrubbery.dissect
          . Shrubbery.branchBuild
          . Shrubbery.branch @FullName      fullNameToText
          . Shrubbery.branch @StreetAddress streetAddressToText
          . Shrubbery.branch @AddressLine1  addressLine1ToText
          . Shrubbery.branch @AddressLine2  addressLine2ToText
          . Shrubbery.branch @AddressLine3  addressLine3ToText
          . Shrubbery.branch @AddressLevel4 addressLevel4ToText
          . Shrubbery.branch @AddressLevel3 addressLevel3ToText
          . Shrubbery.branch @AddressLevel2 addressLevel2ToText
          . Shrubbery.branch @AddressLevel1 addressLevel1ToText
          . Shrubbery.branch @Country       countryToText
          . Shrubbery.branch @CountryName   countryNameToText
          . Shrubbery.branch @PostalCode    postalCodeToText
          $ Shrubbery.branchEnd
      ) a
    ]

newtype Mobile = Mobile Telephone
  deriving Show

mobile :: Mobile
mobile = Mobile Telephone

mobileToBytes :: Mobile -> LBS.ByteString
mobileToBytes (Mobile tel) = "mobile " <> telephoneToBytes tel

mobileToText :: Mobile -> T.Text
mobileToText (Mobile tel) = "mobile " <> telephoneToText tel

newtype Fax = Fax Telephone
  deriving Show

fax :: Fax
fax = Fax Telephone

faxToBytes :: Fax -> LBS.ByteString
faxToBytes (Fax tel) = "fax " <> telephoneToBytes tel

faxToText :: Fax -> T.Text
faxToText (Fax tel) = "fax " <> telephoneToText tel

newtype Pager = Pager Telephone
  deriving Show

pager :: Pager
pager = Pager Telephone

pagerToBytes :: Pager -> LBS.ByteString
pagerToBytes (Pager tel) = "pager " <> telephoneToBytes tel

pagerToText :: Pager -> T.Text
pagerToText (Pager tel) = "pager " <> telephoneToText tel


-- Personal Information
--
data FullName = FullName
  deriving Show

fullNameToBytes :: FullName -> LBS.ByteString
fullNameToBytes FullName = "name"

fullNameToText :: FullName -> T.Text
fullNameToText FullName = "name"

data HonorificPrefix = HonorificPrefix
  deriving Show

honorificPrefixToBytes :: HonorificPrefix -> LBS.ByteString
honorificPrefixToBytes HonorificPrefix = "honorific-prefix"

honorificPrefixToText :: HonorificPrefix -> T.Text
honorificPrefixToText HonorificPrefix = "honorific-prefix"

data GivenName = GivenName
  deriving Show

givenNameToBytes :: GivenName -> LBS.ByteString
givenNameToBytes GivenName = "given-name"

givenNameToText :: GivenName -> T.Text
givenNameToText GivenName = "given-name"

data AdditionalName = AdditionalName
  deriving Show

additionalNameToBytes :: AdditionalName -> LBS.ByteString
additionalNameToBytes AdditionalName = "additional-name"

additionalNameToText :: AdditionalName -> T.Text
additionalNameToText AdditionalName = "additional-name"

data FamilyName = FamilyName
  deriving Show

familyNameToBytes :: FamilyName -> LBS.ByteString
familyNameToBytes FamilyName = "family-name"

familyNameToText :: FamilyName -> T.Text
familyNameToText FamilyName = "family-name"

data HonorificSuffix = HonorificSuffix
  deriving Show

honorificSuffixToBytes :: HonorificSuffix -> LBS.ByteString
honorificSuffixToBytes HonorificSuffix = "honorific-suffix"

honorificSuffixToText :: HonorificSuffix -> T.Text
honorificSuffixToText HonorificSuffix = "honorific-suffix"

data Nickname = Nickname
  deriving Show

nicknameToBytes :: Nickname -> LBS.ByteString
nicknameToBytes Nickname = "nickname"

nicknameToText :: Nickname -> T.Text
nicknameToText Nickname = "nickname"

data Username = Username
  deriving Show

usernameToBytes :: Username -> LBS.ByteString
usernameToBytes Username = "username"

usernameToText :: Username -> T.Text
usernameToText Username = "username"

data NewPassword = NewPassword
  deriving Show

newPasswordToBytes :: NewPassword -> LBS.ByteString
newPasswordToBytes NewPassword = "new-password"

newPasswordToText :: NewPassword -> T.Text
newPasswordToText NewPassword = "new-password"

data CurrentPassword = CurrentPassword
  deriving Show

currentPasswordToBytes :: CurrentPassword -> LBS.ByteString
currentPasswordToBytes CurrentPassword = "current-password"

currentPasswordToText :: CurrentPassword -> T.Text
currentPasswordToText CurrentPassword = "current-password"

data OneTimeCode = OneTimeCode
  deriving Show

oneTimeCodeToBytes :: OneTimeCode -> LBS.ByteString
oneTimeCodeToBytes OneTimeCode = "one-time-code"

oneTimeCodeToText :: OneTimeCode -> T.Text
oneTimeCodeToText OneTimeCode = "one-time-code"

-- Contact Information
--
data Email = Email
  deriving Show

emailToBytes :: Email -> LBS.ByteString
emailToBytes Email = "email"

emailToText :: Email -> T.Text
emailToText Email = "email"

data InstantMessagingProtocol = InstantMessagingProtocol
  deriving Show

instantMessagingProtocolToBytes :: InstantMessagingProtocol -> LBS.ByteString
instantMessagingProtocolToBytes InstantMessagingProtocol = "impp"

instantMessagingProtocolToText :: InstantMessagingProtocol -> T.Text
instantMessagingProtocolToText InstantMessagingProtocol = "impp"

data Telephone = Telephone
  deriving Show

telephoneToBytes :: Telephone -> LBS.ByteString
telephoneToBytes Telephone = "tel"

telephoneToText :: Telephone -> T.Text
telephoneToText Telephone = "tel"

data TelephoneCountryCode = TelephoneCountryCode
  deriving Show

telephoneCountryCodeToBytes :: TelephoneCountryCode -> LBS.ByteString
telephoneCountryCodeToBytes TelephoneCountryCode = "tel-country-code"

telephoneCountryCodeToText :: TelephoneCountryCode -> T.Text
telephoneCountryCodeToText TelephoneCountryCode = "tel-country-code"

data TelephoneNational = TelephoneNational
  deriving Show

telephoneNationalToBytes :: TelephoneNational -> LBS.ByteString
telephoneNationalToBytes TelephoneNational = "tel-national"

telephoneNationalToText :: TelephoneNational -> T.Text
telephoneNationalToText TelephoneNational = "tel-national"

data TelephoneAreaCode = TelephoneAreaCode
  deriving Show

telephoneAreaCodeToBytes :: TelephoneAreaCode -> LBS.ByteString
telephoneAreaCodeToBytes TelephoneAreaCode = "tel-area-code"

telephoneAreaCodeToText :: TelephoneAreaCode -> T.Text
telephoneAreaCodeToText TelephoneAreaCode = "tel-area-code"

data TelephoneLocal = TelephoneLocal
  deriving Show

telephoneLocalToBytes :: TelephoneLocal -> LBS.ByteString
telephoneLocalToBytes TelephoneLocal = "tel-local"

telephoneLocalToText :: TelephoneLocal -> T.Text
telephoneLocalToText TelephoneLocal = "tel-local"

data TelephoneLocalPrefix = TelephoneLocalPrefix
  deriving Show

telephoneLocalPrefixToBytes :: TelephoneLocalPrefix -> LBS.ByteString
telephoneLocalPrefixToBytes TelephoneLocalPrefix = "tel-local-prefix"

telephoneLocalPrefixToText :: TelephoneLocalPrefix -> T.Text
telephoneLocalPrefixToText TelephoneLocalPrefix = "tel-local-prefix"

data TelephoneLocalSuffix = TelephoneLocalSuffix
  deriving Show

telephoneLocalSuffixToBytes :: TelephoneLocalSuffix -> LBS.ByteString
telephoneLocalSuffixToBytes TelephoneLocalSuffix = "tel-local-suffix"

telephoneLocalSuffixToText :: TelephoneLocalSuffix -> T.Text
telephoneLocalSuffixToText TelephoneLocalSuffix = "tel-local-suffix"

data TelephoneExtension = TelephoneExtension
  deriving Show

telephoneExtensionToBytes :: TelephoneExtension -> LBS.ByteString
telephoneExtensionToBytes TelephoneExtension = "tel-extension"

telephoneExtensionToText :: TelephoneExtension -> T.Text
telephoneExtensionToText TelephoneExtension = "tel-extension"

data Organization = Organization
  deriving Show

organizationToBytes :: Organization -> LBS.ByteString
organizationToBytes Organization = "organization"

organizationToText :: Organization -> T.Text
organizationToText Organization = "organization"

data OrganizationTitle = OrganizationTitle
  deriving Show

organizationTitleToBytes :: OrganizationTitle -> LBS.ByteString
organizationTitleToBytes OrganizationTitle = "organization-title"

organizationTitleToText :: OrganizationTitle -> T.Text
organizationTitleToText OrganizationTitle = "organization-title"

-- Address Information
--
data StreetAddress = StreetAddress
  deriving Show

streetAddressToBytes :: StreetAddress -> LBS.ByteString
streetAddressToBytes StreetAddress = "street-address"

streetAddressToText :: StreetAddress -> T.Text
streetAddressToText StreetAddress = "street-address"

data AddressLine1 = AddressLine1
  deriving Show

addressLine1ToBytes :: AddressLine1 -> LBS.ByteString
addressLine1ToBytes AddressLine1 = "address-line1"

addressLine1ToText :: AddressLine1 -> T.Text
addressLine1ToText AddressLine1 = "address-line1"

data AddressLine2 = AddressLine2
  deriving Show

addressLine2ToBytes :: AddressLine2 -> LBS.ByteString
addressLine2ToBytes AddressLine2 = "address-line2"

addressLine2ToText :: AddressLine2 -> T.Text
addressLine2ToText AddressLine2 = "address-line2"

data AddressLine3 = AddressLine3
  deriving Show

addressLine3ToBytes :: AddressLine3 -> LBS.ByteString
addressLine3ToBytes AddressLine3 = "address-line3"

addressLine3ToText :: AddressLine3 -> T.Text
addressLine3ToText AddressLine3 = "address-line3"

data AddressLevel4 = AddressLevel4
  deriving Show

addressLevel4ToBytes :: AddressLevel4 -> LBS.ByteString
addressLevel4ToBytes AddressLevel4 = "address-level4"

addressLevel4ToText :: AddressLevel4 -> T.Text
addressLevel4ToText AddressLevel4 = "address-level4"

data AddressLevel3 = AddressLevel3
  deriving Show

addressLevel3ToBytes :: AddressLevel3 -> LBS.ByteString
addressLevel3ToBytes AddressLevel3 = "address-level3"

addressLevel3ToText :: AddressLevel3 -> T.Text
addressLevel3ToText AddressLevel3 = "address-level3"

data AddressLevel2 = AddressLevel2
  deriving Show

addressLevel2ToBytes :: AddressLevel2 -> LBS.ByteString
addressLevel2ToBytes AddressLevel2 = "address-level2"

addressLevel2ToText :: AddressLevel2 -> T.Text
addressLevel2ToText AddressLevel2 = "address-level2"

data AddressLevel1 = AddressLevel1
  deriving Show

addressLevel1ToBytes :: AddressLevel1 -> LBS.ByteString
addressLevel1ToBytes AddressLevel1 = "address-level1"

addressLevel1ToText :: AddressLevel1 -> T.Text
addressLevel1ToText AddressLevel1 = "address-level1"

data Country = Country
  deriving Show

countryToBytes :: Country -> LBS.ByteString
countryToBytes Country = "country"

countryToText :: Country -> T.Text
countryToText Country = "country"

data CountryName = CountryName
  deriving Show

countryNameToBytes :: CountryName -> LBS.ByteString
countryNameToBytes CountryName = "country-name"

countryNameToText :: CountryName -> T.Text
countryNameToText CountryName = "country-name"

data PostalCode = PostalCode
  deriving Show

postalCodeToBytes :: PostalCode -> LBS.ByteString
postalCodeToBytes PostalCode = "postal-code"

postalCodeToText :: PostalCode -> T.Text
postalCodeToText PostalCode = "postal-code"

-- Credit Card Information
--
data CreditCardFullName = CreditCardFullName
  deriving Show

creditCardFullNameToBytes :: CreditCardFullName -> LBS.ByteString
creditCardFullNameToBytes CreditCardFullName = "cc-name"

creditCardFullNameToText :: CreditCardFullName -> T.Text
creditCardFullNameToText CreditCardFullName = "cc-name"

data CreditCardGivenName = CreditCardGivenName
  deriving Show

creditCardGivenNameToBytes :: CreditCardGivenName -> LBS.ByteString
creditCardGivenNameToBytes CreditCardGivenName = "cc-given-name"

creditCardGivenNameToText :: CreditCardGivenName -> T.Text
creditCardGivenNameToText CreditCardGivenName = "cc-given-name"

data CreditCardAdditionalName = CreditCardAdditionalName
  deriving Show

creditCardAdditionalNameToBytes :: CreditCardAdditionalName -> LBS.ByteString
creditCardAdditionalNameToBytes CreditCardAdditionalName = "cc-additional-name"

creditCardAdditionalNameToText :: CreditCardAdditionalName -> T.Text
creditCardAdditionalNameToText CreditCardAdditionalName = "cc-additional-name"

data CreditCardFamilyName = CreditCardFamilyName
  deriving Show

creditCardFamilyNameToBytes :: CreditCardFamilyName -> LBS.ByteString
creditCardFamilyNameToBytes CreditCardFamilyName = "cc-family-name"

creditCardFamilyNameToText :: CreditCardFamilyName -> T.Text
creditCardFamilyNameToText CreditCardFamilyName = "cc-family-name"

data CreditCardNumber = CreditCardNumber
  deriving Show

creditCardNumberToBytes :: CreditCardNumber -> LBS.ByteString
creditCardNumberToBytes CreditCardNumber = "cc-number"

creditCardNumberToText :: CreditCardNumber -> T.Text
creditCardNumberToText CreditCardNumber = "cc-number"

data CreditCardExpiration = CreditCardExpiration
  deriving Show

creditCardExpirationToBytes :: CreditCardExpiration -> LBS.ByteString
creditCardExpirationToBytes CreditCardExpiration = "cc-exp"

creditCardExpirationToText :: CreditCardExpiration -> T.Text
creditCardExpirationToText CreditCardExpiration = "cc-exp"

data CreditCardExpirationMonth = CreditCardExpirationMonth
  deriving Show

creditCardExpirationMonthToBytes :: CreditCardExpirationMonth -> LBS.ByteString
creditCardExpirationMonthToBytes CreditCardExpirationMonth = "cc-exp-month"

creditCardExpirationMonthToText :: CreditCardExpirationMonth -> T.Text
creditCardExpirationMonthToText CreditCardExpirationMonth = "cc-exp-month"

data CreditCardExpirationYear = CreditCardExpirationYear
  deriving Show

creditCardExpirationYearToBytes :: CreditCardExpirationYear -> LBS.ByteString
creditCardExpirationYearToBytes CreditCardExpirationYear = "cc-exp-year"

creditCardExpirationYearToText :: CreditCardExpirationYear -> T.Text
creditCardExpirationYearToText CreditCardExpirationYear = "cc-exp-year"

data CreditCardSecurityCode = CreditCardSecurityCode
  deriving Show

creditCardSecurityCodeToBytes :: CreditCardSecurityCode -> LBS.ByteString
creditCardSecurityCodeToBytes CreditCardSecurityCode = "cc-csc"

creditCardSecurityCodeToText :: CreditCardSecurityCode -> T.Text
creditCardSecurityCodeToText CreditCardSecurityCode = "cc-csc"

data CreditCardType = CreditCardType
  deriving Show

creditCardTypeToBytes :: CreditCardType -> LBS.ByteString
creditCardTypeToBytes CreditCardType = "cc-type"

creditCardTypeToText :: CreditCardType -> T.Text
creditCardTypeToText CreditCardType = "cc-type"

-- Transaction Details
--
data TransactionCurrency = TransactionCurrency
  deriving Show

transactionCurrencyToBytes :: TransactionCurrency -> LBS.ByteString
transactionCurrencyToBytes TransactionCurrency = "transaction-currency"

transactionCurrencyToText :: TransactionCurrency -> T.Text
transactionCurrencyToText TransactionCurrency = "transaction-currency"

data TransactionAmount = TransactionAmount
  deriving Show

transactionAmountToBytes :: TransactionAmount -> LBS.ByteString
transactionAmountToBytes TransactionAmount = "transaction-amount"

transactionAmountToText :: TransactionAmount -> T.Text
transactionAmountToText TransactionAmount = "transaction-amount"

-- Miscellaneous
--
data Birthday = Birthday
  deriving Show

birthdayToBytes :: Birthday -> LBS.ByteString
birthdayToBytes Birthday = "bday"

birthdayToText :: Birthday -> T.Text
birthdayToText Birthday = "bday"

data BirthdayDay = BirthdayDay
  deriving Show

birthdayDayToBytes :: BirthdayDay -> LBS.ByteString
birthdayDayToBytes BirthdayDay = "bday-day"

birthdayDayToText :: BirthdayDay -> T.Text
birthdayDayToText BirthdayDay = "bday-day"

data BirthdayMonth = BirthdayMonth
  deriving Show

birthdayMonthToBytes :: BirthdayMonth -> LBS.ByteString
birthdayMonthToBytes BirthdayMonth = "bday-month"

birthdayMonthToText :: BirthdayMonth -> T.Text
birthdayMonthToText BirthdayMonth = "bday-month"

data BirthdayYear = BirthdayYear
  deriving Show

birthdayYearToBytes :: BirthdayYear -> LBS.ByteString
birthdayYearToBytes BirthdayYear = "bday-year"

birthdayYearToText :: BirthdayYear -> T.Text
birthdayYearToText BirthdayYear = "bday-year"

data Language = Language
  deriving Show

languageToBytes :: Language -> LBS.ByteString
languageToBytes Language = "language"

languageToText :: Language -> T.Text
languageToText Language = "language"

data Sex = Sex
  deriving Show

sexToBytes :: Sex -> LBS.ByteString
sexToBytes Sex = "sex"

sexToText :: Sex -> T.Text
sexToText Sex = "sex"

data Url = Url
  deriving Show

urlToBytes :: Url -> LBS.ByteString
urlToBytes Url = "url"

urlToText :: Url -> T.Text
urlToText Url = "url"

data Photo = Photo
  deriving Show

photoToBytes :: Photo -> LBS.ByteString
photoToBytes Photo = "photo"

photoToText :: Photo -> T.Text
photoToText Photo = "photo"
