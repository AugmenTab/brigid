{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Types.AutocompleteToken
  ( AutocompleteToken
  , AutocompleteTokenTypes
  , mkAutocompleteToken
  , autocompleteTokenToBytes
  , autocompleteTokenToText
  , Modifier
  , section
  , home
  , work
  , shipping
  , billing
  , mobile
  , fax
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
import Data.Kind (Type)
import Data.Text qualified as T
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Types.Boolean (OnOff, onOffToBytes, onOffToText)

newtype AutocompleteToken =
  AutocompleteToken (Shrubbery.Union AutocompleteTokenTypes)

instance Show AutocompleteToken where
  show (AutocompleteToken token) =
    ( Shrubbery.dissect
        . Shrubbery.branchBuild
        . Shrubbery.branch @OnOff                                 show
        . Shrubbery.branch @(Modifier FullName)                   show
        . Shrubbery.branch @(Modifier HonorificPrefix)            show
        . Shrubbery.branch @(Modifier GivenName)                  show
        . Shrubbery.branch @(Modifier AdditionalName)             show
        . Shrubbery.branch @(Modifier FamilyName)                 show
        . Shrubbery.branch @(Modifier HonorificSuffix)            show
        . Shrubbery.branch @(Modifier Nickname)                   show
        . Shrubbery.branch @(Modifier Username)                   show
        . Shrubbery.branch @(Modifier NewPassword)                show
        . Shrubbery.branch @(Modifier CurrentPassword)            show
        . Shrubbery.branch @(Modifier OneTimeCode)                show
        . Shrubbery.branch @(Modifier Email)                      show
        . Shrubbery.branch @(Modifier InstantMessagingProtocol)   show
        . Shrubbery.branch @(Modifier Telephone)                  show
        . Shrubbery.branch @(Modifier TelephoneCountryCode)       show
        . Shrubbery.branch @(Modifier TelephoneNational)          show
        . Shrubbery.branch @(Modifier TelephoneAreaCode)          show
        . Shrubbery.branch @(Modifier TelephoneLocal)             show
        . Shrubbery.branch @(Modifier TelephoneLocalPrefix)       show
        . Shrubbery.branch @(Modifier TelephoneLocalSuffix)       show
        . Shrubbery.branch @(Modifier TelephoneExtension)         show
        . Shrubbery.branch @(Modifier Organization)               show
        . Shrubbery.branch @(Modifier OrganizationTitle)          show
        . Shrubbery.branch @(Modifier StreetAddress)              show
        . Shrubbery.branch @(Modifier AddressLine1)               show
        . Shrubbery.branch @(Modifier AddressLine2)               show
        . Shrubbery.branch @(Modifier AddressLine3)               show
        . Shrubbery.branch @(Modifier AddressLevel4)              show
        . Shrubbery.branch @(Modifier AddressLevel3)              show
        . Shrubbery.branch @(Modifier AddressLevel2)              show
        . Shrubbery.branch @(Modifier AddressLevel1)              show
        . Shrubbery.branch @(Modifier Country)                    show
        . Shrubbery.branch @(Modifier CountryName)                show
        . Shrubbery.branch @(Modifier PostalCode)                 show
        . Shrubbery.branch @(Modifier CreditCardFullName)         show
        . Shrubbery.branch @(Modifier CreditCardGivenName)        show
        . Shrubbery.branch @(Modifier CreditCardAdditionalName)   show
        . Shrubbery.branch @(Modifier CreditCardFamilyName)       show
        . Shrubbery.branch @(Modifier CreditCardNumber)           show
        . Shrubbery.branch @(Modifier CreditCardExpiration)       show
        . Shrubbery.branch @(Modifier CreditCardExpirationMonth)  show
        . Shrubbery.branch @(Modifier CreditCardExpirationYear)   show
        . Shrubbery.branch @(Modifier CreditCardSecurityCode)     show
        . Shrubbery.branch @(Modifier CreditCardType)             show
        . Shrubbery.branch @(Modifier TransactionCurrency)        show
        . Shrubbery.branch @(Modifier TransactionAmount)          show
        . Shrubbery.branch @(Modifier Birthday)                   show
        . Shrubbery.branch @(Modifier BirthdayDay)                show
        . Shrubbery.branch @(Modifier BirthdayMonth)              show
        . Shrubbery.branch @(Modifier BirthdayYear)               show
        . Shrubbery.branch @(Modifier Language)                   show
        . Shrubbery.branch @(Modifier Sex)                        show
        . Shrubbery.branch @(Modifier Url)                        show
        . Shrubbery.branch @(Modifier Photo)                      show
        . Shrubbery.branch @FullName                              show
        . Shrubbery.branch @HonorificPrefix                       show
        . Shrubbery.branch @GivenName                             show
        . Shrubbery.branch @AdditionalName                        show
        . Shrubbery.branch @FamilyName                            show
        . Shrubbery.branch @HonorificSuffix                       show
        . Shrubbery.branch @Nickname                              show
        . Shrubbery.branch @Username                              show
        . Shrubbery.branch @NewPassword                           show
        . Shrubbery.branch @CurrentPassword                       show
        . Shrubbery.branch @OneTimeCode                           show
        . Shrubbery.branch @Email                                 show
        . Shrubbery.branch @InstantMessagingProtocol              show
        . Shrubbery.branch @Telephone                             show
        . Shrubbery.branch @TelephoneCountryCode                  show
        . Shrubbery.branch @TelephoneNational                     show
        . Shrubbery.branch @TelephoneAreaCode                     show
        . Shrubbery.branch @TelephoneLocal                        show
        . Shrubbery.branch @TelephoneLocalPrefix                  show
        . Shrubbery.branch @TelephoneLocalSuffix                  show
        . Shrubbery.branch @TelephoneExtension                    show
        . Shrubbery.branch @Organization                          show
        . Shrubbery.branch @OrganizationTitle                     show
        . Shrubbery.branch @StreetAddress                         show
        . Shrubbery.branch @AddressLine1                          show
        . Shrubbery.branch @AddressLine2                          show
        . Shrubbery.branch @AddressLine3                          show
        . Shrubbery.branch @AddressLevel4                         show
        . Shrubbery.branch @AddressLevel3                         show
        . Shrubbery.branch @AddressLevel2                         show
        . Shrubbery.branch @AddressLevel1                         show
        . Shrubbery.branch @Country                               show
        . Shrubbery.branch @CountryName                           show
        . Shrubbery.branch @PostalCode                            show
        . Shrubbery.branch @CreditCardFullName                    show
        . Shrubbery.branch @CreditCardGivenName                   show
        . Shrubbery.branch @CreditCardAdditionalName              show
        . Shrubbery.branch @CreditCardFamilyName                  show
        . Shrubbery.branch @CreditCardNumber                      show
        . Shrubbery.branch @CreditCardExpiration                  show
        . Shrubbery.branch @CreditCardExpirationMonth             show
        . Shrubbery.branch @CreditCardExpirationYear              show
        . Shrubbery.branch @CreditCardSecurityCode                show
        . Shrubbery.branch @CreditCardType                        show
        . Shrubbery.branch @TransactionCurrency                   show
        . Shrubbery.branch @TransactionAmount                     show
        . Shrubbery.branch @Birthday                              show
        . Shrubbery.branch @BirthdayDay                           show
        . Shrubbery.branch @BirthdayMonth                         show
        . Shrubbery.branch @BirthdayYear                          show
        . Shrubbery.branch @Language                              show
        . Shrubbery.branch @Sex                                   show
        . Shrubbery.branch @Url                                   show
        . Shrubbery.branch @Photo                                 show
        $ Shrubbery.branchEnd
    ) token

type AutocompleteTokenTypes =
  [ OnOff
  , Modifier FullName
  , Modifier HonorificPrefix
  , Modifier GivenName
  , Modifier AdditionalName
  , Modifier FamilyName
  , Modifier HonorificSuffix
  , Modifier Nickname
  , Modifier Username
  , Modifier NewPassword
  , Modifier CurrentPassword
  , Modifier OneTimeCode
  , Modifier Email
  , Modifier InstantMessagingProtocol
  , Modifier Telephone
  , Modifier TelephoneCountryCode
  , Modifier TelephoneNational
  , Modifier TelephoneAreaCode
  , Modifier TelephoneLocal
  , Modifier TelephoneLocalPrefix
  , Modifier TelephoneLocalSuffix
  , Modifier TelephoneExtension
  , Modifier Organization
  , Modifier OrganizationTitle
  , Modifier StreetAddress
  , Modifier AddressLine1
  , Modifier AddressLine2
  , Modifier AddressLine3
  , Modifier AddressLevel4
  , Modifier AddressLevel3
  , Modifier AddressLevel2
  , Modifier AddressLevel1
  , Modifier Country
  , Modifier CountryName
  , Modifier PostalCode
  , Modifier CreditCardFullName
  , Modifier CreditCardGivenName
  , Modifier CreditCardAdditionalName
  , Modifier CreditCardFamilyName
  , Modifier CreditCardNumber
  , Modifier CreditCardExpiration
  , Modifier CreditCardExpirationMonth
  , Modifier CreditCardExpirationYear
  , Modifier CreditCardSecurityCode
  , Modifier CreditCardType
  , Modifier TransactionCurrency
  , Modifier TransactionAmount
  , Modifier Birthday
  , Modifier BirthdayDay
  , Modifier BirthdayMonth
  , Modifier BirthdayYear
  , Modifier Language
  , Modifier Sex
  , Modifier Url
  , Modifier Photo
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
      . Shrubbery.branch @OnOff                                onOffToBytes
      . Shrubbery.branch @(Modifier FullName)                  (modifierToBytes fullNameToBytes)
      . Shrubbery.branch @(Modifier HonorificPrefix)           (modifierToBytes honorificPrefixToBytes)
      . Shrubbery.branch @(Modifier GivenName)                 (modifierToBytes givenNameToBytes)
      . Shrubbery.branch @(Modifier AdditionalName)            (modifierToBytes additionalNameToBytes)
      . Shrubbery.branch @(Modifier FamilyName)                (modifierToBytes familyNameToBytes)
      . Shrubbery.branch @(Modifier HonorificSuffix)           (modifierToBytes honorificSuffixToBytes)
      . Shrubbery.branch @(Modifier Nickname)                  (modifierToBytes nicknameToBytes)
      . Shrubbery.branch @(Modifier Username)                  (modifierToBytes usernameToBytes)
      . Shrubbery.branch @(Modifier NewPassword)               (modifierToBytes newPasswordToBytes)
      . Shrubbery.branch @(Modifier CurrentPassword)           (modifierToBytes currentPasswordToBytes)
      . Shrubbery.branch @(Modifier OneTimeCode)               (modifierToBytes oneTimeCodeToBytes)
      . Shrubbery.branch @(Modifier Email)                     (modifierToBytes emailToBytes)
      . Shrubbery.branch @(Modifier InstantMessagingProtocol)  (modifierToBytes instantMessagingProtocolToBytes)
      . Shrubbery.branch @(Modifier Telephone)                 (modifierToBytes telephoneToBytes)
      . Shrubbery.branch @(Modifier TelephoneCountryCode)      (modifierToBytes telephoneCountryCodeToBytes)
      . Shrubbery.branch @(Modifier TelephoneNational)         (modifierToBytes telephoneNationalToBytes)
      . Shrubbery.branch @(Modifier TelephoneAreaCode)         (modifierToBytes telephoneAreaCodeToBytes)
      . Shrubbery.branch @(Modifier TelephoneLocal)            (modifierToBytes telephoneLocalToBytes)
      . Shrubbery.branch @(Modifier TelephoneLocalPrefix)      (modifierToBytes telephoneLocalPrefixToBytes)
      . Shrubbery.branch @(Modifier TelephoneLocalSuffix)      (modifierToBytes telephoneLocalSuffixToBytes)
      . Shrubbery.branch @(Modifier TelephoneExtension)        (modifierToBytes telephoneExtensionToBytes)
      . Shrubbery.branch @(Modifier Organization)              (modifierToBytes organizationToBytes)
      . Shrubbery.branch @(Modifier OrganizationTitle)         (modifierToBytes organizationTitleToBytes)
      . Shrubbery.branch @(Modifier StreetAddress)             (modifierToBytes streetAddressToBytes)
      . Shrubbery.branch @(Modifier AddressLine1)              (modifierToBytes addressLine1ToBytes)
      . Shrubbery.branch @(Modifier AddressLine2)              (modifierToBytes addressLine2ToBytes)
      . Shrubbery.branch @(Modifier AddressLine3)              (modifierToBytes addressLine3ToBytes)
      . Shrubbery.branch @(Modifier AddressLevel4)             (modifierToBytes addressLevel4ToBytes)
      . Shrubbery.branch @(Modifier AddressLevel3)             (modifierToBytes addressLevel3ToBytes)
      . Shrubbery.branch @(Modifier AddressLevel2)             (modifierToBytes addressLevel2ToBytes)
      . Shrubbery.branch @(Modifier AddressLevel1)             (modifierToBytes addressLevel1ToBytes)
      . Shrubbery.branch @(Modifier Country)                   (modifierToBytes countryToBytes)
      . Shrubbery.branch @(Modifier CountryName)               (modifierToBytes countryNameToBytes)
      . Shrubbery.branch @(Modifier PostalCode)                (modifierToBytes postalCodeToBytes)
      . Shrubbery.branch @(Modifier CreditCardFullName)        (modifierToBytes creditCardFullNameToBytes)
      . Shrubbery.branch @(Modifier CreditCardGivenName)       (modifierToBytes creditCardGivenNameToBytes)
      . Shrubbery.branch @(Modifier CreditCardAdditionalName)  (modifierToBytes creditCardAdditionalNameToBytes)
      . Shrubbery.branch @(Modifier CreditCardFamilyName)      (modifierToBytes creditCardFamilyNameToBytes)
      . Shrubbery.branch @(Modifier CreditCardNumber)          (modifierToBytes creditCardNumberToBytes)
      . Shrubbery.branch @(Modifier CreditCardExpiration)      (modifierToBytes creditCardExpirationToBytes)
      . Shrubbery.branch @(Modifier CreditCardExpirationMonth) (modifierToBytes creditCardExpirationMonthToBytes)
      . Shrubbery.branch @(Modifier CreditCardExpirationYear)  (modifierToBytes creditCardExpirationYearToBytes)
      . Shrubbery.branch @(Modifier CreditCardSecurityCode)    (modifierToBytes creditCardSecurityCodeToBytes)
      . Shrubbery.branch @(Modifier CreditCardType)            (modifierToBytes creditCardTypeToBytes)
      . Shrubbery.branch @(Modifier TransactionCurrency)       (modifierToBytes transactionCurrencyToBytes)
      . Shrubbery.branch @(Modifier TransactionAmount)         (modifierToBytes transactionAmountToBytes)
      . Shrubbery.branch @(Modifier Birthday)                  (modifierToBytes birthdayToBytes)
      . Shrubbery.branch @(Modifier BirthdayDay)               (modifierToBytes birthdayDayToBytes)
      . Shrubbery.branch @(Modifier BirthdayMonth)             (modifierToBytes birthdayMonthToBytes)
      . Shrubbery.branch @(Modifier BirthdayYear)              (modifierToBytes birthdayYearToBytes)
      . Shrubbery.branch @(Modifier Language)                  (modifierToBytes languageToBytes)
      . Shrubbery.branch @(Modifier Sex)                       (modifierToBytes sexToBytes)
      . Shrubbery.branch @(Modifier Url)                       (modifierToBytes urlToBytes)
      . Shrubbery.branch @(Modifier Photo)                     (modifierToBytes photoToBytes)
      . Shrubbery.branch @FullName                             fullNameToBytes
      . Shrubbery.branch @HonorificPrefix                      honorificPrefixToBytes
      . Shrubbery.branch @GivenName                            givenNameToBytes
      . Shrubbery.branch @AdditionalName                       additionalNameToBytes
      . Shrubbery.branch @FamilyName                           familyNameToBytes
      . Shrubbery.branch @HonorificSuffix                      honorificSuffixToBytes
      . Shrubbery.branch @Nickname                             nicknameToBytes
      . Shrubbery.branch @Username                             usernameToBytes
      . Shrubbery.branch @NewPassword                          newPasswordToBytes
      . Shrubbery.branch @CurrentPassword                      currentPasswordToBytes
      . Shrubbery.branch @OneTimeCode                          oneTimeCodeToBytes
      . Shrubbery.branch @Email                                emailToBytes
      . Shrubbery.branch @InstantMessagingProtocol             instantMessagingProtocolToBytes
      . Shrubbery.branch @Telephone                            telephoneToBytes
      . Shrubbery.branch @TelephoneCountryCode                 telephoneCountryCodeToBytes
      . Shrubbery.branch @TelephoneNational                    telephoneNationalToBytes
      . Shrubbery.branch @TelephoneAreaCode                    telephoneAreaCodeToBytes
      . Shrubbery.branch @TelephoneLocal                       telephoneLocalToBytes
      . Shrubbery.branch @TelephoneLocalPrefix                 telephoneLocalPrefixToBytes
      . Shrubbery.branch @TelephoneLocalSuffix                 telephoneLocalSuffixToBytes
      . Shrubbery.branch @TelephoneExtension                   telephoneExtensionToBytes
      . Shrubbery.branch @Organization                         organizationToBytes
      . Shrubbery.branch @OrganizationTitle                    organizationTitleToBytes
      . Shrubbery.branch @StreetAddress                        streetAddressToBytes
      . Shrubbery.branch @AddressLine1                         addressLine1ToBytes
      . Shrubbery.branch @AddressLine2                         addressLine2ToBytes
      . Shrubbery.branch @AddressLine3                         addressLine3ToBytes
      . Shrubbery.branch @AddressLevel4                        addressLevel4ToBytes
      . Shrubbery.branch @AddressLevel3                        addressLevel3ToBytes
      . Shrubbery.branch @AddressLevel2                        addressLevel2ToBytes
      . Shrubbery.branch @AddressLevel1                        addressLevel1ToBytes
      . Shrubbery.branch @Country                              countryToBytes
      . Shrubbery.branch @CountryName                          countryNameToBytes
      . Shrubbery.branch @PostalCode                           postalCodeToBytes
      . Shrubbery.branch @CreditCardFullName                   creditCardFullNameToBytes
      . Shrubbery.branch @CreditCardGivenName                  creditCardGivenNameToBytes
      . Shrubbery.branch @CreditCardAdditionalName             creditCardAdditionalNameToBytes
      . Shrubbery.branch @CreditCardFamilyName                 creditCardFamilyNameToBytes
      . Shrubbery.branch @CreditCardNumber                     creditCardNumberToBytes
      . Shrubbery.branch @CreditCardExpiration                 creditCardExpirationToBytes
      . Shrubbery.branch @CreditCardExpirationMonth            creditCardExpirationMonthToBytes
      . Shrubbery.branch @CreditCardExpirationYear             creditCardExpirationYearToBytes
      . Shrubbery.branch @CreditCardSecurityCode               creditCardSecurityCodeToBytes
      . Shrubbery.branch @CreditCardType                       creditCardTypeToBytes
      . Shrubbery.branch @TransactionCurrency                  transactionCurrencyToBytes
      . Shrubbery.branch @TransactionAmount                    transactionAmountToBytes
      . Shrubbery.branch @Birthday                             birthdayToBytes
      . Shrubbery.branch @BirthdayDay                          birthdayDayToBytes
      . Shrubbery.branch @BirthdayMonth                        birthdayMonthToBytes
      . Shrubbery.branch @BirthdayYear                         birthdayYearToBytes
      . Shrubbery.branch @Language                             languageToBytes
      . Shrubbery.branch @Sex                                  sexToBytes
      . Shrubbery.branch @Url                                  urlToBytes
      . Shrubbery.branch @Photo                                photoToBytes
      $ Shrubbery.branchEnd
  ) token

autocompleteTokenToText :: AutocompleteToken -> T.Text
autocompleteTokenToText (AutocompleteToken token) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @OnOff                               onOffToText
      . Shrubbery.branch @(Modifier FullName)                 (modifierToText fullNameToText)
      . Shrubbery.branch @(Modifier HonorificPrefix)          (modifierToText honorificPrefixToText)
      . Shrubbery.branch @(Modifier GivenName)                (modifierToText givenNameToText)
      . Shrubbery.branch @(Modifier AdditionalName)           (modifierToText additionalNameToText)
      . Shrubbery.branch @(Modifier FamilyName)               (modifierToText familyNameToText)
      . Shrubbery.branch @(Modifier HonorificSuffix)          (modifierToText honorificSuffixToText)
      . Shrubbery.branch @(Modifier Nickname)                 (modifierToText nicknameToText)
      . Shrubbery.branch @(Modifier Username)                 (modifierToText usernameToText)
      . Shrubbery.branch @(Modifier NewPassword)              (modifierToText newPasswordToText)
      . Shrubbery.branch @(Modifier CurrentPassword)          (modifierToText currentPasswordToText)
      . Shrubbery.branch @(Modifier OneTimeCode)              (modifierToText oneTimeCodeToText)
      . Shrubbery.branch @(Modifier Email)                    (modifierToText emailToText)
      . Shrubbery.branch @(Modifier InstantMessagingProtocol) (modifierToText instantMessagingProtocolToText)
      . Shrubbery.branch @(Modifier Telephone)                (modifierToText telephoneToText)
      . Shrubbery.branch @(Modifier TelephoneCountryCode)     (modifierToText telephoneCountryCodeToText)
      . Shrubbery.branch @(Modifier TelephoneNational)        (modifierToText telephoneNationalToText)
      . Shrubbery.branch @(Modifier TelephoneAreaCode)        (modifierToText telephoneAreaCodeToText)
      . Shrubbery.branch @(Modifier TelephoneLocal)           (modifierToText telephoneLocalToText)
      . Shrubbery.branch @(Modifier TelephoneLocalPrefix)     (modifierToText telephoneLocalPrefixToText)
      . Shrubbery.branch @(Modifier TelephoneLocalSuffix)     (modifierToText telephoneLocalSuffixToText)
      . Shrubbery.branch @(Modifier TelephoneExtension)       (modifierToText telephoneExtensionToText)
      . Shrubbery.branch @(Modifier Organization)             (modifierToText organizationToText)
      . Shrubbery.branch @(Modifier OrganizationTitle)        (modifierToText organizationTitleToText)
      . Shrubbery.branch @(Modifier StreetAddress)            (modifierToText streetAddressToText)
      . Shrubbery.branch @(Modifier AddressLine1)             (modifierToText addressLine1ToText)
      . Shrubbery.branch @(Modifier AddressLine2)             (modifierToText addressLine2ToText)
      . Shrubbery.branch @(Modifier AddressLine3)             (modifierToText addressLine3ToText)
      . Shrubbery.branch @(Modifier AddressLevel4)            (modifierToText addressLevel4ToText)
      . Shrubbery.branch @(Modifier AddressLevel3)            (modifierToText addressLevel3ToText)
      . Shrubbery.branch @(Modifier AddressLevel2)            (modifierToText addressLevel2ToText)
      . Shrubbery.branch @(Modifier AddressLevel1)            (modifierToText addressLevel1ToText)
      . Shrubbery.branch @(Modifier Country)                  (modifierToText countryToText)
      . Shrubbery.branch @(Modifier CountryName)              (modifierToText countryNameToText)
      . Shrubbery.branch @(Modifier PostalCode)               (modifierToText postalCodeToText)
      . Shrubbery.branch @(Modifier CreditCardFullName)       (modifierToText creditCardFullNameToText)
      . Shrubbery.branch @(Modifier CreditCardGivenName)      (modifierToText creditCardGivenNameToText)
      . Shrubbery.branch @(Modifier CreditCardAdditionalName) (modifierToText creditCardAdditionalNameToText)
      . Shrubbery.branch @(Modifier CreditCardFamilyName)     (modifierToText creditCardFamilyNameToText)
      . Shrubbery.branch @(Modifier CreditCardNumber)         (modifierToText creditCardNumberToText)
      . Shrubbery.branch @(Modifier CreditCardExpiration)     (modifierToText creditCardExpirationToText)
      . Shrubbery.branch @(Modifier CreditCardExpirationMonth)(modifierToText creditCardExpirationMonthToText)
      . Shrubbery.branch @(Modifier CreditCardExpirationYear) (modifierToText creditCardExpirationYearToText)
      . Shrubbery.branch @(Modifier CreditCardSecurityCode)   (modifierToText creditCardSecurityCodeToText)
      . Shrubbery.branch @(Modifier CreditCardType)           (modifierToText creditCardTypeToText)
      . Shrubbery.branch @(Modifier TransactionCurrency)      (modifierToText transactionCurrencyToText)
      . Shrubbery.branch @(Modifier TransactionAmount)        (modifierToText transactionAmountToText)
      . Shrubbery.branch @(Modifier Birthday)                 (modifierToText birthdayToText)
      . Shrubbery.branch @(Modifier BirthdayDay)              (modifierToText birthdayDayToText)
      . Shrubbery.branch @(Modifier BirthdayMonth)            (modifierToText birthdayMonthToText)
      . Shrubbery.branch @(Modifier BirthdayYear)             (modifierToText birthdayYearToText)
      . Shrubbery.branch @(Modifier Language)                 (modifierToText languageToText)
      . Shrubbery.branch @(Modifier Sex)                      (modifierToText sexToText)
      . Shrubbery.branch @(Modifier Url)                      (modifierToText urlToText)
      . Shrubbery.branch @(Modifier Photo)                    (modifierToText photoToText)
      . Shrubbery.branch @FullName                             fullNameToText
      . Shrubbery.branch @HonorificPrefix                      honorificPrefixToText
      . Shrubbery.branch @GivenName                            givenNameToText
      . Shrubbery.branch @AdditionalName                       additionalNameToText
      . Shrubbery.branch @FamilyName                           familyNameToText
      . Shrubbery.branch @HonorificSuffix                      honorificSuffixToText
      . Shrubbery.branch @Nickname                             nicknameToText
      . Shrubbery.branch @Username                             usernameToText
      . Shrubbery.branch @NewPassword                          newPasswordToText
      . Shrubbery.branch @CurrentPassword                      currentPasswordToText
      . Shrubbery.branch @OneTimeCode                          oneTimeCodeToText
      . Shrubbery.branch @Email                                emailToText
      . Shrubbery.branch @InstantMessagingProtocol             instantMessagingProtocolToText
      . Shrubbery.branch @Telephone                            telephoneToText
      . Shrubbery.branch @TelephoneCountryCode                 telephoneCountryCodeToText
      . Shrubbery.branch @TelephoneNational                    telephoneNationalToText
      . Shrubbery.branch @TelephoneAreaCode                    telephoneAreaCodeToText
      . Shrubbery.branch @TelephoneLocal                       telephoneLocalToText
      . Shrubbery.branch @TelephoneLocalPrefix                 telephoneLocalPrefixToText
      . Shrubbery.branch @TelephoneLocalSuffix                 telephoneLocalSuffixToText
      . Shrubbery.branch @TelephoneExtension                   telephoneExtensionToText
      . Shrubbery.branch @Organization                         organizationToText
      . Shrubbery.branch @OrganizationTitle                    organizationTitleToText
      . Shrubbery.branch @StreetAddress                        streetAddressToText
      . Shrubbery.branch @AddressLine1                         addressLine1ToText
      . Shrubbery.branch @AddressLine2                         addressLine2ToText
      . Shrubbery.branch @AddressLine3                         addressLine3ToText
      . Shrubbery.branch @AddressLevel4                        addressLevel4ToText
      . Shrubbery.branch @AddressLevel3                        addressLevel3ToText
      . Shrubbery.branch @AddressLevel2                        addressLevel2ToText
      . Shrubbery.branch @AddressLevel1                        addressLevel1ToText
      . Shrubbery.branch @Country                              countryToText
      . Shrubbery.branch @CountryName                          countryNameToText
      . Shrubbery.branch @PostalCode                           postalCodeToText
      . Shrubbery.branch @CreditCardFullName                   creditCardFullNameToText
      . Shrubbery.branch @CreditCardGivenName                  creditCardGivenNameToText
      . Shrubbery.branch @CreditCardAdditionalName             creditCardAdditionalNameToText
      . Shrubbery.branch @CreditCardFamilyName                 creditCardFamilyNameToText
      . Shrubbery.branch @CreditCardNumber                     creditCardNumberToText
      . Shrubbery.branch @CreditCardExpiration                 creditCardExpirationToText
      . Shrubbery.branch @CreditCardExpirationMonth            creditCardExpirationMonthToText
      . Shrubbery.branch @CreditCardExpirationYear             creditCardExpirationYearToText
      . Shrubbery.branch @CreditCardSecurityCode               creditCardSecurityCodeToText
      . Shrubbery.branch @CreditCardType                       creditCardTypeToText
      . Shrubbery.branch @TransactionCurrency                  transactionCurrencyToText
      . Shrubbery.branch @TransactionAmount                    transactionAmountToText
      . Shrubbery.branch @Birthday                             birthdayToText
      . Shrubbery.branch @BirthdayDay                          birthdayDayToText
      . Shrubbery.branch @BirthdayMonth                        birthdayMonthToText
      . Shrubbery.branch @BirthdayYear                         birthdayYearToText
      . Shrubbery.branch @Language                             languageToText
      . Shrubbery.branch @Sex                                  sexToText
      . Shrubbery.branch @Url                                  urlToText
      . Shrubbery.branch @Photo                                photoToText
      $ Shrubbery.branchEnd
  ) token

type family Elem (x :: Type) (xs :: [Type]) :: Bool where
  Elem e '[]       = 'False
  Elem e (e ': ts) = 'True
  Elem e (t ': ts) = Elem e ts

-- Modifiers
--
data Modifier token where
  Section  :: ValidSection  token => String -> token -> Modifier token
  Home     :: ValidHome     token =>           token -> Modifier token
  Work     :: ValidWork     token =>           token -> Modifier token
  Shipping :: ValidShipping token =>           token -> Modifier token
  Billing  :: ValidBilling  token =>           token -> Modifier token
  Mobile   :: ValidMobile   token =>           token -> Modifier token
  Fax      :: ValidFax      token =>           token -> Modifier token
  Pager    :: ValidPager    token =>           token -> Modifier token

instance Show token => Show (Modifier token) where
  show = show

type ValidSection token = Elem token SectionTypes ~ 'True

type SectionTypes =
  [ FullName
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

section :: ValidSection token => String -> token -> Modifier token
section = Section

type ValidHome token = Elem token HomeTypes ~ 'True

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

home :: ValidHome token => token -> Modifier token
home = Home

type ValidWork token = Elem token WorkTypes ~ 'True

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

work :: ValidWork token => token -> Modifier token
work = Work

type ValidShipping token = Elem token ShippingTypes ~ 'True

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

shipping :: ValidShipping token => token -> Modifier token
shipping = Shipping

type ValidBilling token = Elem token BillingTypes ~ 'True

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

billing :: ValidBilling token => token -> Modifier token
billing = Billing

type ValidMobile token = Elem token MobileTypes ~ 'True

type MobileTypes = '[ Telephone ]

mobile :: ValidMobile token => token -> Modifier token
mobile = Mobile

type ValidFax token = Elem token FaxTypes ~ 'True

type FaxTypes = '[ Telephone ]

fax :: ValidFax token => token -> Modifier token
fax = Fax

type ValidPager token = Elem token PagerTypes ~ 'True

type PagerTypes = '[ Telephone ]

pager :: ValidPager token => token -> Modifier token
pager = Pager

modifierToBytes :: (token -> LBS.ByteString) -> Modifier token -> LBS.ByteString
modifierToBytes toBytes modifier =
  LBS8.unwords $
    case modifier of
      Section name token -> [ "section-" <> LBS8.pack name, toBytes token ]
      Home         token -> [ "home",                       toBytes token ]
      Work         token -> [ "work",                       toBytes token ]
      Shipping     token -> [ "shipping",                   toBytes token ]
      Billing      token -> [ "billing",                    toBytes token ]
      Mobile       token -> [ "mobile",                     toBytes token ]
      Fax          token -> [ "fax",                        toBytes token ]
      Pager        token -> [ "pager",                      toBytes token ]

modifierToText :: (token -> T.Text) -> Modifier token -> T.Text
modifierToText toText modifier =
  T.unwords $
    case modifier of
      Section name token -> [ "section-" <> T.pack name, toText token ]
      Home         token -> [ "home",                    toText token ]
      Work         token -> [ "work",                    toText token ]
      Shipping     token -> [ "shipping",                toText token ]
      Billing      token -> [ "billing",                 toText token ]
      Mobile       token -> [ "mobile",                  toText token ]
      Fax          token -> [ "fax",                     toText token ]
      Pager        token -> [ "pager",                   toText token ]

-- Personal Information
--
data FullName = FullName
  deriving (Eq, Show)

fullNameToBytes :: FullName -> LBS.ByteString
fullNameToBytes FullName = "name"

fullNameToText :: FullName -> T.Text
fullNameToText FullName = "name"

data HonorificPrefix = HonorificPrefix
  deriving (Eq, Show)

honorificPrefixToBytes :: HonorificPrefix -> LBS.ByteString
honorificPrefixToBytes HonorificPrefix = "honorific-prefix"

honorificPrefixToText :: HonorificPrefix -> T.Text
honorificPrefixToText HonorificPrefix = "honorific-prefix"

data GivenName = GivenName
  deriving (Eq, Show)

givenNameToBytes :: GivenName -> LBS.ByteString
givenNameToBytes GivenName = "given-name"

givenNameToText :: GivenName -> T.Text
givenNameToText GivenName = "given-name"

data AdditionalName = AdditionalName
  deriving (Eq, Show)

additionalNameToBytes :: AdditionalName -> LBS.ByteString
additionalNameToBytes AdditionalName = "additional-name"

additionalNameToText :: AdditionalName -> T.Text
additionalNameToText AdditionalName = "additional-name"

data FamilyName = FamilyName
  deriving (Eq, Show)

familyNameToBytes :: FamilyName -> LBS.ByteString
familyNameToBytes FamilyName = "family-name"

familyNameToText :: FamilyName -> T.Text
familyNameToText FamilyName = "family-name"

data HonorificSuffix = HonorificSuffix
  deriving (Eq, Show)

honorificSuffixToBytes :: HonorificSuffix -> LBS.ByteString
honorificSuffixToBytes HonorificSuffix = "honorific-suffix"

honorificSuffixToText :: HonorificSuffix -> T.Text
honorificSuffixToText HonorificSuffix = "honorific-suffix"

data Nickname = Nickname
  deriving (Eq, Show)

nicknameToBytes :: Nickname -> LBS.ByteString
nicknameToBytes Nickname = "nickname"

nicknameToText :: Nickname -> T.Text
nicknameToText Nickname = "nickname"

data Username = Username
  deriving (Eq, Show)

usernameToBytes :: Username -> LBS.ByteString
usernameToBytes Username = "username"

usernameToText :: Username -> T.Text
usernameToText Username = "username"

data NewPassword = NewPassword
  deriving (Eq, Show)

newPasswordToBytes :: NewPassword -> LBS.ByteString
newPasswordToBytes NewPassword = "new-password"

newPasswordToText :: NewPassword -> T.Text
newPasswordToText NewPassword = "new-password"

data CurrentPassword = CurrentPassword
  deriving (Eq, Show)

currentPasswordToBytes :: CurrentPassword -> LBS.ByteString
currentPasswordToBytes CurrentPassword = "current-password"

currentPasswordToText :: CurrentPassword -> T.Text
currentPasswordToText CurrentPassword = "current-password"

data OneTimeCode = OneTimeCode
  deriving (Eq, Show)

oneTimeCodeToBytes :: OneTimeCode -> LBS.ByteString
oneTimeCodeToBytes OneTimeCode = "one-time-code"

oneTimeCodeToText :: OneTimeCode -> T.Text
oneTimeCodeToText OneTimeCode = "one-time-code"

-- Contact Information
--
data Email = Email
  deriving (Eq, Show)

emailToBytes :: Email -> LBS.ByteString
emailToBytes Email = "email"

emailToText :: Email -> T.Text
emailToText Email = "email"

data InstantMessagingProtocol = InstantMessagingProtocol
  deriving (Eq, Show)

instantMessagingProtocolToBytes :: InstantMessagingProtocol -> LBS.ByteString
instantMessagingProtocolToBytes InstantMessagingProtocol = "impp"

instantMessagingProtocolToText :: InstantMessagingProtocol -> T.Text
instantMessagingProtocolToText InstantMessagingProtocol = "impp"

data Telephone = Telephone
  deriving (Eq, Show)

telephoneToBytes :: Telephone -> LBS.ByteString
telephoneToBytes Telephone = "tel"

telephoneToText :: Telephone -> T.Text
telephoneToText Telephone = "tel"

data TelephoneCountryCode = TelephoneCountryCode
  deriving (Eq, Show)

telephoneCountryCodeToBytes :: TelephoneCountryCode -> LBS.ByteString
telephoneCountryCodeToBytes TelephoneCountryCode = "tel-country-code"

telephoneCountryCodeToText :: TelephoneCountryCode -> T.Text
telephoneCountryCodeToText TelephoneCountryCode = "tel-country-code"

data TelephoneNational = TelephoneNational
  deriving (Eq, Show)

telephoneNationalToBytes :: TelephoneNational -> LBS.ByteString
telephoneNationalToBytes TelephoneNational = "tel-national"

telephoneNationalToText :: TelephoneNational -> T.Text
telephoneNationalToText TelephoneNational = "tel-national"

data TelephoneAreaCode = TelephoneAreaCode
  deriving (Eq, Show)

telephoneAreaCodeToBytes :: TelephoneAreaCode -> LBS.ByteString
telephoneAreaCodeToBytes TelephoneAreaCode = "tel-area-code"

telephoneAreaCodeToText :: TelephoneAreaCode -> T.Text
telephoneAreaCodeToText TelephoneAreaCode = "tel-area-code"

data TelephoneLocal = TelephoneLocal
  deriving (Eq, Show)

telephoneLocalToBytes :: TelephoneLocal -> LBS.ByteString
telephoneLocalToBytes TelephoneLocal = "tel-local"

telephoneLocalToText :: TelephoneLocal -> T.Text
telephoneLocalToText TelephoneLocal = "tel-local"

data TelephoneLocalPrefix = TelephoneLocalPrefix
  deriving (Eq, Show)

telephoneLocalPrefixToBytes :: TelephoneLocalPrefix -> LBS.ByteString
telephoneLocalPrefixToBytes TelephoneLocalPrefix = "tel-local-prefix"

telephoneLocalPrefixToText :: TelephoneLocalPrefix -> T.Text
telephoneLocalPrefixToText TelephoneLocalPrefix = "tel-local-prefix"

data TelephoneLocalSuffix = TelephoneLocalSuffix
  deriving (Eq, Show)

telephoneLocalSuffixToBytes :: TelephoneLocalSuffix -> LBS.ByteString
telephoneLocalSuffixToBytes TelephoneLocalSuffix = "tel-local-suffix"

telephoneLocalSuffixToText :: TelephoneLocalSuffix -> T.Text
telephoneLocalSuffixToText TelephoneLocalSuffix = "tel-local-suffix"

data TelephoneExtension = TelephoneExtension
  deriving (Eq, Show)

telephoneExtensionToBytes :: TelephoneExtension -> LBS.ByteString
telephoneExtensionToBytes TelephoneExtension = "tel-extension"

telephoneExtensionToText :: TelephoneExtension -> T.Text
telephoneExtensionToText TelephoneExtension = "tel-extension"

data Organization = Organization
  deriving (Eq, Show)

organizationToBytes :: Organization -> LBS.ByteString
organizationToBytes Organization = "organization"

organizationToText :: Organization -> T.Text
organizationToText Organization = "organization"

data OrganizationTitle = OrganizationTitle
  deriving (Eq, Show)

organizationTitleToBytes :: OrganizationTitle -> LBS.ByteString
organizationTitleToBytes OrganizationTitle = "organization-title"

organizationTitleToText :: OrganizationTitle -> T.Text
organizationTitleToText OrganizationTitle = "organization-title"

-- Address Information
--
data StreetAddress = StreetAddress
  deriving (Eq, Show)

streetAddressToBytes :: StreetAddress -> LBS.ByteString
streetAddressToBytes StreetAddress = "street-address"

streetAddressToText :: StreetAddress -> T.Text
streetAddressToText StreetAddress = "street-address"

data AddressLine1 = AddressLine1
  deriving (Eq, Show)

addressLine1ToBytes :: AddressLine1 -> LBS.ByteString
addressLine1ToBytes AddressLine1 = "address-line1"

addressLine1ToText :: AddressLine1 -> T.Text
addressLine1ToText AddressLine1 = "address-line1"

data AddressLine2 = AddressLine2
  deriving (Eq, Show)

addressLine2ToBytes :: AddressLine2 -> LBS.ByteString
addressLine2ToBytes AddressLine2 = "address-line2"

addressLine2ToText :: AddressLine2 -> T.Text
addressLine2ToText AddressLine2 = "address-line2"

data AddressLine3 = AddressLine3
  deriving (Eq, Show)

addressLine3ToBytes :: AddressLine3 -> LBS.ByteString
addressLine3ToBytes AddressLine3 = "address-line3"

addressLine3ToText :: AddressLine3 -> T.Text
addressLine3ToText AddressLine3 = "address-line3"

data AddressLevel4 = AddressLevel4
  deriving (Eq, Show)

addressLevel4ToBytes :: AddressLevel4 -> LBS.ByteString
addressLevel4ToBytes AddressLevel4 = "address-level4"

addressLevel4ToText :: AddressLevel4 -> T.Text
addressLevel4ToText AddressLevel4 = "address-level4"

data AddressLevel3 = AddressLevel3
  deriving (Eq, Show)

addressLevel3ToBytes :: AddressLevel3 -> LBS.ByteString
addressLevel3ToBytes AddressLevel3 = "address-level3"

addressLevel3ToText :: AddressLevel3 -> T.Text
addressLevel3ToText AddressLevel3 = "address-level3"

data AddressLevel2 = AddressLevel2
  deriving (Eq, Show)

addressLevel2ToBytes :: AddressLevel2 -> LBS.ByteString
addressLevel2ToBytes AddressLevel2 = "address-level2"

addressLevel2ToText :: AddressLevel2 -> T.Text
addressLevel2ToText AddressLevel2 = "address-level2"

data AddressLevel1 = AddressLevel1
  deriving (Eq, Show)

addressLevel1ToBytes :: AddressLevel1 -> LBS.ByteString
addressLevel1ToBytes AddressLevel1 = "address-level1"

addressLevel1ToText :: AddressLevel1 -> T.Text
addressLevel1ToText AddressLevel1 = "address-level1"

data Country = Country
  deriving (Eq, Show)

countryToBytes :: Country -> LBS.ByteString
countryToBytes Country = "country"

countryToText :: Country -> T.Text
countryToText Country = "country"

data CountryName = CountryName
  deriving (Eq, Show)

countryNameToBytes :: CountryName -> LBS.ByteString
countryNameToBytes CountryName = "country-name"

countryNameToText :: CountryName -> T.Text
countryNameToText CountryName = "country-name"

data PostalCode = PostalCode
  deriving (Eq, Show)

postalCodeToBytes :: PostalCode -> LBS.ByteString
postalCodeToBytes PostalCode = "postal-code"

postalCodeToText :: PostalCode -> T.Text
postalCodeToText PostalCode = "postal-code"

-- Credit Card Information
--
data CreditCardFullName = CreditCardFullName
  deriving (Eq, Show)

creditCardFullNameToBytes :: CreditCardFullName -> LBS.ByteString
creditCardFullNameToBytes CreditCardFullName = "cc-name"

creditCardFullNameToText :: CreditCardFullName -> T.Text
creditCardFullNameToText CreditCardFullName = "cc-name"

data CreditCardGivenName = CreditCardGivenName
  deriving (Eq, Show)

creditCardGivenNameToBytes :: CreditCardGivenName -> LBS.ByteString
creditCardGivenNameToBytes CreditCardGivenName = "cc-given-name"

creditCardGivenNameToText :: CreditCardGivenName -> T.Text
creditCardGivenNameToText CreditCardGivenName = "cc-given-name"

data CreditCardAdditionalName = CreditCardAdditionalName
  deriving (Eq, Show)

creditCardAdditionalNameToBytes :: CreditCardAdditionalName -> LBS.ByteString
creditCardAdditionalNameToBytes CreditCardAdditionalName = "cc-additional-name"

creditCardAdditionalNameToText :: CreditCardAdditionalName -> T.Text
creditCardAdditionalNameToText CreditCardAdditionalName = "cc-additional-name"

data CreditCardFamilyName = CreditCardFamilyName
  deriving (Eq, Show)

creditCardFamilyNameToBytes :: CreditCardFamilyName -> LBS.ByteString
creditCardFamilyNameToBytes CreditCardFamilyName = "cc-family-name"

creditCardFamilyNameToText :: CreditCardFamilyName -> T.Text
creditCardFamilyNameToText CreditCardFamilyName = "cc-family-name"

data CreditCardNumber = CreditCardNumber
  deriving (Eq, Show)

creditCardNumberToBytes :: CreditCardNumber -> LBS.ByteString
creditCardNumberToBytes CreditCardNumber = "cc-number"

creditCardNumberToText :: CreditCardNumber -> T.Text
creditCardNumberToText CreditCardNumber = "cc-number"

data CreditCardExpiration = CreditCardExpiration
  deriving (Eq, Show)

creditCardExpirationToBytes :: CreditCardExpiration -> LBS.ByteString
creditCardExpirationToBytes CreditCardExpiration = "cc-exp"

creditCardExpirationToText :: CreditCardExpiration -> T.Text
creditCardExpirationToText CreditCardExpiration = "cc-exp"

data CreditCardExpirationMonth = CreditCardExpirationMonth
  deriving (Eq, Show)

creditCardExpirationMonthToBytes :: CreditCardExpirationMonth -> LBS.ByteString
creditCardExpirationMonthToBytes CreditCardExpirationMonth = "cc-exp-month"

creditCardExpirationMonthToText :: CreditCardExpirationMonth -> T.Text
creditCardExpirationMonthToText CreditCardExpirationMonth = "cc-exp-month"

data CreditCardExpirationYear = CreditCardExpirationYear
  deriving (Eq, Show)

creditCardExpirationYearToBytes :: CreditCardExpirationYear -> LBS.ByteString
creditCardExpirationYearToBytes CreditCardExpirationYear = "cc-exp-year"

creditCardExpirationYearToText :: CreditCardExpirationYear -> T.Text
creditCardExpirationYearToText CreditCardExpirationYear = "cc-exp-year"

data CreditCardSecurityCode = CreditCardSecurityCode
  deriving (Eq, Show)

creditCardSecurityCodeToBytes :: CreditCardSecurityCode -> LBS.ByteString
creditCardSecurityCodeToBytes CreditCardSecurityCode = "cc-csc"

creditCardSecurityCodeToText :: CreditCardSecurityCode -> T.Text
creditCardSecurityCodeToText CreditCardSecurityCode = "cc-csc"

data CreditCardType = CreditCardType
  deriving (Eq, Show)

creditCardTypeToBytes :: CreditCardType -> LBS.ByteString
creditCardTypeToBytes CreditCardType = "cc-type"

creditCardTypeToText :: CreditCardType -> T.Text
creditCardTypeToText CreditCardType = "cc-type"

-- Transaction Details
--
data TransactionCurrency = TransactionCurrency
  deriving (Eq, Show)

transactionCurrencyToBytes :: TransactionCurrency -> LBS.ByteString
transactionCurrencyToBytes TransactionCurrency = "transaction-currency"

transactionCurrencyToText :: TransactionCurrency -> T.Text
transactionCurrencyToText TransactionCurrency = "transaction-currency"

data TransactionAmount = TransactionAmount
  deriving (Eq, Show)

transactionAmountToBytes :: TransactionAmount -> LBS.ByteString
transactionAmountToBytes TransactionAmount = "transaction-amount"

transactionAmountToText :: TransactionAmount -> T.Text
transactionAmountToText TransactionAmount = "transaction-amount"

-- Miscellaneous
--
data Birthday = Birthday
  deriving (Eq, Show)

birthdayToBytes :: Birthday -> LBS.ByteString
birthdayToBytes Birthday = "bday"

birthdayToText :: Birthday -> T.Text
birthdayToText Birthday = "bday"

data BirthdayDay = BirthdayDay
  deriving (Eq, Show)

birthdayDayToBytes :: BirthdayDay -> LBS.ByteString
birthdayDayToBytes BirthdayDay = "bday-day"

birthdayDayToText :: BirthdayDay -> T.Text
birthdayDayToText BirthdayDay = "bday-day"

data BirthdayMonth = BirthdayMonth
  deriving (Eq, Show)

birthdayMonthToBytes :: BirthdayMonth -> LBS.ByteString
birthdayMonthToBytes BirthdayMonth = "bday-month"

birthdayMonthToText :: BirthdayMonth -> T.Text
birthdayMonthToText BirthdayMonth = "bday-month"

data BirthdayYear = BirthdayYear
  deriving (Eq, Show)

birthdayYearToBytes :: BirthdayYear -> LBS.ByteString
birthdayYearToBytes BirthdayYear = "bday-year"

birthdayYearToText :: BirthdayYear -> T.Text
birthdayYearToText BirthdayYear = "bday-year"

data Language = Language
  deriving (Eq, Show)

languageToBytes :: Language -> LBS.ByteString
languageToBytes Language = "language"

languageToText :: Language -> T.Text
languageToText Language = "language"

data Sex = Sex
  deriving (Eq, Show)

sexToBytes :: Sex -> LBS.ByteString
sexToBytes Sex = "sex"

sexToText :: Sex -> T.Text
sexToText Sex = "sex"

data Url = Url
  deriving (Eq, Show)

urlToBytes :: Url -> LBS.ByteString
urlToBytes Url = "url"

urlToText :: Url -> T.Text
urlToText Url = "url"

data Photo = Photo
  deriving (Eq, Show)

photoToBytes :: Photo -> LBS.ByteString
photoToBytes Photo = "photo"

photoToText :: Photo -> T.Text
photoToText Photo = "photo"
