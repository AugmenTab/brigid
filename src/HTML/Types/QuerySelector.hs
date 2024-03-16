{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HTML.Types.QuerySelector
  ( QuerySelector
  , QuerySelectorTypes
  , mkQuerySelector
  , unQuerySelector
  , querySelectorToBytes
  , querySelectorToText
  , RawQuerySelector (RawQuerySelector)
  , rawQuerySelectorToBytes
  , rawQuerySelectorToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import HTML.Types.Class (Class, classToBytes, classToText)
import HTML.Types.ElementSelector qualified as ES
import HTML.Types.Id (Id, idToBytes, idToText)

newtype QuerySelector =
  QuerySelector
    { unQuerySelector :: Shrubbery.Union QuerySelectorTypes
    }

type QuerySelectorTypes =
  [ Id
  , Class
  , ES.ElementSelector
  , RawQuerySelector
  ]

mkQuerySelector :: ( KnownNat branchIndex
                   , branchIndex ~ FirstIndexOf querySelector QuerySelectorTypes
                   )
                => querySelector -> QuerySelector
mkQuerySelector =
  QuerySelector . Shrubbery.unify

querySelectorToBytes :: QuerySelector -> LBS.ByteString
querySelectorToBytes =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Id idToBytes
      . Shrubbery.branch @Class classToBytes
      . Shrubbery.branch @ES.ElementSelector ES.elementSelectorToBytes
      . Shrubbery.branch @RawQuerySelector rawQuerySelectorToBytes
      $ Shrubbery.branchEnd
  ) . unQuerySelector

querySelectorToText :: QuerySelector -> T.Text
querySelectorToText =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @Id idToText
      . Shrubbery.branch @Class classToText
      . Shrubbery.branch @ES.ElementSelector ES.elementSelectorToText
      . Shrubbery.branch @RawQuerySelector rawQuerySelectorToText
      $ Shrubbery.branchEnd
  ) . unQuerySelector

newtype RawQuerySelector =
  RawQuerySelector
    { rawQuerySelectorToText :: T.Text
    }

rawQuerySelectorToBytes :: RawQuerySelector -> LBS.ByteString
rawQuerySelectorToBytes =
  LBS.fromStrict . TE.encodeUtf8 . rawQuerySelectorToText
