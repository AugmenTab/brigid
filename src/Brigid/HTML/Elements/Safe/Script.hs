{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Elements.Safe.Script
  ( script
  , Script
  , InlineScript (..)
  , ExternalScript (..)
  , LoadingBehavior
      ( Block
      , Async
      , Defer
      )
  ) where

import Data.Maybe (catMaybes)
import Data.NonEmptyText qualified as NET
import GHC.TypeLits (KnownNat)
import Shrubbery qualified
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Attributes.Internal (Attribute)
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Types (CrossOriginFetch, ReferrerPolicy)

script :: ( KnownNat branchIndex
          , branchIndex ~ FirstIndexOf script ScriptTypes
          , ValidChild Tags.Script parent grandparent
          )
       => script -> E.ChildHTML parent grandparent
script details =
  let union = mkScript details
   in E.script
        (scriptAttributes union)
        (scriptContent union)

newtype Script = Script (Shrubbery.Union ScriptTypes)

type ScriptTypes =
  [ InlineScript
  , ExternalScript
  ]

mkScript :: ( KnownNat branchIndex
            , branchIndex ~ FirstIndexOf script ScriptTypes
            )
         => script -> Script
mkScript =
  Script . Shrubbery.unify

scriptAttributes :: Script -> [Attribute Tags.Script]
scriptAttributes (Script union) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InlineScript   inlineScriptAttributes
      . Shrubbery.branch @ExternalScript externalScriptAttributes
      $ Shrubbery.branchEnd
  ) union

scriptContent :: Script -> Maybe NET.NonEmptyText
scriptContent (Script union) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InlineScript   (Just . inlineScriptContent)
      . Shrubbery.branch @ExternalScript (const Nothing)
      $ Shrubbery.branchEnd
  ) union

data InlineScript =
  InlineScript
    { inlineScriptContent        :: NET.NonEmptyText
    , inlineScriptReferrerPolicy :: Maybe ReferrerPolicy
 -- , inlineScriptNonce          :: Maybe _
 -- , inlineScriptType           :: Maybe _
    }

inlineScriptAttributes :: InlineScript -> [Attribute Tags.Script]
inlineScriptAttributes inline =
  catMaybes
    [ A.referrerpolicy <$> inlineScriptReferrerPolicy inline
 -- , A.nonce          <$> inlineScriptNonce          inline
 -- , A.type_          <$> inlineScriptType           inline
    ]

data ExternalScript =
  ExternalScript
    { externalScriptCrossorigin     :: Maybe CrossOriginFetch
 -- , externalScriptIntegrity       :: Maybe _
    , externalScriptLoadingBehavior :: LoadingBehavior
 -- , externalScriptNomodule        :: Maybe _
 -- , externalScriptNonce           :: Maybe _
 -- , externalScriptReferrerPolicy  :: Maybe _
 -- , externalScriptSource          :: _
 -- , externalScriptType            :: Maybe _
    }

externalScriptAttributes :: ExternalScript -> [Attribute Tags.Script]
externalScriptAttributes external =
  catMaybes
 -- [ A.src                 <$> externalScriptSource          external
 -- , A.type_               <$> externalScriptType            external
    [ loadingBehaviorAttribute (externalScriptLoadingBehavior external)
    , A.crossorigin         <$> externalScriptCrossorigin     external
 -- , A.integrity           <$> externalScriptIntegrity       external
 -- , A.nomodule            <$> externalScriptReferrerPolicy  external
 -- , A.nonce               <$> externalScriptNonce           external
 -- , A.referrerpolicy      <$> externalScriptNomodule        external
    ]

data LoadingBehavior
  = Block
  | Async
  | Defer

loadingBehaviorAttribute :: LoadingBehavior -> Maybe (Attribute Tags.Script)
loadingBehaviorAttribute behavior =
  case behavior of
    Block -> Nothing
    Async -> Just A.async
    Defer -> Just A.defer
