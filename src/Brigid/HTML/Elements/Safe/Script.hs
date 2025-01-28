{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Elements.Safe.Script
  ( script
  , Script
  , InlineScript (..)
  , defaultInlineScript
  , ExternalScript (..)
  , defaultExternalScript
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
import Brigid.HTML.Attributes.Source (ValidSource)
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types

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
  , ExternalScript Types.AbsoluteURL
  , ExternalScript (Types.RelativeURL Types.Get)
  , ExternalScript Types.RawURL
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
      . Shrubbery.branch @InlineScript inlineScriptAttributes
      . Shrubbery.branch @(ExternalScript Types.AbsoluteURL) externalScriptAttributes
      . Shrubbery.branch @(ExternalScript (Types.RelativeURL Types.Get)) externalScriptAttributes
      . Shrubbery.branch @(ExternalScript Types.RawURL) externalScriptAttributes
      $ Shrubbery.branchEnd
  ) union

scriptContent :: Script -> Maybe NET.NonEmptyText
scriptContent (Script union) =
  ( Shrubbery.dissect
      . Shrubbery.branchBuild
      . Shrubbery.branch @InlineScript   (Just . inlineScriptContent)
      . Shrubbery.branch @(ExternalScript Types.AbsoluteURL) (const Nothing)
      . Shrubbery.branch @(ExternalScript (Types.RelativeURL Types.Get)) (const Nothing)
      . Shrubbery.branch @(ExternalScript Types.RawURL) (const Nothing)
      $ Shrubbery.branchEnd
  ) union

data InlineScript =
  InlineScript
    { inlineScriptContent        :: NET.NonEmptyText
    , inlineScriptReferrerPolicy :: Maybe Types.ReferrerPolicy
 -- , inlineScriptNonce          :: Maybe _
 -- , inlineScriptType           :: Maybe _
    }

defaultInlineScript :: NET.NonEmptyText -> InlineScript
defaultInlineScript content =
  InlineScript
    { inlineScriptContent        = content
    , inlineScriptReferrerPolicy = Nothing
 -- , inlineScriptNonce          = Nothing
 -- , inlineScriptType           = Nothing
    }

inlineScriptAttributes :: InlineScript -> [Attribute Tags.Script]
inlineScriptAttributes inline =
  catMaybes
    [ A.referrerpolicy <$> inlineScriptReferrerPolicy inline
 -- , A.nonce          <$> inlineScriptNonce          inline
 -- , A.type_          <$> inlineScriptType           inline
    ]

data ExternalScript url =
  ExternalScript
    { externalScriptCrossorigin     :: Maybe Types.CrossOriginFetch
 -- , externalScriptIntegrity       :: Maybe _
    , externalScriptLoadingBehavior :: LoadingBehavior
    , externalScriptNoModule        :: Bool
 -- , externalScriptNonce           :: Maybe _
    , externalScriptReferrerPolicy  :: Maybe Types.ReferrerPolicy
    , externalScriptSource          :: url
 -- , externalScriptType            :: Maybe _
    }

defaultExternalScript :: url -> ExternalScript url
defaultExternalScript url =
  ExternalScript
    { externalScriptCrossorigin     = Nothing
 -- , externalScriptIntegrity       = Nothing
    , externalScriptLoadingBehavior = Block
    , externalScriptNoModule        = False
 -- , externalScriptNonce           = Nothing
    , externalScriptReferrerPolicy  = Nothing
    , externalScriptSource          = url
 -- , externalScriptType            = Nothing
    }

externalScriptAttributes  :: ( KnownNat branchIndex
                             , branchIndex ~ FirstIndexOf url Types.URLTypes
                             , ValidSource url Tags.Script
                             )
                          => ExternalScript url -> [Attribute Tags.Script]
externalScriptAttributes external =
  catMaybes
    [ Just . A.src            $ externalScriptSource          external
    , Just . A.nomodule       $ externalScriptNoModule        external
 -- , A.type_               <$> externalScriptType            external
    , loadingBehaviorAttribute (externalScriptLoadingBehavior external)
    , A.crossorigin         <$> externalScriptCrossorigin     external
 -- , A.integrity           <$> externalScriptIntegrity       external
 -- , A.nonce               <$> externalScriptNonce           external
    , A.referrerpolicy      <$> externalScriptReferrerPolicy  external
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
