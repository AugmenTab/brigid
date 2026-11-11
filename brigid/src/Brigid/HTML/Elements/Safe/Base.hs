{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Elements.Safe.Base
  ( baseWithHref
  , baseWithTarget
  , baseWithHrefAndTarget
  ) where

import GHC.TypeLits (KnownNat)
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Attributes.Href (ValidHref)
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Types qualified as Types

baseWithHref :: ( KnownNat branchIndex
                , branchIndex ~ FirstIndexOf href Types.HrefTypes
                , ValidHref href E.Base
                , ValidChild E.Base parent grandparent
                )
             => href -> [A.Attribute E.Base] -> E.ChildHTML parent grandparent
baseWithHref href attrs =
  E.base $ A.href href : attrs

baseWithTarget :: ValidChild E.Base parent grandparent
               => Types.Target
               -> [A.Attribute E.Base]
               -> E.ChildHTML parent grandparent
baseWithTarget target attrs =
  E.base $ A.target target : attrs

baseWithHrefAndTarget :: ( KnownNat branchIndex
                         , branchIndex ~ FirstIndexOf href Types.HrefTypes
                         , ValidHref href E.Base
                         , ValidChild E.Base parent grandparent
                         )
                      => href
                      -> Types.Target
                      -> [A.Attribute E.Base]
                      -> E.ChildHTML parent grandparent
baseWithHrefAndTarget href target attrs =
  E.base $ A.href href : A.target target : attrs

