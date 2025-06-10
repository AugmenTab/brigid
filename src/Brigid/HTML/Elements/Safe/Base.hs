{-# LANGUAGE TypeOperators #-}

module Brigid.HTML.Elements.Safe.Base
  ( base
  ) where

import Data.These (These (..))
import GHC.TypeLits (KnownNat)
import Shrubbery.TypeList (FirstIndexOf)

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Attributes.Href (ValidHref)
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Types qualified as Types
import Brigid.Types qualified as Types

base :: ( KnownNat branchIndex
        , branchIndex ~ FirstIndexOf href (Types.HrefTypes Types.Get)
        , ValidHref href Tags.Base
        , ValidChild Tags.Base parent grandparent
        )
     => These href Types.Target -> E.ChildHTML parent grandparent
base attrs =
  E.base $
    case attrs of
      This href -> [ A.href href ]
      That target -> [ A.target target ]
      These href target -> [ A.href href, A.target target ]
