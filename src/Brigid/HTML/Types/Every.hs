module Brigid.HTML.Types.Every
  ( Every
  , every
  , everyToBytes
  , everyToText
  ) where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Integer (Positive)

import Brigid.HTML.Types.TimingDeclaration qualified as TD
import Brigid.HTML.Types.TriggerFilter qualified as TF

data Every =
  Every
    { everyTiming :: TD.TimingDeclaration
    , everyFilter :: Maybe TF.TriggerFilter
    } deriving (Eq, Show)

every :: Positive -> TD.TimingUnits -> Maybe TF.TriggerFilter -> Every
every n tu = Every (TD.TimingDeclaration n tu)

everyToBytes :: Every -> LBS.ByteString
everyToBytes e =
  LBS.intercalate (LBS8.pack " ")
    . catMaybes
    $ [ Just "every"
      , Just . TD.timingDeclarationToBytes $ everyTiming e
      , (\tf -> "[" <> TF.triggerFilterToBytes tf <> "]") <$> everyFilter e
      ]

everyToText :: Every -> T.Text
everyToText e =
  T.unwords
    . catMaybes
    $ [ Just "every"
      , Just . TD.timingDeclarationToText $ everyTiming e
      , (\tf -> "[" <> TF.triggerFilterToText tf <> "]") <$> everyFilter e
      ]
