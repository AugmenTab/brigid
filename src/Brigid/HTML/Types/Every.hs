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
import Numeric.Natural (Natural)

import Brigid.HTML.Types.TriggerFilter qualified as TF

data Every =
  Every
    { everySeconds :: Natural
    , everyFilter  :: Maybe TF.TriggerFilter
    } deriving (Eq, Show)

every :: Natural -> Maybe TF.TriggerFilter -> Every
every = Every

everyToBytes :: Every -> LBS.ByteString
everyToBytes e =
  LBS.intercalate (LBS8.pack " ")
    . catMaybes
    $ [ Just "every"
      , Just
          . LBS.concat
          $ [ LBS8.pack . show $ everySeconds e
            , "s"
            ]
      , (\tf -> "[" <> TF.triggerFilterToBytes tf <> "]") <$> everyFilter e
      ]

everyToText :: Every -> T.Text
everyToText e =
  T.unwords
    . catMaybes
    $ [ Just "every"
      , Just
          . T.concat
          $ [ T.pack . show $ everySeconds e
            , "s"
            ]
      , (\tf -> "[" <> TF.triggerFilterToText tf <> "]") <$> everyFilter e
      ]
