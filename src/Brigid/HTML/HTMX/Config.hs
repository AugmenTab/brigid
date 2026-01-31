module Brigid.HTML.HTMX.Config
  ( Config (..)
  , configToText
  , defaultConfig
  , setConfig
  ) where

import Data.Bool qualified as B
import Data.List.NonEmpty qualified as NEL
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TB
import Numeric.Natural (Natural)

import Brigid.HTML.Attributes qualified as A
import Brigid.HTML.Elements.Children (ValidChild)
import Brigid.HTML.Elements.Tags qualified as Tags
import Brigid.HTML.Elements qualified as E
import Brigid.HTML.Types.Class (Class, classToText)
import Brigid.HTML.Types.QuerySelector qualified as QS
import Brigid.HTML.Types.ScrollBehavior qualified as SB
import Brigid.HTML.Types.Swap (SwapStyle, swapStyleToText)
import Brigid.HTML.Types.WebsocketBinaryType qualified as WBT
import Brigid.Internal.Render qualified as Render
import Brigid.Types.Method (Method, methodToText)

data Config =
  Config
    { historyEnabled          :: Maybe Bool
    , historyCacheSize        :: Maybe Natural
    , refreshOnHistoryMiss    :: Maybe Bool
    , defaultSwapStyle        :: Maybe SwapStyle
    , defaultSwapDelay        :: Maybe Natural
    , defaultSettleDelay      :: Maybe Natural
    , includeIndicatorStyles  :: Maybe Bool
    , indicatorClass          :: Maybe Class
    , requestClass            :: Maybe Class
    , addedClass              :: Maybe Class
    , settlingClass           :: Maybe Class
    , swappingClass           :: Maybe Class
    , allowEval               :: Maybe Bool
    , allowScriptTypes        :: Maybe Bool
 -- , inlineScriptNonce       :: Maybe _
    , attributesToSettle      :: Maybe (NEL.NonEmpty QS.AttributeType)
    , useTemplateFragments    :: Maybe Bool
 -- , wsReconnectDelay        :: Maybe _
    , wsBinaryType            :: Maybe WBT.WebsocketBinaryType
    , disableSelector         :: Maybe (NEL.NonEmpty QS.AttributeType)
    , withCredentials         :: Maybe Bool
    , timeout                 :: Maybe Natural
    , scrollBehavior          :: Maybe SB.ScrollBehavior
 -- , defaultFocusScroll      :: Maybe _
    , getCacheBusterParam     :: Maybe Bool
    , globalViewTransitions   :: Maybe Bool
    , methodsThatUseUrlParams :: Maybe (NEL.NonEmpty Method)
    , selfRequestsOnly        :: Maybe Bool
    , ignoreTitle             :: Maybe Bool
    , scrollIntoViewOnBoost   :: Maybe Bool
 -- , triggerSpecsCache       :: Maybe _
    }

defaultConfig :: Config
defaultConfig =
  Config
    { historyEnabled          = Nothing
    , historyCacheSize        = Nothing
    , refreshOnHistoryMiss    = Nothing
    , defaultSwapStyle        = Nothing
    , defaultSwapDelay        = Nothing
    , defaultSettleDelay      = Nothing
    , includeIndicatorStyles  = Nothing
    , indicatorClass          = Nothing
    , requestClass            = Nothing
    , addedClass              = Nothing
    , settlingClass           = Nothing
    , swappingClass           = Nothing
    , allowEval               = Nothing
    , allowScriptTypes        = Nothing
 -- , inlineScriptNonce       = Nothing
    , attributesToSettle      = Nothing
    , useTemplateFragments    = Nothing
 -- , wsReconnectDelay        = Nothing
    , wsBinaryType            = Nothing
    , disableSelector         = Nothing
    , withCredentials         = Nothing
    , timeout                 = Nothing
    , scrollBehavior          = Nothing
 -- , defaultFocusScroll      = Nothing
    , getCacheBusterParam     = Nothing
    , globalViewTransitions   = Nothing
    , methodsThatUseUrlParams = Nothing
    , selfRequestsOnly        = Nothing
    , ignoreTitle             = Nothing
    , scrollIntoViewOnBoost   = Nothing
 -- , triggerSpecsCache       = Nothing
    }

setConfig :: ValidChild Tags.Meta parent grandparent
          => Config -> E.ChildHTML parent grandparent
setConfig config =
  E.meta
    [ A.customAttribute "name" "htmx-config"
    , A.content $ configToText config
    ]

configToText :: Config -> T.Text
configToText config =
  TB.runBuilder $ TB.fromText "{" <> makeFields config <> TB.fromText "}"

makeFields :: Config -> TB.Builder
makeFields config =
  let
    foldBuilders mbAcc fields =
      case (mbAcc, fields) of
        (Just acc, []) -> acc
        (Nothing, []) -> mempty
        (_mbAcc, Nothing:xs) -> foldBuilders mbAcc xs
        (Just acc, (Just x):xs) -> foldBuilders (Just $ acc <> comma <> x) xs
        (Nothing, (Just x):xs) -> foldBuilders (Just x) xs
  in
    foldBuilders Nothing
      [ bool "historyEnabled" <$> historyEnabled config
      , int "historyCacheSize" <$> historyCacheSize config
      , bool "refreshOnHistoryMiss" <$> refreshOnHistoryMiss config
      , text "defaultSwapStyle" swapStyleToText <$> defaultSwapStyle config
      , int "defaultSwapDelay" <$> defaultSwapDelay config
      , int "defaultSettleDelay" <$> defaultSettleDelay config
      , bool "includeIndicatorStyles" <$> includeIndicatorStyles config
      , text "indicatorClass" classToText <$> indicatorClass config
      , text "requestClass" classToText <$> requestClass config
      , text "addedClass" classToText <$> addedClass config
      , text "settlingClass" classToText <$> settlingClass config
      , text "swappingClass" classToText <$> swappingClass config
      , bool "allowEval" <$> allowEval config
      , bool "allowScriptTypes" <$> allowScriptTypes config
   -- , () <$> inlineScriptNonce config
      , list "attributesToSettle" QS.attributeTypeToText <$> attributesToSettle config
      , bool "useTemplateFragments" <$> useTemplateFragments config
   -- , () <$> wsReconnectDelay config
      , text "websocketBinaryType" WBT.websocketBinaryTypeToText <$> wsBinaryType config
      , list "disableSelector" QS.attributeTypeToText <$> disableSelector config
      , bool "withCredentials" <$> withCredentials config
      , int "timeout" <$> timeout config
      , text "scrollBehavior" SB.scrollBehaviorToText <$> scrollBehavior config
   -- , () <$> defaultFocusScroll config
      , bool "getCacheBusterParam" <$> getCacheBusterParam config
      , bool "globalViewTransitions" <$> globalViewTransitions config
      , list "methodsThatUseUrlParams" methodToText <$> methodsThatUseUrlParams config
      , bool "selfRequestsOnly" <$> selfRequestsOnly config
      , bool "ignoreTitle" <$> ignoreTitle config
      , bool "scrollIntoViewOnBoost" <$> scrollIntoViewOnBoost config
   -- , () <$> triggerSpecsCache config
      ]

quotationMark :: TB.Builder
quotationMark =
  TB.fromChar '"'

colon :: TB.Builder
colon =
  TB.fromChar ':'

comma :: TB.Builder
comma =
  TB.fromChar ','

quote :: T.Text -> TB.Builder
quote t =
  quotationMark <> TB.fromText t <> quotationMark

bool :: T.Text -> Bool -> TB.Builder
bool label x =
  mconcat
    [ quote label
    , colon
    , B.bool "false" "true" x
    ]

int :: Integral a => T.Text -> a -> TB.Builder
int label x =
  mconcat
    [ quote label
    , colon
    , TB.fromUnboundedDec x
    ]

list :: Foldable f => T.Text -> (a -> T.Text) -> f a -> TB.Builder
list label toText xs =
  mconcat
    [ quote label
    , colon
    , TB.fromChar '['
    , TB.fromText $ Render.foldToTextWithSeparator toText "," xs
    , TB.fromChar ']'
    ]

text :: T.Text -> (a -> T.Text) -> a -> TB.Builder
text label toText x =
  mconcat
    [ quote label
    , colon
    , TB.fromText $ toText x
    ]
