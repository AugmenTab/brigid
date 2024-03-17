module HTML.HTMX.Config
  ( Config (..)
  , configSchema
  , defaultConfig
  ) where

import Data.List.NonEmpty qualified as NEL
import Fleece.Core ((#+))
import Fleece.Core qualified as FC
import Numeric.Natural (Natural)

import HTML.Types.Class (Class(Class))
import HTML.Types.Method (Method, methodFromText, methodToText)
import HTML.Types.QuerySelector qualified as QS

data Config =
  Config
    { historyEnabled          :: Maybe Bool
    , historyCacheSize        :: Maybe Natural
    , refreshOnHistoryMiss    :: Maybe Bool
 -- , defaultSwapStyle        :: Maybe _
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
 -- , wsBinaryType            :: Maybe _
    , disableSelector         :: Maybe (NEL.NonEmpty QS.AttributeType)
    , withCredentials         :: Maybe Bool
    , timeout                 :: Maybe Natural
 -- , scrollBehavior          :: Maybe _
 -- , defaultFocusScroll      :: Maybe _
    , getCacheBusterParam     :: Maybe Bool
    , globalViewTransitions   :: Maybe Bool
    , methodsThatUseUrlParams :: Maybe (NEL.NonEmpty Method)
    , selfRequestsOnly        :: Maybe Bool
    , ignoreTitle             :: Maybe Bool
    , scrollIntoViewOnBoost   :: Maybe Bool
 -- , triggerSpecsCache       :: Maybe _
    }

configSchema :: FC.Fleece schema => schema Config
configSchema =
  FC.object $
    FC.constructor Config
      #+ FC.optional "historyEnabled"          historyEnabled          FC.boolean
      #+ FC.optional "historyCacheSize"        historyCacheSize        naturalSchema
      #+ FC.optional "refreshOnHistoryMiss"    refreshOnHistoryMiss    FC.boolean
 --   #+ FC.optional "defaultSwapStyle"        defaultSwapStyle        :: Maybe _
      #+ FC.optional "defaultSwapDelay"        defaultSwapDelay        naturalSchema
      #+ FC.optional "defaultSettleDelay"      defaultSettleDelay      naturalSchema
      #+ FC.optional "includeIndicatorStyles"  includeIndicatorStyles  FC.boolean
      #+ FC.optional "indicatorClass"          indicatorClass          (FC.coerceSchema FC.text)
      #+ FC.optional "requestClass"            requestClass            (FC.coerceSchema FC.text)
      #+ FC.optional "addedClass"              addedClass              (FC.coerceSchema FC.text)
      #+ FC.optional "settlingClass"           settlingClass           (FC.coerceSchema FC.text)
      #+ FC.optional "swappingClass"           swappingClass           (FC.coerceSchema FC.text)
      #+ FC.optional "allowEval"               allowEval               FC.boolean
      #+ FC.optional "allowScriptTypes"        allowScriptTypes        FC.boolean
 --   #+ FC.optional "inlineScriptNonce"       inlineScriptNonce       :: Maybe _
      #+ FC.optional "attributesToSettle"      attributesToSettle      (FC.nonEmpty attributeTypeSchema)
      #+ FC.optional "useTemplateFragments"    useTemplateFragments    FC.boolean
 --   #+ FC.optional "wsReconnectDelay"        wsReconnectDelay        :: Maybe _
 --   #+ FC.optional "wsBinaryType"            wsBinaryType            :: Maybe _
      #+ FC.optional "disableSelector"         disableSelector         (FC.nonEmpty attributeTypeSchema)
      #+ FC.optional "withCredentials"         withCredentials         FC.boolean
      #+ FC.optional "timeout"                 timeout                 naturalSchema
 --   #+ FC.optional "scrollBehavior"          scrollBehavior          :: Maybe _
 --   #+ FC.optional "defaultFocusScroll"      defaultFocusScroll      :: Maybe _
      #+ FC.optional "getCacheBusterParam"     getCacheBusterParam     FC.boolean
      #+ FC.optional "globalViewTransitions"   globalViewTransitions   FC.boolean
      #+ FC.optional "methodsThatUseUrlParams" methodsThatUseUrlParams (FC.nonEmpty methodSchema)
      #+ FC.optional "selfRequestsOnly"        selfRequestsOnly        FC.boolean
      #+ FC.optional "ignoreTitle"             ignoreTitle             FC.boolean
      #+ FC.optional "scrollIntoViewOnBoost"   scrollIntoViewOnBoost   FC.boolean
 --   #+ FC.optional "triggerSpecsCache"       triggerSpecsCache       :: Maybe _

defaultConfig :: Config
defaultConfig =
  Config
    { historyEnabled          = Nothing
    , historyCacheSize        = Nothing
    , refreshOnHistoryMiss    = Nothing
 -- , defaultSwapStyle        = Nothing
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
 -- , wsBinaryType            = Nothing
    , disableSelector         = Nothing
    , withCredentials         = Nothing
    , timeout                 = Nothing
 -- , scrollBehavior          = Nothing
 -- , defaultFocusScroll      = Nothing
    , getCacheBusterParam     = Nothing
    , globalViewTransitions   = Nothing
    , methodsThatUseUrlParams = Nothing
    , selfRequestsOnly        = Nothing
    , ignoreTitle             = Nothing
    , scrollIntoViewOnBoost   = Nothing
 -- , triggerSpecsCache       = Nothing
    }

attributeTypeSchema :: FC.Fleece schema => schema QS.AttributeType
attributeTypeSchema =
  FC.transform QS.attributeTypeToText QS.attributeTypeFromText FC.text

methodSchema :: FC.Fleece schema => schema Method
methodSchema =
  FC.validate methodToText methodFromText FC.text

naturalSchema :: FC.Fleece schema => schema Natural
naturalSchema =
  let check int
        | int < 0   = Left $ "Invalid Natural " <> show int
        | otherwise = Right $ fromIntegral int
   in FC.validate fromIntegral check FC.integer

-- TODO
-- setConfig :: Config -> ChildHTML parent grandparent
-- setConfig config =
--   meta [ name "htmx-config"
--        , content . T.pack . LBS8.unpack $ FA.encode configSchema config
--        ]
