{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Strategies
  ( StrategiesAPI
  , strategiesServer
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON, defaultOptions, Options (..))
import Data.Char (toLower)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime, UTCTime)
import Domain.Greeks (Greeks (..))
import Domain.Strategy
  ( AIAdvice (..)
  , SpreadType (..)
  , Strategy (..)
  , StrategyMetrics (..)
  , StrategyStatus (..)
  , StrategyType (..)
  )
import Domain.Types (StrategyId (..), TradingMode (..), InstrumentId (..), Side (..), OptionType (..))
import GHC.Generics (Generic)
import Polysemy
import Polysemy.Embed
import Servant

-- ============================================================================
-- API Type
-- ============================================================================

type StrategiesAPI =
  "strategies" :> QueryParam "mode" TradingMode :> Get '[JSON] [StrategyResponse]
  :<|> "strategies" :> Capture "strategyId" StrategyId :> Get '[JSON] (Maybe StrategyResponse)

-- ============================================================================
-- Response Types
-- ============================================================================

data StrategyResponse = StrategyResponse
  { respStrategyId :: StrategyId
  , respStrategyType :: StrategyType
  , respStrategyName :: Text
  , respStrategyDescription :: Text
  , respStrategyUnderlying :: Text
  , respStrategyGreeks :: Greeks
  , respStrategyMetrics :: StrategyMetrics
  , respStrategyNetPremium :: Scientific
  , respStrategyMarginRequired :: Scientific
  , respStrategyAdvice :: AIAdvice
  , respStrategyStatus :: StrategyStatus
  } deriving stock (Eq, Show, Generic)

instance ToJSON StrategyResponse where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON StrategyResponse where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }

-- ============================================================================
-- Mock Strategy Generation
-- ============================================================================

generateMockStrategies :: UTCTime -> [StrategyResponse]
generateMockStrategies now =
  [ mockIronCondor now 1
  , mockBullCallSpread now 2
  , mockBearPutSpread now 3
  , mockStraddle now 4
  ]

mockIronCondor :: UTCTime -> Int -> StrategyResponse
mockIronCondor now idx = StrategyResponse
  { respStrategyId = StrategyId $ read $ "550e8400-e29b-41d4-a716-44665544000" ++ show idx
  , respStrategyType = IronCondor
  , respStrategyName = "BTC Iron Condor Mar 15"
  , respStrategyDescription = "Sell 65k call, buy 70k call, sell 55k put, buy 50k put"
  , respStrategyUnderlying = "BTC-USD"
  , respStrategyGreeks = Greeks
    { greeksDelta = 0.05
    , greeksGamma = -0.001
    , greeksTheta = 2.5
    , greeksVega = -0.5
    , greeksRho = 0.01
    }
  , respStrategyMetrics = StrategyMetrics
    { metricsMaxProfit = Just 450.0
    , metricsMaxLoss = Just 550.0
    , metricsBreakEvenPoints = [54500.0, 65500.0]
    , metricsProbabilityOfProfit = Just 0.65
    , metricsExpectedReturn = Just 45.0
    , metricsSuggestedTP = Just 400.0
    , metricsSuggestedSL = Just 500.0
    }
  , respStrategyNetPremium = 450.0
  , respStrategyMarginRequired = 5000.0
  , respStrategyAdvice = AIAdvice True "High probability of profit with defined risk" 0.85
  , respStrategyStatus = StrategyActive
  }

mockBullCallSpread :: UTCTime -> Int -> StrategyResponse
mockBullCallSpread now idx = StrategyResponse
  { respStrategyId = StrategyId $ read $ "550e8400-e29b-41d4-a716-44665544000" ++ show idx
  , respStrategyType = VerticalSpread DebitSpread
  , respStrategyName = "ETH Bull Call Spread Mar 20"
  , respStrategyDescription = "Buy 3.2k call, sell 3.5k call"
  , respStrategyUnderlying = "ETH-USD"
  , respStrategyGreeks = Greeks
    { greeksDelta = 0.45
    , greeksGamma = 0.02
    , greeksTheta = -0.8
    , greeksVega = 1.2
    , greeksRho = 0.05
    }
  , respStrategyMetrics = StrategyMetrics
    { metricsMaxProfit = Just 280.0
    , metricsMaxLoss = Just 220.0
    , metricsBreakEvenPoints = [3220.0]
    , metricsProbabilityOfProfit = Just 0.58
    , metricsExpectedReturn = Just 28.0
    , metricsSuggestedTP = Just 250.0
    , metricsSuggestedSL = Just 200.0
    }
  , respStrategyNetPremium = -220.0
  , respStrategyMarginRequired = 500.0
  , respStrategyAdvice = AIAdvice True "Moderate risk with good profit potential" 0.65
  , respStrategyStatus = StrategyActive
  }

mockBearPutSpread :: UTCTime -> Int -> StrategyResponse
mockBearPutSpread now idx = StrategyResponse
  { respStrategyId = StrategyId $ read $ "550e8400-e29b-41d4-a716-44665544000" ++ show idx
  , respStrategyType = VerticalSpread CreditSpread
  , respStrategyName = "SOL Bear Put Spread Mar 18"
  , respStrategyDescription = "Buy 140 put, sell 120 put"
  , respStrategyUnderlying = "SOL-USD"
  , respStrategyGreeks = Greeks
    { greeksDelta = -0.38
    , greeksGamma = 0.015
    , greeksTheta = -0.6
    , greeksVega = 0.9
    , greeksRho = -0.03
    }
  , respStrategyMetrics = StrategyMetrics
    { metricsMaxProfit = Just 1800.0
    , metricsMaxLoss = Just 200.0
    , metricsBreakEvenPoints = [1380.0]
    , metricsProbabilityOfProfit = Just 0.42
    , metricsExpectedReturn = Just 180.0
    , metricsSuggestedTP = Just 1600.0
    , metricsSuggestedSL = Just 180.0
    }
  , respStrategyNetPremium = -200.0
  , respStrategyMarginRequired = 2000.0
  , respStrategyAdvice = AIAdvice False "High risk/reward ratio requires careful monitoring" 0.45
  , respStrategyStatus = StrategyActive
  }

mockStraddle :: UTCTime -> Int -> StrategyResponse
mockStraddle now idx = StrategyResponse
  { respStrategyId = StrategyId $ read $ "550e8400-e29b-41d4-a716-44665544000" ++ show idx
  , respStrategyType = Straddle
  , respStrategyName = "BTC Long Straddle (Earnings Play)"
  , respStrategyDescription = "Buy ATM call and put for volatility expansion"
  , respStrategyUnderlying = "BTC-USD"
  , respStrategyGreeks = Greeks
    { greeksDelta = 0.02
    , greeksGamma = 0.08
    , greeksTheta = -5.2
    , greeksVega = 4.5
    , greeksRho = 0.0
    }
  , respStrategyMetrics = StrategyMetrics
    { metricsMaxProfit = Nothing
    , metricsMaxLoss = Just 1200.0
    , metricsBreakEvenPoints = [58800.0, 61200.0]
    , metricsProbabilityOfProfit = Just 0.35
    , metricsExpectedReturn = Nothing
    , metricsSuggestedTP = Just 5000.0
    , metricsSuggestedSL = Just 1000.0
    }
  , respStrategyNetPremium = -1200.0
  , respStrategyMarginRequired = 1200.0
  , respStrategyAdvice = AIAdvice False "High risk play dependent on volatility expansion" 0.35
  , respStrategyStatus = StrategyActive
  }

-- ============================================================================
-- Server
-- ============================================================================

strategiesServer :: Members '[Embed IO] r => ServerT StrategiesAPI (Sem r)
strategiesServer = listStrategies :<|> getStrategy
  where
    listStrategies _mMode = do
      now <- embed @IO getCurrentTime
      pure $ generateMockStrategies now

    getStrategy strategyId = do
      now <- embed @IO getCurrentTime
      let strategies = generateMockStrategies now
      pure $ case filter (\s -> respStrategyId s == strategyId) strategies of
        (s:_) -> Just s
        [] -> Nothing
