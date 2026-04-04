{-# LANGUAGE DerivingStrategies #-}

module Domain.Strategy
  ( Strategy (..)
  , StrategyType (..)
  , SpreadType (..)
  , AIAdvice (..)
  , StrategyStatus (..)
  , StrategyMetrics (..)
  , calculateMaxProfit
  , calculateMaxLoss
  , calculateBreakEvenPoints
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Greeks (Greeks)
import Domain.Option (OptionLeg (..))
import Domain.Types
  ( Currency (..)
  , InstrumentId (..)
  , Price
  , Quantity
  , Side (..)
  , StrategyId (..)
  )
import GHC.Generics (Generic)

-- ============================================================================
-- Strategy Types
-- ============================================================================

data StrategyType
  = CoveredCall
  | IronCondor
  | VerticalSpread SpreadType
  | NakedCall
  | NakedPut
  | Straddle
  | Strangle
  | CustomStrategy Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SpreadType
  = CreditSpread
  | DebitSpread
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data StrategyStatus
  = StrategyAvailable
  | StrategyOpening
  | StrategyActive
  | StrategyClosed
  | StrategyExpired
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- AI Advice
-- ============================================================================

data AIAdvice = AIAdvice
  { adviceShouldOpen :: Bool
  , adviceReasoning :: Text
  , adviceConfidence :: Scientific
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Strategy Metrics
-- ============================================================================

data StrategyMetrics = StrategyMetrics
  { metricsMaxProfit :: Maybe Scientific
  , metricsMaxLoss :: Maybe Scientific
  , metricsBreakEvenPoints :: [Scientific]
  , metricsProbabilityOfProfit :: Maybe Scientific
  , metricsExpectedReturn :: Maybe Scientific
  , metricsSuggestedTP :: Maybe Scientific
  , metricsSuggestedSL :: Maybe Scientific
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Strategy Definition
-- ============================================================================

data Strategy = Strategy
  { strategyId :: StrategyId
  , strategyType :: StrategyType
  , strategyName :: Text
  , strategyDescription :: Text
  , strategyUnderlying :: Text
  , strategyLegs :: [OptionLeg]
  , strategyGreeks :: Greeks
  , strategyMetrics :: StrategyMetrics
  , strategyNetPremium :: Scientific
  , strategyMarginRequired :: Scientific
  , strategyAdvice :: AIAdvice
  , strategyStatus :: StrategyStatus
  , strategyCreatedAt :: UTCTime
  , strategyExpiresAt :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Calculation Functions
-- ============================================================================

calculateMaxProfit :: StrategyType -> [OptionLeg] -> Scientific -> Maybe Scientific
calculateMaxProfit strategyType legs netPremium = case strategyType of
  CoveredCall -> Just netPremium
  IronCondor -> Just netPremium
  VerticalSpread CreditSpread -> Just netPremium
  VerticalSpread DebitSpread -> Nothing
  NakedCall -> Nothing
  NakedPut -> Just netPremium
  _ -> Nothing

calculateMaxLoss :: StrategyType -> [OptionLeg] -> Scientific -> Maybe Scientific
calculateMaxLoss strategyType legs netPremium = case strategyType of
  CoveredCall -> Nothing
  IronCondor -> Just (abs netPremium)
  VerticalSpread CreditSpread -> Just (abs netPremium)
  VerticalSpread DebitSpread -> Just (abs netPremium)
  NakedCall -> Nothing
  NakedPut -> Nothing
  _ -> Nothing

calculateBreakEvenPoints :: StrategyType -> [OptionLeg] -> Scientific -> [Scientific]
calculateBreakEvenPoints _strategyType _legs _netPremium =
  -- TODO: Implement actual break-even calculations based on strategy type
  []
