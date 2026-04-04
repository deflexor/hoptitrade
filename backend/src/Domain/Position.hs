{-# LANGUAGE DerivingStrategies #-}

module Domain.Position
  ( Position (..)
  , PositionLeg (..)
  , PositionUpdate (..)
  , calculateUnrealizedPL
  , isPositionActive
  , isPositionClosing
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Greeks (Greeks)
import Domain.Types
  ( OrderId (..)
  , PositionId (..)
  , PositionStatus (..)
  , Price
  , Quantity
  , Side (..)
  , StrategyId (..)
  )
import GHC.Generics (Generic)

-- ============================================================================
-- Position Leg (represents a filled order leg)
-- ============================================================================

data PositionLeg = PositionLeg
  { posLegOrderId :: OrderId
  , posLegSide :: Side
  , posLegQuantity :: Quantity
  , posLegFilledPrice :: Price
  , posLegFilledAt :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Position (aggregate of all legs for a strategy)
-- ============================================================================

data Position = Position
  { positionId :: PositionId
  , positionStrategyId :: StrategyId
  , positionStatus :: PositionStatus
  , positionLegs :: [PositionLeg]
  , positionGreeks :: Maybe Greeks
  , positionRealizedPL :: Maybe Scientific
  , positionUnrealizedPL :: Maybe Scientific
  , positionMarginUsed :: Scientific
  , positionOpenedAt :: Maybe UTCTime
  , positionClosedAt :: Maybe UTCTime
  , positionNotes :: Maybe Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Position Update (from OKX WebSocket)
-- ============================================================================

data PositionUpdate = PositionUpdate
  { posUpdatePositionId :: PositionId
  , posUpdateStatus :: PositionStatus
  , posUpdateFilledQty :: Quantity
  , posUpdateAvgPrice :: Price
  , posUpdateTimestamp :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Utility Functions
-- ============================================================================

calculateUnrealizedPL :: Position -> Price -> Maybe Scientific
calculateUnrealizedPL _position _currentPrice =
  -- TODO: Implement P/L calculation based on position type and current market price
  Nothing

isPositionActive :: PositionStatus -> Bool
isPositionActive PositionActive = True
isPositionActive (PositionPartial _) = True
isPositionActive _ = False

isPositionClosing :: PositionStatus -> Bool
isPositionClosing PositionClosing = True
isPositionClosing _ = False
