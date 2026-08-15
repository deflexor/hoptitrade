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
  ( InstrumentId (..)
  , OrderId (..)
  , PositionId (..)
  , PositionStatus (..)
  , Price
  , Quantity
  , Side (..)
  , StrategyId (..)
  )
import GHC.Generics (Generic)

data PositionLeg = PositionLeg
  { posLegOrderId :: OrderId
  , posLegInstrumentId :: InstrumentId
  , posLegSide :: Side
  , posLegQuantity :: Quantity
  , posLegFilledPrice :: Price
  , posLegFilledAt :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Position = Position
  { positionId :: PositionId
  , positionStrategyId :: StrategyId
  , positionStatus :: PositionStatus
  , positionLegs :: [PositionLeg]
  , positionGreeks :: Maybe Greeks
  , positionRealizedPL :: Maybe Scientific
  , positionUnrealizedPL :: Maybe Scientific
  , positionMarginUsed :: Scientific
  , positionMaxProfit :: Maybe Scientific
  , positionMaxLoss :: Maybe Scientific
  , positionEntryPremium :: Maybe Scientific
  , positionEntryPop :: Maybe Scientific
  , positionOpenedAt :: Maybe UTCTime
  , positionClosedAt :: Maybe UTCTime
  , positionNotes :: Maybe Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PositionUpdate = PositionUpdate
  { posUpdatePositionId :: PositionId
  , posUpdateStatus :: PositionStatus
  , posUpdateFilledQty :: Quantity
  , posUpdateAvgPrice :: Price
  , posUpdateTimestamp :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Mark-to-market P/L from current mid prices per leg.
-- Buy leg: (mark - fill) * qty; Sell leg: (fill - mark) * qty.
calculateUnrealizedPL :: Position -> [(InstrumentId, Price)] -> Maybe Scientific
calculateUnrealizedPL position marks =
  let markMap = marks
      legPL PositionLeg{..} =
        lookup posLegInstrumentId markMap >>= \mark ->
          let diff = case posLegSide of
                Buy  -> mark - posLegFilledPrice
                Sell -> posLegFilledPrice - mark
          in Just (diff * posLegQuantity)
  in fmap sum $ traverse legPL (positionLegs position)

isPositionActive :: PositionStatus -> Bool
isPositionActive PositionOpening = True
isPositionActive PositionActive = True
isPositionActive (PositionPartial _) = True
isPositionActive _ = False

isPositionClosing :: PositionStatus -> Bool
isPositionClosing PositionClosing = True
isPositionClosing _ = False
