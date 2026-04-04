{-# LANGUAGE DerivingStrategies #-}

module Domain.Order
  ( OrderRequest (..)
  , OrderResponse (..)
  , OrderUpdate (..)
  , CancelRequest (..)
  , OrderFill (..)
  , OrderType (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Types
  ( InstrumentId (..)
  , OrderId (..)
  , OrderStatus (..)
  , PositionId (..)
  , Price
  , Quantity
  , Side (..)
  )
import GHC.Generics (Generic)

-- ============================================================================
-- Order Request (from frontend to open a position)
-- ============================================================================

data OrderRequest = OrderRequest
  { orderRequestPositionId :: PositionId
  , orderRequestInstrumentId :: InstrumentId
  , orderRequestSide :: Side
  , orderRequestQuantity :: Quantity
  , orderRequestPrice :: Maybe Price
  , orderRequestOrderType :: OrderType
  , orderRequestTPPrice :: Maybe Price
  , orderRequestSLPrice :: Maybe Price
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OrderType
  = MarketOrder
  | LimitOrder
  | PostOnly
  | FillOrKill
  | ImmediateOrCancel
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Order Response (from OKX after order submission)
-- ============================================================================

data OrderResponse = OrderResponse
  { orderResponseOrderId :: OrderId
  , orderResponseClientOrderId :: Maybe Text
  , orderResponseStatus :: OrderStatus
  , orderResponseFilledQty :: Quantity
  , orderResponseAvgPrice :: Maybe Price
  , orderResponseTimestamp :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Order Update (from OKX WebSocket)
-- ============================================================================

data OrderUpdate = OrderUpdate
  { orderUpdateOrderId :: OrderId
  , orderUpdateStatus :: OrderStatus
  , orderUpdateFilledQty :: Quantity
  , orderUpdateRemainingQty :: Quantity
  , orderUpdateAvgPrice :: Maybe Price
  , orderUpdateTimestamp :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Order Fill Details
-- ============================================================================

data OrderFill = OrderFill
  { fillOrderId :: OrderId
  , fillTradeId :: Text
  , fillPrice :: Price
  , fillQuantity :: Quantity
  , fillFee :: Scientific
  , fillFeeCurrency :: Text
  , fillTimestamp :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Cancel Request
-- ============================================================================

data CancelRequest = CancelRequest
  { cancelRequestOrderId :: OrderId
  , cancelRequestPositionId :: PositionId
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
