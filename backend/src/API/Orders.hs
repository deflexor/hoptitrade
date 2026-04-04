{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Orders
  ( OrdersAPI
  , ordersServer
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Domain.Order (CancelRequest (..), OrderRequest (..), OrderResponse (..), OrderType (..))
import Domain.Types (InstrumentId (..), OrderId (..), PositionId (..), Quantity, Side (..))
import Effects.OKX
import Effects.Position
import GHC.Generics (Generic)
import Polysemy
import Servant

-- ============================================================================
-- API Type
-- ============================================================================

type OrdersAPI =
  "orders" :> "open" :> ReqBody '[JSON] OpenOrderRequest :> Post '[JSON] OpenOrderResponse
  :<|> "orders" :> "cancel" :> ReqBody '[JSON] CancelOrderRequest :> Post '[JSON] CancelOrderResponse

-- ============================================================================
-- Request/Response Types
-- ============================================================================

data OpenOrderRequest = OpenOrderRequest
  { openPositionId :: PositionId
  , openInstrumentId :: InstrumentId
  , openSide :: Side
  , openQuantity :: Quantity
  , openPrice :: Maybe Scientific
  , openOrderType :: OrderType
  , openTPPrice :: Maybe Scientific
  , openSLPrice :: Maybe Scientific
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OpenOrderResponse = OpenOrderResponse
  { openSuccess :: Bool
  , openOrderId :: Maybe OrderId
  , openStatus :: Text
  , openError :: Maybe Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CancelOrderRequest = CancelOrderRequest
  { cancelOrderId :: OrderId
  , cancelPositionId :: PositionId
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CancelOrderResponse = CancelOrderResponse
  { cancelSuccess :: Bool
  , cancelError :: Maybe Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Server
-- ============================================================================

ordersServer :: Members '[OKXEffect, PositionEffect, Embed IO] r
             => ServerT OrdersAPI (Sem r)
ordersServer = openOrderHandler :<|> cancelOrderHandler
  where
    openOrderHandler req = do
      let orderReq = OrderRequest
            { orderRequestPositionId = openPositionId req
            , orderRequestInstrumentId = openInstrumentId req
            , orderRequestSide = openSide req
            , orderRequestQuantity = openQuantity req
            , orderRequestPrice = openPrice req
            , orderRequestOrderType = openOrderType req
            , orderRequestTPPrice = openTPPrice req
            , orderRequestSLPrice = openSLPrice req
            }

      -- TODO: Submit order to OKX
      response <- placeOrder orderReq

      pure $ OpenOrderResponse
        { openSuccess = True
        , openOrderId = Just $ orderResponseOrderId response
        , openStatus = "pending"
        , openError = Nothing
        }

    cancelOrderHandler req = do
      let cancelReq = CancelRequest
            { cancelRequestOrderId = cancelOrderId req
            , cancelRequestPositionId = cancelPositionId req
            }

      success <- cancelOrder cancelReq

      pure $ CancelOrderResponse
        { cancelSuccess = success
        , cancelError = if success then Nothing else Just "Failed to cancel order"
        }
