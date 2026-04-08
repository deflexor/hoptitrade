{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Orders
  ( OrdersAPI
  , ordersServer
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Domain.Broker (BrokerConfig (..), BrokerMode (..))
import Domain.Order (CancelRequest (..), OrderRequest (..), OrderResponse (..), OrderType (..))
import Domain.Settings (settingsToBrokerConfig)
import Domain.Types (InstrumentId (..), OrderId (..), PositionId (..), Quantity, Side (..), UserId (..))
import Effects.Broker
import Effects.Position
import Effects.Settings
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
  , openWarning :: Maybe Text  -- ^ e.g., "Using sandbox mode"
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

ordersServer :: Members '[BrokerEffect, PositionEffect, SettingsEffect, Embed IO] r
             => ServerT OrdersAPI (Sem r)
ordersServer = openOrderHandler :<|> cancelOrderHandler
  where
    -- Helper to get current user ID (placeholder until auth is implemented)
    getCurrentUserId :: UserId
    getCurrentUserId = UserId $ read "550e8400-e29b-41d4-a716-446655440000"

    -- Helper to get broker config from settings
    getBrokerConfig :: Member SettingsEffect r => Sem r (Maybe BrokerConfig)
    getBrokerConfig = do
      let uid = getCurrentUserId
      settings <- getSettings uid
      pure $ settingsToBrokerConfig settings

    openOrderHandler req = do
      let uid = getCurrentUserId
          instId = openInstrumentId req
      
      -- CHECK 1: Duplicate position prevention (per TODO.md!)
      hasDuplicate <- checkDuplicatePosition uid instId
      
      case hasDuplicate of
        True -> 
          pure $ OpenOrderResponse
            { openSuccess = False
            , openOrderId = Nothing
            , openStatus = "rejected"
            , openError = Just $ "Duplicate position: You already have an active position for " <> unInstrumentId instId
            , openWarning = Nothing
            }
        
        False -> do
          let orderReq = OrderRequest
                { orderRequestPositionId = openPositionId req
                , orderRequestInstrumentId = instId
                , orderRequestSide = openSide req
                , orderRequestQuantity = openQuantity req
                , orderRequestPrice = openPrice req
                , orderRequestOrderType = openOrderType req
                , orderRequestTPPrice = openTPPrice req
                , orderRequestSLPrice = openSLPrice req
                }

          -- Get broker configuration from user settings
          mBrokerConfig <- getBrokerConfig
          
          case mBrokerConfig of
            Nothing -> 
              -- No broker configured
              pure $ OpenOrderResponse
                { openSuccess = False
                , openOrderId = Nothing
                , openStatus = "error"
                , openError = Just "No broker configured. Please set up broker credentials in settings."
                , openWarning = Nothing
                }
            
            Just brokerConfig -> do
              -- Check if using sandbox mode for warning message
              let warningMsg = case brokerConfig of
                    TBankConfig _ _ Sandbox -> Just "Using T-Bank SANDBOX mode (virtual money)"
                    TBankConfig _ _ Real -> Just "WARNING: Using T-Bank REAL trading (real money)!"
                    OKXConfig _ _ _ demo -> if demo 
                      then Just "Using OKX demo mode"
                      else Nothing
              
              -- Submit order via configured broker
              response <- placeOrder brokerConfig orderReq

              pure $ OpenOrderResponse
                { openSuccess = True
                , openOrderId = Just $ orderResponseOrderId response
                , openStatus = "pending"
                , openError = Nothing
                , openWarning = warningMsg
                }

    cancelOrderHandler req = do
      let cancelReq = CancelRequest
            { cancelRequestOrderId = cancelOrderId req
            , cancelRequestPositionId = cancelPositionId req
            }

      -- Get broker configuration from user settings
      mBrokerConfig <- getBrokerConfig
      
      case mBrokerConfig of
        Nothing ->
          pure $ CancelOrderResponse
            { cancelSuccess = False
            , cancelError = Just "No broker configured"
            }
        
        Just brokerConfig -> do
          success <- cancelOrder brokerConfig cancelReq

          pure $ CancelOrderResponse
            { cancelSuccess = success
            , cancelError = if success then Nothing else Just "Failed to cancel order"
            }
