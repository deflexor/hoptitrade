{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Orders
  ( OrdersAPI
  , ordersServer
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Domain.Broker (BrokerConfig (..), BrokerMode (..), LiquidityAssessment (..), SlippageEstimate (..), defaultLiquidityThreshold)
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
              
              -- CHECK 2: Liquidity Guardian (MOEX-specific, critical for thin markets)
              liquidityWarning <- case brokerConfig of
                TBankConfig{} -> do
                  assessment <- checkLiquidity brokerConfig instId (openQuantity req) (openSide req)
                  let est = laSlippageEstimate assessment
                  pure $ if laIsLiquid assessment
                    then Nothing  -- Liquid enough
                    else Just $ Text.concat
                      [ "LIQUIDITY WARNING: Expected slippage "
                      , Text.pack $ show (seExpectedSlippage est)
                      , "%, spread "
                      , Text.pack $ show (laSpreadPercent assessment)
                      , "%, bid volume "
                      , Text.pack $ show (laBidVolume assessment)
                      , ", ask volume "
                      , Text.pack $ show (laAskVolume assessment)
                      ]
                _ -> pure Nothing  -- No liquidity check for crypto (24/7 liquid markets)
              
              -- Combine warnings
              let combinedWarning = case (warningMsg, liquidityWarning) of
                    (Just w, Just lw) -> Just $ w <> " | " <> lw
                    (Just w, Nothing) -> Just w
                    (Nothing, Just lw) -> Just lw
                    (Nothing, Nothing) -> Nothing
              
              -- Submit order via configured broker
              response <- placeOrder brokerConfig orderReq

              pure $ OpenOrderResponse
                { openSuccess = True
                , openOrderId = Just $ orderResponseOrderId response
                , openStatus = "pending"
                , openError = Nothing
                , openWarning = combinedWarning
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
