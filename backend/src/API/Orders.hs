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
import Domain.Broker (BrokerConfig (..), BrokerMode (..), LiquidityAssessment (..), SlippageEstimate (..), isMarketOpen)
import Domain.Order (CancelRequest (..), OrderRequest (..), OrderResponse (..), OrderType (..))
import Domain.Settings (settingsToBrokerConfig)
import Domain.Types (InstrumentId (..), OrderId (..), PositionId (..), Quantity, Side (..), UserId (..))
import Domain.User (AuthToken (..))
import Effects.Auth (AuthEffect, extractBearerToken, verifyToken)
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
  "orders" :> "open" :> Header "Authorization" Text :> ReqBody '[JSON] OpenOrderRequest :> Post '[JSON] OpenOrderResponse
  :<|> "orders" :> "cancel" :> Header "Authorization" Text :> ReqBody '[JSON] CancelOrderRequest :> Post '[JSON] CancelOrderResponse

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

ordersServer :: Members '[AuthEffect, BrokerEffect, PositionEffect, SettingsEffect, Embed IO] r
              => ServerT OrdersAPI (Sem r)
ordersServer = openOrderHandler :<|> cancelOrderHandler
  where
    -- Resolve user from Authorization header
    resolveUser :: Member AuthEffect r => Maybe Text -> Sem r (Either Text UserId)
    resolveUser mAuthHeader = case mAuthHeader >>= extractBearerToken of
      Nothing -> pure $ Left "Missing or invalid Authorization header"
      Just token -> do
        mUid <- verifyToken (AuthToken token)
        pure $ maybe (Left "Invalid or expired token") Right mUid

    -- Helper to get broker config from settings
    getBrokerConfig :: Members '[AuthEffect, SettingsEffect] r => UserId -> Sem r (Maybe BrokerConfig)
    getBrokerConfig uid = do
      settings <- getSettings uid
      pure $ settingsToBrokerConfig settings

    openOrderHandler mAuthHeader req = do
      authResult <- resolveUser mAuthHeader
      case authResult of
        Left err -> pure $ OpenOrderResponse
          { openSuccess = False
          , openOrderId = Nothing
          , openStatus = "unauthorized"
          , openError = Just err
          , openWarning = Nothing
          }
        Right uid -> do
          let instId = openInstrumentId req
          
          -- CHECK 1: Duplicate position prevention
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
              mBrokerConfig <- getBrokerConfig uid
              
              case mBrokerConfig of
                Nothing -> 
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
                  
                  -- CHECK 2: Market hours (MOEX-specific)
                  marketCheck <- case brokerConfig of
                    TBankConfig{} -> do
                      status <- getTradingStatus brokerConfig instId
                      pure $ if isMarketOpen status
                        then Nothing
                        else Just $ "MOEX market is closed (status: " <> Text.pack (show status)
                              <> "). Trading hours: Mon-Fri 10:00-18:45 MSK"
                    _ -> pure Nothing  -- Crypto markets are 24/7

                  case marketCheck of
                    Just marketError -> pure $ OpenOrderResponse
                      { openSuccess = False
                      , openOrderId = Nothing
                      , openStatus = "rejected"
                      , openError = Just marketError
                      , openWarning = Nothing
                      }
                    Nothing -> do
                      -- CHECK 3: Liquidity Guardian (MOEX-specific)
                      liquidityWarning <- case brokerConfig of
                        TBankConfig{} -> do
                          assessment <- checkLiquidity brokerConfig instId (openQuantity req) (openSide req)
                          let est = laSlippageEstimate assessment
                          pure $ if laIsLiquid assessment
                            then Nothing
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
                        _ -> pure Nothing
                      
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

    cancelOrderHandler mAuthHeader req = do
      authResult <- resolveUser mAuthHeader
      case authResult of
        Left _ -> pure $ CancelOrderResponse
          { cancelSuccess = False
          , cancelError = Just "Unauthorized"
          }
        Right _uid -> do
          let cancelReq = CancelRequest
                { cancelRequestOrderId = cancelOrderId req
                , cancelRequestPositionId = cancelPositionId req
                }

          -- Get broker configuration from user settings
          mBrokerConfig <- getBrokerConfig _uid
          
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
