{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Infrastructure.Broker.TBank
  ( -- * T-Bank Client Initialization
    runTBankBroker
    -- * Sandbox Operations (re-exported)
  , module Infrastructure.Broker.TBank.Sandbox
    -- * Market Data Operations (re-exported)
  , module Infrastructure.Broker.TBank.MarketData
    -- * Real Operations
  , postOrder
  , cancelOrder
  , getOrderState
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (eitherDecodeStrict, encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID.V4 (nextRandom)
import qualified Data.UUID as UUID
import Domain.Broker
  ( BrokerConfig (..)
  , BrokerMode (..)
  , LiquidityAssessment (..)
  , PriceLevel (..)
  , SlippageConfidence (..)
  , SlippageEstimate (..)
  , TradingSession (..)
  , TradingStatus (..)
  , defaultLiquidityThreshold
  , isMarketOpen
  )
import Domain.Order (OrderRequest (..), OrderResponse (..))
import Domain.Types (InstrumentId (..), OrderId (..), OrderStatus (..), Quantity, Side (..))
import Infrastructure.Broker.TBank.Sandbox
  ( MoneyValue (..)
  , OrderDirection (..)
  , Quotation (..)
  , SandboxOrderResponse (..)
  , TBankError (..)
  , TBankOrderType (..)
  , buildAuthRequest
  , mapExecutionStatus
  , mapOrderType
  , mapSide
  , moneyValueToScientific
  , sandboxOrderResponseToOrderResponse
  , scientificToQuotation
  )
import qualified Infrastructure.Broker.TBank.Sandbox as Sandbox
import Infrastructure.Broker.TBank.MarketData
import qualified Infrastructure.Broker.TBank.MarketData as MarketData
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- ============================================================================
-- Configuration
-- ============================================================================

tbankProductionUrl :: String
tbankProductionUrl = "https://invest-public-api.tinkoff.ru/rest/"

tbankOrdersService :: String -> String
tbankOrdersService method = tbankProductionUrl <> "tinkoff.public.invest.api.contract.v1.OrdersService/" <> method

-- ============================================================================
-- Broker Interpreter Entry Point
-- ============================================================================

-- | Initialize T-Bank broker (checks mode and validates connection)
runTBankBroker :: BrokerConfig -> IO ()
runTBankBroker config@TBankConfig{tbankMode = Sandbox} = do
  putStrLn "T-Bank Broker: Sandbox mode initialized"

runTBankBroker config@TBankConfig{tbankMode = Real} = do
  putStrLn "T-Bank Broker: Real trading mode initialized"
  putStrLn "WARNING: Using real money!"

-- ============================================================================
-- Real Trading Operations (MOEX via T-Bank Invest API)
-- ============================================================================

-- | Place a real order on MOEX via T-Bank Invest API
-- Uses OrdersService/PostOrder endpoint with idempotency key
postOrder :: Text -> Text -> OrderRequest -> IO (Either TBankError OrderResponse)
postOrder token accountId req = do
  -- Generate idempotency key (orderId in request = UID format)
  orderRequestId <- UUID.toText <$> nextRandom

  let url = tbankOrdersService "PostOrder"
      priceObj = case orderRequestPrice req of
        Just p  -> scientificToQuotation p
        Nothing -> Quotation 0 0

      requestBody = object
        [ "accountId" .= accountId
        , "instrumentId" .= unInstrumentId (orderRequestInstrumentId req)
        , "quantity" .= Text.pack (show (truncate (orderRequestQuantity req) :: Integer))
        , "price" .= priceObj
        , "direction" .= mapSide (orderRequestSide req)
        , "orderType" .= mapOrderType (orderRequestOrderType req)
        , "orderId" .= orderRequestId
        ]
      body = RequestBodyLBS $ encode requestBody

  result <- try @SomeException $ do
    request <- buildAuthRequest token url body
    manager <- newManager tlsManagerSettings
    httpLbs request manager

  case result of
    Left e -> return $ Left $ TBankNetworkError (Text.pack $ show (e :: SomeException))
    Right response -> do
      let bodyBytes = BL.toStrict $ responseBody response
      case statusCode $ responseStatus response of
        200 -> case eitherDecodeStrict bodyBytes of
          Left err -> return $ Left $ TBankParseError (Text.pack err)
          Right tbankResp -> do
            orderResp <- sandboxOrderResponseToOrderResponse tbankResp
            return $ Right orderResp
        400 -> return $ Left $ TBankApiError "Invalid order parameters" Nothing
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        403 -> return $ Left $ TBankApiError "Insufficient permissions (need full-access token)" Nothing
        404 -> return $ Left $ TBankHttpError 404 "Account or instrument not found"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)

-- | Cancel a real order on MOEX via T-Bank Invest API
cancelOrder :: Text -> Text -> OrderId -> IO (Either TBankError Bool)
cancelOrder token accountId (OrderId orderId) = do
  let url = tbankOrdersService "CancelOrder"
      body = RequestBodyLBS $ encode $ object
        [ "accountId" .= accountId
        , "orderId" .= orderId
        ]

  result <- try @SomeException $ do
    request <- buildAuthRequest token url body
    manager <- newManager tlsManagerSettings
    httpLbs request manager

  case result of
    Left e -> return $ Left $ TBankNetworkError (Text.pack $ show (e :: SomeException))
    Right response ->
      case statusCode $ responseStatus response of
        200 -> return $ Right True
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        404 -> return $ Left $ TBankHttpError 404 "Order not found"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)

-- | Get the current state of a real order via T-Bank Invest API
getOrderState :: Text -> Text -> OrderId -> IO (Either TBankError (Maybe OrderResponse))
getOrderState token accountId (OrderId orderId) = do
  let url = tbankOrdersService "GetOrderState"
      body = RequestBodyLBS $ encode $ object
        [ "accountId" .= accountId
        , "orderId" .= orderId
        ]

  result <- try @SomeException $ do
    request <- buildAuthRequest token url body
    manager <- newManager tlsManagerSettings
    httpLbs request manager

  case result of
    Left e -> return $ Left $ TBankNetworkError (Text.pack $ show (e :: SomeException))
    Right response -> do
      let bodyBytes = BL.toStrict $ responseBody response
      case statusCode $ responseStatus response of
        200 -> case eitherDecodeStrict bodyBytes of
          Left err -> return $ Left $ TBankParseError (Text.pack err)
          Right tbankResp -> do
            orderResp <- sandboxOrderResponseToOrderResponse tbankResp
            return $ Right $ Just orderResp
        404 -> return $ Right Nothing
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)

-- ============================================================================
-- Market Data Operations (delegated to MarketData module)
-- ============================================================================

-- | Helper to check if market is open for a specific instrument
checkMarketOpen :: Text -> InstrumentId -> IO (Either TBankError Bool)
checkMarketOpen token instId = do
  statusResult <- MarketData.getTradingStatus token instId
  return $ fmap isMarketOpen statusResult
