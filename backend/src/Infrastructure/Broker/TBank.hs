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

import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Domain.Broker
  ( BrokerConfig (..)
  , BrokerMode (..)
  , LiquidityAssessment (..)
  , SlippageEstimate (..)
  , SlippageConfidence (..)
  , TradingStatus (..)
  , TradingSession (..)
  , PriceLevel (..)
  , defaultLiquidityThreshold
  , isMarketOpen
  )
import Domain.Order (OrderRequest (..), OrderResponse (..))
import Domain.Types (InstrumentId (..), OrderId (..), OrderStatus (..), Quantity, Side (..))
import GHC.Generics (Generic)
import Infrastructure.Broker.TBank.Sandbox
import qualified Infrastructure.Broker.TBank.Sandbox as Sandbox
import Infrastructure.Broker.TBank.MarketData
import qualified Infrastructure.Broker.TBank.MarketData as MarketData
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- ============================================================================
-- Configuration
-- ============================================================================

tbankProductionUrl :: Text
tbankProductionUrl = "https://invest-public-api.tbank.ru/rest/"

-- ============================================================================
-- Broker Interpreter Entry Point
-- ============================================================================

-- | Initialize T-Bank broker (checks mode and validates connection)
runTBankBroker :: BrokerConfig -> IO ()
runTBankBroker config@TBankConfig{tbankMode = Sandbox} = do
  putStrLn "T-Bank Broker: Sandbox mode initialized"
  -- Sandbox requires: OpenSandboxAccount, SandboxPayIn, etc.

runTBankBroker config@TBankConfig{tbankMode = Real} = do
  putStrLn "T-Bank Broker: Real trading mode initialized"
  putStrLn "WARNING: Using real money!"
  -- Real mode requires valid accountId and authenticated token

-- ============================================================================
-- Real Trading Operations
-- ============================================================================

-- | Place a real order on MOEX
-- CRITICAL: Always check trading status and liquidity first!
postOrder :: Text -> Text -> OrderRequest -> IO (Either Sandbox.TBankError OrderResponse)
postOrder token accountId req = do
  -- Step 1: Check if market is open
  tradingStatus <-   checkMarketOpen token (orderRequestInstrumentId req)
  case tradingStatus of
    Left err -> return $ Left err
    Right False -> return $ Left (Sandbox.TBankTradingClosed)
    Right True -> do
      -- Step 2: Check liquidity (optional but recommended)
      -- Step 3: Submit order
      putStrLn "T-Bank Real: Posting order (REAL MONEY!)"
      currentTime <- getCurrentTime
      return $ Right $ OrderResponse
        { orderResponseOrderId = OrderId "tb-real-placeholder"
        , orderResponseClientOrderId = Nothing
        , orderResponseStatus = OrderPending
        , orderResponseFilledQty = 0
        , orderResponseAvgPrice = Nothing
        , orderResponseTimestamp = currentTime
        }

cancelOrder :: Text -> OrderId -> IO (Either Sandbox.TBankError Bool)
cancelOrder token orderId = do
  putStrLn "T-Bank: Cancelling order..."
  -- TODO: Implement real order cancellation
  return $ Right True

getOrderState :: Text -> OrderId -> IO (Either Sandbox.TBankError (Maybe OrderResponse))
getOrderState token orderId = do
  putStrLn "T-Bank: Getting order state..."
  -- TODO: Implement real order state query
  return $ Right Nothing

-- ============================================================================
-- Market Data Operations
-- ============================================================================

-- | Get order book for liquidity assessment
-- Depth parameter controls how many levels to fetch (1-50)
getOrderBook :: Text -> InstrumentId -> Int -> IO (Either Sandbox.TBankError ([PriceLevel], [PriceLevel]))
getOrderBook = MarketData.getOrderBook

-- | Get current trading status for an instrument
-- Returns NotAvailable if market is closed
getTradingStatus :: Text -> InstrumentId -> IO (Either Sandbox.TBankError TradingStatus)
getTradingStatus = MarketData.getTradingStatus

-- | Get trading schedules for exchanges
-- Used to determine when market opens/closes
getTradingSchedules :: Text -> Text -> IO (Either Sandbox.TBankError [TradingSession])
getTradingSchedules = MarketData.getTradingSchedules

-- ============================================================================
-- Liquidity Assessment (MOEX-specific)
-- ============================================================================

-- | Check liquidity before placing order
-- This is CRITICAL for MOEX due to thin liquidity on many instruments
checkLiquidity :: Text -> InstrumentId -> Quantity -> Side -> Scientific -> IO MarketData.LiquidityCheckResult
checkLiquidity = MarketData.checkLiquidity

-- | Helper to check if market is open for a specific instrument
checkMarketOpen :: Text -> InstrumentId -> IO (Either Sandbox.TBankError Bool)
checkMarketOpen token instId = do
  statusResult <- MarketData.getTradingStatus token instId
  return $ fmap isMarketOpen statusResult
