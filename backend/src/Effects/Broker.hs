{-# LANGUAGE TemplateHaskell #-}

module Effects.Broker
  ( -- * Broker Effect
    BrokerEffect (..)
    -- * Order Operations
  , placeOrder
  , cancelOrder
  , getOrderState
  , getOrders
    -- * Position Operations
  , getPositions
  , getPortfolio
    -- * Market Data
  , getOrderBook
  , getInstruments
  , getLastPrice
    -- * Broker Management
  , authenticate
  , getConnectionStatus
    -- * MOEX-specific Operations
  , getTradingStatus
  , checkLiquidity
    -- * Interpreters
  , runBrokerIO
    -- * Types
  , PriceLevel (..)
  , InstrumentInfo (..)
  , InstrumentType (..)
  ) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Time
import Domain.Broker
  ( BrokerConfig (..)
  , BrokerConnectionStatus (..)
  , LiquidityAssessment (..)
  , SlippageEstimate (..)
  , SlippageConfidence (..)
  , TradingStatus (..)
  , BrokerMode (..)
  , PriceLevel (..)
  , defaultLiquidityThreshold
  , isMarketOpen
  )
import Domain.Order (CancelRequest (..), OrderRequest (..), OrderResponse (..), OrderType (..))
import Domain.Position (Position)
import Domain.Types (InstrumentId (..), OrderId (..), PositionId (..), Quantity, Side (..), OrderStatus (..))
import qualified Infrastructure.Broker.OKX as OKX
import qualified Infrastructure.Broker.TBank as TBank
import qualified Infrastructure.Broker.TBank.MarketData as MarketData
import qualified Infrastructure.Broker.TBank.Sandbox as TBankSandbox
import Polysemy
import Polysemy.Embed

-- ============================================================================
-- Instrument Info
-- ============================================================================

-- | Generic instrument information across exchanges
data InstrumentInfo = InstrumentInfo
  { iiInstrumentId :: InstrumentId
  , iiTicker :: Text
  , iiName :: Text
  , iiInstrumentType :: InstrumentType
  , iiExchange :: Text
  , iiCurrency :: Text
  , iiLotSize :: Quantity
  , iiMinQuantity :: Quantity
  , iiTickSize :: Scientific
  , iiIsTradable :: Bool
  } deriving stock (Eq, Show)

data InstrumentType
  = Stock
  | Bond
  | Future
  | Option
  | CryptoSpot
  | CryptoPerp
  | CryptoOption
  deriving stock (Eq, Show)

-- ============================================================================
-- Broker Effect Definition
-- ============================================================================

-- | Abstract broker effect supporting multiple exchanges (OKX, T-Bank)
-- Each operation takes a BrokerConfig to determine which exchange to use
-- and whether to use sandbox or real trading.
data BrokerEffect m a where
  -- Order operations
  PlaceOrder :: BrokerConfig -> OrderRequest -> BrokerEffect m OrderResponse
  CancelOrder :: BrokerConfig -> CancelRequest -> BrokerEffect m Bool
  GetOrderState :: BrokerConfig -> OrderId -> BrokerEffect m (Maybe OrderResponse)
  GetOrders :: BrokerConfig -> BrokerEffect m [OrderResponse]

  -- Position operations
  GetPositions :: BrokerConfig -> BrokerEffect m [Position]
  GetPortfolio :: BrokerConfig -> BrokerEffect m [(InstrumentId, Quantity)]

  -- Market data operations
  GetOrderBook :: BrokerConfig -> InstrumentId -> Int -> BrokerEffect m (Maybe ([PriceLevel], [PriceLevel]))
  GetInstruments :: BrokerConfig -> Text -> BrokerEffect m [InstrumentInfo]
  GetLastPrice :: BrokerConfig -> InstrumentId -> BrokerEffect m (Maybe Scientific)

  -- Broker management
  Authenticate :: BrokerConfig -> BrokerEffect m Bool
  GetConnectionStatus :: BrokerConfig -> BrokerEffect m BrokerConnectionStatus

  -- MOEX-specific: Trading status and liquidity
  GetTradingStatus :: BrokerConfig -> InstrumentId -> BrokerEffect m TradingStatus
  CheckLiquidity :: BrokerConfig -> InstrumentId -> Quantity -> Side -> BrokerEffect m LiquidityAssessment

makeSem ''BrokerEffect

-- ============================================================================
-- IO Interpreter (Production)
-- ============================================================================

-- | The IO interpreter dispatches to the appropriate exchange implementation
-- based on the BrokerConfig type.
runBrokerIO :: Members '[Embed IO] r => Sem (BrokerEffect ': r) a -> Sem r a
runBrokerIO = interpret $ \case
  -- Order operations
  PlaceOrder config@OKXConfig{} req -> embed $ do
    putStrLn "Broker: Placing order via OKX"
    result <- OKX.placeOrderOKX config req
    case result of
      Left err -> error $ "OKX Error: " <> show err  -- TODO: Proper error handling
      Right resp -> pure resp

  PlaceOrder config@TBankConfig{tbankMode = Sandbox} req -> embed $ do
    putStrLn "Broker: Placing order via T-Bank Sandbox"
    case tbankAccountId config of
      Nothing -> error "T-Bank Sandbox requires accountId"
      Just accId -> do
        result <- TBankSandbox.postSandboxOrder (tbankToken config) accId req
        case result of
          Left err -> error $ "T-Bank Error: " <> show err
          Right resp -> pure resp

  PlaceOrder config@TBankConfig{tbankMode = Real} req -> embed $ do
    putStrLn "Broker: Placing order via T-Bank (REAL MONEY!)"
    putStrLn "WARNING: This will use real funds!"
    case tbankAccountId config of
      Nothing -> error "T-Bank Real trading requires accountId"
      Just accId -> do
        result <- TBank.postOrder (tbankToken config) accId req
        case result of
          Left err -> error $ "T-Bank Error: " <> show err
          Right resp -> pure resp

  CancelOrder config@OKXConfig{} req -> embed $ do
    putStrLn "Broker: Cancelling order via OKX"
    result <- OKX.cancelOrderOKX config req
    case result of
      Left err -> error $ "OKX Error: " <> show err
      Right success -> pure success

  CancelOrder config@TBankConfig{} req -> embed $ do
    putStrLn "Broker: Cancelling order via T-Bank"
    result <- TBank.cancelOrder (tbankToken config) (cancelRequestOrderId req)
    case result of
      Left err -> error $ "T-Bank Error: " <> show err
      Right success -> pure success

  GetOrderState config@OKXConfig{} orderId -> embed $ do
    putStrLn "Broker: Getting order state via OKX"
    -- TODO: Implement OKX.getOrderState
    pure Nothing

  GetOrderState config@TBankConfig{} orderId -> embed $ do
    putStrLn "Broker: Getting order state via T-Bank"
    result <- TBank.getOrderState (tbankToken config) orderId
    case result of
      Left err -> error $ "T-Bank Error: " <> show err
      Right mResp -> pure mResp

  GetOrders config -> embed $ do
    putStrLn "Broker: Getting orders"
    -- TODO: Implement per-exchange order listing
    pure []

  -- Position operations
  GetPositions config -> embed $ do
    putStrLn "Broker: Getting positions"
    -- TODO: Implement per-exchange position query
    pure []

  GetPortfolio config@OKXConfig{} -> embed $ do
    putStrLn "Broker: Getting portfolio via OKX"
    -- TODO: Implement OKX portfolio query
    pure []

  GetPortfolio config@TBankConfig{tbankMode = Sandbox} -> embed $ do
    putStrLn "Broker: Getting portfolio via T-Bank Sandbox"
    case tbankAccountId config of
      Nothing -> pure []
      Just accId -> do
        result <- TBankSandbox.getSandboxPortfolio (tbankToken config) accId TBankSandbox.RUB
        case result of
          Left err -> error $ "T-Bank Error: " <> show err
          Right portfolio -> pure $ map extractPortfolioPosition portfolio

  GetPortfolio config@TBankConfig{tbankMode = Real} -> embed $ do
    putStrLn "Broker: Getting portfolio via T-Bank Real"
    -- TODO: Implement T-Bank real portfolio query
    pure []

  -- Market data operations
  GetOrderBook config@OKXConfig{} instId depth -> embed $ do
    putStrLn "Broker: Getting order book via OKX"
    result <- OKX.getOrderBookOKX config instId depth
    case result of
      Left err -> error $ "OKX Error: " <> show err
      Right mBook -> pure mBook

  GetOrderBook config@TBankConfig{} instId depth -> embed $ do
    putStrLn "Broker: Getting order book via T-Bank"
    result <- TBank.getOrderBook (tbankToken config) instId depth
    case result of
      Left err -> error $ "T-Bank Error: " <> show err
      Right (bids, asks) -> pure $ Just (bids, asks)

  GetInstruments config@OKXConfig{} underlying -> embed $ do
    putStrLn "Broker: Getting instruments via OKX"
    result <- OKX.getInstrumentsOKX config underlying
    case result of
      Left err -> error $ "OKX Error: " <> show err
      Right _instruments -> pure []  -- TODO: Map OKXInstrument to InstrumentInfo

  GetInstruments config@TBankConfig{} underlying -> embed $ do
    putStrLn "Broker: Getting instruments via T-Bank"
    -- TODO: Implement T-Bank instrument listing
    pure []

  GetLastPrice config@OKXConfig{} instId -> embed $ do
    putStrLn "Broker: Getting last price via OKX"
    -- TODO: Implement OKX price query
    pure Nothing

  GetLastPrice config@TBankConfig{} instId -> embed $ do
    putStrLn "Broker: Getting last price via T-Bank"
    -- TODO: Implement T-Bank price query
    pure Nothing

  -- Broker management
  Authenticate config@OKXConfig{} -> embed $ do
    putStrLn "Broker: Authenticating with OKX"
    -- TODO: Implement OKX authentication
    pure True

  Authenticate config@TBankConfig{} -> embed $ do
    putStrLn "Broker: Authenticating with T-Bank"
    -- TODO: Validate token with T-Bank
    pure True

  GetConnectionStatus config -> embed $ do
    putStrLn "Broker: Getting connection status"
    -- TODO: Implement per-exchange connection check
    pure Disconnected

  -- MOEX-specific operations
  GetTradingStatus config@OKXConfig{} instId -> embed $ do
    putStrLn "Broker: Getting trading status via OKX (always open)"
    -- OKX crypto markets are always open
    pure Trading

  GetTradingStatus config@TBankConfig{} instId -> embed $ do
    putStrLn "Broker: Getting trading status via T-Bank"
    result <- TBank.getTradingStatus (tbankToken config) instId
    case result of
      Left err -> error $ "T-Bank Error: " <> show err
      Right status -> pure status

  CheckLiquidity config@OKXConfig{} instId qty side -> embed $ do
    putStrLn "Broker: Checking liquidity via OKX"
    -- OKX generally has good liquidity, but still check
    currentTime <- Data.Time.getCurrentTime
    pure $ LiquidityAssessment
      { laInstrumentId = instId
      , laTimestamp = currentTime
      , laBidVolume = 0  -- TODO: Query actual order book
      , laAskVolume = 0
      , laSpreadPercent = 0
      , laSlippageEstimate = SlippageEstimate
          { seForQuantity = qty
          , seExpectedSlippage = 0.001  -- 0.1% for liquid crypto
          , seMaxSlippage = 0.005       -- 0.5% worst case
          , seConfidence = HighConfidence
          }
      , laIsLiquid = True
      }

  CheckLiquidity config@TBankConfig{} instId qty side -> embed $ do
    putStrLn "Broker: Checking liquidity via T-Bank (CRITICAL for MOEX)"
    result <- TBank.checkLiquidity (tbankToken config) instId qty side defaultLiquidityThreshold
    case result of
      MarketData.LiquidityOK assessment -> pure assessment
      MarketData.LiquidityWarning assessment _warn -> pure assessment
      MarketData.LiquidityCritical assessment -> pure assessment

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Extract (InstrumentId, Quantity) from T-Bank portfolio position
extractPortfolioPosition :: TBankSandbox.PortfolioPosition -> (InstrumentId, Quantity)
extractPortfolioPosition pos =
  let instId = case TBankSandbox.ppFigi pos of
        Just figi -> InstrumentId figi
        Nothing -> InstrumentId "unknown"
      qty = case TBankSandbox.ppQuantity pos of
        Just q -> TBankSandbox.quotationToScientific q
        Nothing -> 0
  in (instId, qty)
