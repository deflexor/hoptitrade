{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Broker
  ( BrokerEffect (..)
  , placeOrder
  , cancelOrder
  , getOrderState
  , getOrders
  , getPositions
  , getPortfolio
  , getOrderBook
  , getInstruments
  , getLastPrice
  , authenticate
  , getConnectionStatus
  , getTradingStatus
  , checkLiquidity
  , runBrokerIO
  , PriceLevel (..)
  , InstrumentInfo (..)
  , InstrumentType (..)
  ) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
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
  )
import Domain.Order (CancelRequest (..), OrderRequest (..), OrderResponse (..), OrderType (..))
import Domain.Position (Position)
import Domain.Types (InstrumentId (..), OrderId (..), OrderStatus (..), Quantity, Side (..))
import qualified Infrastructure.Broker.Bybit as Bybit
import qualified Infrastructure.Broker.OKX as OKX
import qualified Infrastructure.Broker.TBank as TBank
import qualified Infrastructure.Broker.TBank.MarketData as MarketData
import qualified Infrastructure.Broker.TBank.Sandbox as TBankSandbox
import Polysemy
import Polysemy.Embed

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

data BrokerEffect m a where
  PlaceOrder :: BrokerConfig -> OrderRequest -> BrokerEffect m OrderResponse
  CancelOrder :: BrokerConfig -> CancelRequest -> BrokerEffect m Bool
  GetOrderState :: BrokerConfig -> OrderId -> BrokerEffect m (Maybe OrderResponse)
  GetOrders :: BrokerConfig -> BrokerEffect m [OrderResponse]
  GetPositions :: BrokerConfig -> BrokerEffect m [Position]
  GetPortfolio :: BrokerConfig -> BrokerEffect m [(InstrumentId, Quantity)]
  GetOrderBook :: BrokerConfig -> InstrumentId -> Int -> BrokerEffect m (Maybe ([PriceLevel], [PriceLevel]))
  GetInstruments :: BrokerConfig -> Text -> BrokerEffect m [InstrumentInfo]
  GetLastPrice :: BrokerConfig -> InstrumentId -> BrokerEffect m (Maybe Scientific)
  Authenticate :: BrokerConfig -> BrokerEffect m Bool
  GetConnectionStatus :: BrokerConfig -> BrokerEffect m BrokerConnectionStatus
  GetTradingStatus :: BrokerConfig -> InstrumentId -> BrokerEffect m TradingStatus
  CheckLiquidity :: BrokerConfig -> InstrumentId -> Quantity -> Side -> BrokerEffect m LiquidityAssessment

makeSem ''BrokerEffect

logBrokerErr :: Show e => Text -> e -> IO ()
logBrokerErr prefix err = putStrLn $ Text.unpack prefix <> ": " <> show err

failedOrder :: Text -> IO OrderResponse
failedOrder msg = do
  now <- Data.Time.getCurrentTime
  pure OrderResponse
    { orderResponseOrderId = OrderId "failed"
    , orderResponseClientOrderId = Nothing
    , orderResponseStatus = OrderFailed msg
    , orderResponseFilledQty = 0
    , orderResponseAvgPrice = Nothing
    , orderResponseTimestamp = now
    }

runBrokerIO :: Members '[Embed IO] r => Sem (BrokerEffect ': r) a -> Sem r a
runBrokerIO = interpret $ \case
  PlaceOrder config@OKXConfig{} req -> embed @IO $ do
    putStrLn "Broker: Placing order via OKX"
    result <- OKX.placeOrderOKX config req
    case result of
      Left err -> logBrokerErr "OKX Error" err >> failedOrder (Text.pack $ show err)
      Right resp -> pure resp

  PlaceOrder config@TBankConfig{tbankMode = Sandbox} req -> embed @IO $ do
    putStrLn "Broker: Placing order via T-Bank Sandbox"
    case tbankAccountId config of
      Nothing -> failedOrder "T-Bank Sandbox requires accountId"
      Just accId -> do
        result <- TBankSandbox.postSandboxOrder (tbankToken config) accId req
        case result of
          Left err -> logBrokerErr "T-Bank Error" err >> failedOrder (Text.pack $ show err)
          Right resp -> pure resp

  PlaceOrder config@TBankConfig{tbankMode = Real} req -> embed @IO $ do
    putStrLn "Broker: Placing order via T-Bank (REAL MONEY!)"
    case tbankAccountId config of
      Nothing -> failedOrder "T-Bank Real trading requires accountId"
      Just accId -> do
        result <- TBank.postOrder (tbankToken config) accId req
        case result of
          Left err -> logBrokerErr "T-Bank Error" err >> failedOrder (Text.pack $ show err)
          Right resp -> pure resp

  PlaceOrder config@BybitConfig{} req -> embed @IO $ do
    putStrLn "Broker: Placing order via Bybit"
    result <- Bybit.placeOrderBybit config req False
    case result of
      Left err -> logBrokerErr "Bybit Error" err >> failedOrder (Text.pack $ show err)
      Right resp -> pure resp

  CancelOrder config@OKXConfig{} req -> embed @IO $ do
    result <- OKX.cancelOrderOKX config req
    case result of
      Left err -> logBrokerErr "OKX Error" err >> pure False
      Right success -> pure success

  CancelOrder config@TBankConfig{} req -> embed @IO $ do
    case tbankAccountId config of
      Nothing -> pure False
      Just accId -> do
        result <- TBank.cancelOrder (tbankToken config) accId (cancelRequestOrderId req)
        case result of
          Left err -> logBrokerErr "T-Bank Error" err >> pure False
          Right success -> pure success

  CancelOrder config@BybitConfig{} req -> embed @IO $ do
    result <- Bybit.cancelOrderBybit config req ""
    case result of
      Left err -> logBrokerErr "Bybit Error" err >> pure False
      Right success -> pure success

  GetOrderState OKXConfig{} _ -> embed @IO $ pure Nothing
  GetOrderState config@TBankConfig{} orderId -> embed @IO $
    case tbankAccountId config of
      Nothing -> pure Nothing
      Just accId -> do
        result <- TBank.getOrderState (tbankToken config) accId orderId
        case result of
          Left err -> logBrokerErr "T-Bank Error" err >> pure Nothing
          Right mResp -> pure mResp
  GetOrderState BybitConfig{} _ -> embed @IO $ pure Nothing

  GetOrders _ -> embed @IO $ pure []

  GetPositions _ -> embed @IO $ pure []

  GetPortfolio OKXConfig{} -> embed @IO $ pure []
  GetPortfolio config@TBankConfig{tbankMode = Sandbox} -> embed @IO $
    case tbankAccountId config of
      Nothing -> pure []
      Just accId -> do
        result <- TBankSandbox.getSandboxPortfolio (tbankToken config) accId TBankSandbox.RUB
        case result of
          Left err -> logBrokerErr "T-Bank Error" err >> pure []
          Right portfolio -> pure $ map extractPortfolioPosition portfolio
  GetPortfolio TBankConfig{} -> embed @IO $ pure []
  GetPortfolio BybitConfig{} -> embed @IO $ pure []

  GetOrderBook config@OKXConfig{} instId depth -> embed @IO $ do
    result <- OKX.getOrderBookOKX config instId depth
    case result of
      Left err -> logBrokerErr "OKX Error" err >> pure Nothing
      Right mBook -> pure mBook
  GetOrderBook config@TBankConfig{} instId depth -> embed @IO $ do
    result <- TBank.getOrderBook (tbankToken config) instId depth
    case result of
      Left err -> logBrokerErr "T-Bank Error" err >> pure Nothing
      Right (bids, asks) -> pure $ Just (bids, asks)
  GetOrderBook BybitConfig{} _ _ -> embed @IO $ pure Nothing

  GetInstruments config@OKXConfig{} underlying -> embed @IO $ do
    result <- OKX.getInstrumentsOKX config underlying
    case result of
      Left err -> logBrokerErr "OKX Error" err >> pure []
      Right _ -> pure []
  GetInstruments TBankConfig{} _ -> embed @IO $ pure []
  GetInstruments config@BybitConfig{} baseCoin -> embed @IO $ do
    result <- Bybit.getInstrumentsBybit config baseCoin
    case result of
      Left err -> logBrokerErr "Bybit Error" err >> pure []
      Right instruments -> pure $ map bybitToInfo instruments

  GetLastPrice _ _ -> embed @IO $ pure Nothing

  Authenticate OKXConfig{} -> embed @IO $ pure True
  Authenticate TBankConfig{} -> embed @IO $ pure True
  Authenticate BybitConfig{} -> embed @IO $ pure True

  GetConnectionStatus _ -> embed @IO $ pure Disconnected

  GetTradingStatus OKXConfig{} _ -> embed @IO $ pure Trading
  GetTradingStatus BybitConfig{} _ -> embed @IO $ pure Trading
  GetTradingStatus config@TBankConfig{} instId -> embed @IO $ do
    result <- TBank.getTradingStatus (tbankToken config) instId
    case result of
      Left err -> logBrokerErr "T-Bank Error" err >> pure NotAvailable
      Right status -> pure status

  CheckLiquidity OKXConfig{} instId qty _side -> embed @IO $ do
    currentTime <- Data.Time.getCurrentTime
    pure $ defaultLiq instId qty currentTime 0.001 0.005
  CheckLiquidity BybitConfig{} instId qty _side -> embed @IO $ do
    currentTime <- Data.Time.getCurrentTime
    pure $ defaultLiq instId qty currentTime 0.002 0.01
  CheckLiquidity config@TBankConfig{} instId qty side -> embed @IO $ do
    result <- TBank.checkLiquidity (tbankToken config) instId qty side defaultLiquidityThreshold
    case result of
      MarketData.LiquidityOK assessment -> pure assessment
      MarketData.LiquidityWarning assessment _ -> pure assessment
      MarketData.LiquidityCritical assessment -> pure assessment

defaultLiq :: InstrumentId -> Quantity -> Data.Time.UTCTime -> Scientific -> Scientific -> LiquidityAssessment
defaultLiq instId qty currentTime expected maxSlip = LiquidityAssessment
  { laInstrumentId = instId
  , laTimestamp = currentTime
  , laBidVolume = 0
  , laAskVolume = 0
  , laSpreadPercent = 0
  , laSlippageEstimate = SlippageEstimate
      { seForQuantity = qty
      , seExpectedSlippage = expected
      , seMaxSlippage = maxSlip
      , seConfidence = HighConfidence
      }
  , laIsLiquid = True
  }

bybitToInfo :: Bybit.BybitInstrument -> InstrumentInfo
bybitToInfo i = InstrumentInfo
  { iiInstrumentId = InstrumentId (Bybit.biSymbol i)
  , iiTicker = Bybit.biSymbol i
  , iiName = Bybit.biSymbol i
  , iiInstrumentType = CryptoOption
  , iiExchange = "Bybit"
  , iiCurrency = Bybit.biSettleCoin i
  , iiLotSize = parseQty (Bybit.biQtyStep i)
  , iiMinQuantity = parseQty (Bybit.biMinOrderQty i)
  , iiTickSize = parseQty (Bybit.biTickSize i)
  , iiIsTradable = Bybit.biStatus i == "Trading"
  }
  where
    parseQty t = case reads (Text.unpack t) of
      [(n, _)] -> n
      _ -> 0

extractPortfolioPosition :: TBankSandbox.PortfolioPosition -> (InstrumentId, Quantity)
extractPortfolioPosition pos =
  let instId = case TBankSandbox.ppFigi pos of
        Just figi -> InstrumentId figi
        Nothing -> InstrumentId "unknown"
      qty = case TBankSandbox.ppQuantity pos of
        Just q -> TBankSandbox.quotationToScientific q
        Nothing -> 0
  in (instId, qty)
