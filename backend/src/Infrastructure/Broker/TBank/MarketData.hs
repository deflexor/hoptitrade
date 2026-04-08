{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Infrastructure.Broker.TBank.MarketData
  ( -- * Market Data Operations
    getOrderBook
  , getTradingStatus
  , getTradingSchedules
  , checkLiquidity
    -- * Liquidity Assessment
  , LiquidityCheckResult (..)
  , SlippageWarning (..)
  , defaultLiquidityThreshold
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (listToMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime, TimeOfDay, getCurrentTime, Day)
import Domain.Broker
  ( DaySession (..)
  , LiquidityAssessment (..)
  , PriceLevel (..)
  , SessionSegment (..)
  , SlippageConfidence (..)
  , SlippageEstimate (..)
  , TradingSession (..)
  , TradingStatus (..)
  , defaultLiquidityThreshold
  )
import Domain.Order (OrderRequest (..))
import Domain.Types (Side (..))
import Domain.Types (InstrumentId (..), Quantity)
import qualified Infrastructure.Broker.TBank.Sandbox as Sandbox
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- ============================================================================
-- Configuration
-- ============================================================================

tbankProductionUrl :: String
tbankProductionUrl = "https://invest-public-api.tbank.ru/rest/"

-- ============================================================================
-- Liquidity Check Result
-- ============================================================================

data LiquidityCheckResult
  = LiquidityOK LiquidityAssessment
  | LiquidityWarning LiquidityAssessment SlippageWarning
  | LiquidityCritical LiquidityAssessment
  deriving (Show, Eq)

data SlippageWarning = SlippageWarning
  { swExpectedSlippage :: Scientific
  , swThreshold :: Scientific
  , swMessage :: Text
  } deriving (Show, Eq)

-- ============================================================================
-- Order Book
-- ============================================================================

-- | Get order book for an instrument
-- Returns (bids, asks) where each is a list of PriceLevel
data OrderBookResponse = OrderBookResponse
  { obrFigi :: Text
  , obrDepth :: Int
  , obrBids :: [OrderBookEntry]
  , obrAsks :: [OrderBookEntry]
  , obrLastPrice :: Maybe Sandbox.Quotation
  , obrClosePrice :: Maybe Sandbox.Quotation
  } deriving (Show, Eq)

data OrderBookEntry = OrderBookEntry
  { obePrice :: Sandbox.Quotation
  , obeQuantity :: Integer
  } deriving (Show, Eq)

instance FromJSON OrderBookEntry where
  parseJSON = withObject "OrderBookEntry" $ \v -> OrderBookEntry
    <$> v .: "price"
    <*> v .: "quantity"

instance FromJSON OrderBookResponse where
  parseJSON = withObject "OrderBookResponse" $ \v -> OrderBookResponse
    <$> v .: "figi"
    <*> v .: "depth"
    <*> v .:? "bids" .!= []
    <*> v .:? "asks" .!= []
    <*> v .:? "lastPrice"
    <*> v .:? "closePrice"

getOrderBook :: Text -> InstrumentId -> Int -> IO (Either Sandbox.TBankError ([PriceLevel], [PriceLevel]))
getOrderBook token (InstrumentId figi) depth = do
  manager <- newManager tlsManagerSettings
  let url = tbankProductionUrl <> "tinkoff.public.invest.api.contract.v1.MarketDataService/GetOrderBook"
      body = RequestBodyLBS $ encode $ object
        [ "instrumentId" .= figi
        , "depth" .= depth
        ]
  
  initReq <- parseRequest url
  let request = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", "Bearer " <> encodeUtf8 token)
            , ("Content-Type", "application/json")
            ]
        , requestBody = body
        }
  
  result <- try @SomeException $ httpLbs request manager
  
  case result of
    Left e -> return $ Left $ Sandbox.TBankNetworkError (Text.pack $ show (e :: SomeException))
    Right response -> do
      let bodyBytes = BL.toStrict $ responseBody response
      case statusCode $ responseStatus response of
        200 -> case eitherDecodeStrict bodyBytes of
          Left err -> return $ Left $ Sandbox.TBankParseError (Text.pack err)
          Right orderBook -> do
            let bids = map entryToPriceLevel (obrBids orderBook)
                asks = map entryToPriceLevel (obrAsks orderBook)
            return $ Right (bids, asks)
        code -> return $ Left $ Sandbox.TBankHttpError code (Text.pack $ "HTTP " <> show code)
  where
    entryToPriceLevel OrderBookEntry{..} = PriceLevel
      { priceLevelPrice = Sandbox.quotationToScientific obePrice
      , priceLevelSize = fromInteger obeQuantity
      }

-- ============================================================================
-- Trading Status
-- ============================================================================

data TradingStatusResponse = TradingStatusResponse
  { tsrFigi :: Text
  , tsrTradingStatus :: Text
  } deriving (Show, Eq)

instance FromJSON TradingStatusResponse where
  parseJSON = withObject "TradingStatusResponse" $ \v -> TradingStatusResponse
    <$> v .: "figi"
    <*> v .: "tradingStatus"

getTradingStatus :: Text -> InstrumentId -> IO (Either Sandbox.TBankError TradingStatus)
getTradingStatus token (InstrumentId figi) = do
  manager <- newManager tlsManagerSettings
  let url = tbankProductionUrl <> "tinkoff.public.invest.api.contract.v1.MarketDataService/GetTradingStatus"
      body = RequestBodyLBS $ encode $ object ["instrumentId" .= figi]
  
  initReq <- parseRequest url
  let request = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", "Bearer " <> encodeUtf8 token)
            , ("Content-Type", "application/json")
            ]
        , requestBody = body
        }
  
  result <- try @SomeException $ httpLbs request manager
  
  case result of
    Left e -> return $ Left $ Sandbox.TBankNetworkError (Text.pack $ show (e :: SomeException))
    Right response -> do
      let bodyBytes = BL.toStrict $ responseBody response
      case statusCode $ responseStatus response of
        200 -> case eitherDecodeStrict bodyBytes of
          Left err -> return $ Left $ Sandbox.TBankParseError (Text.pack err)
          Right statusResp -> return $ Right $ textToTradingStatus (tsrTradingStatus statusResp)
        code -> return $ Left $ Sandbox.TBankHttpError code (Text.pack $ "HTTP " <> show code)
  where
    textToTradingStatus txt
      | txt == "SECURITY_TRADING_STATUS_OPENING_PERIOD" = OpeningAuction
      | txt == "SECURITY_TRADING_STATUS_DEALER_NORMAL_TRADING" = Trading
      | txt == "SECURITY_TRADING_STATUS_CLOSING_AUCTION" = ClosingAuction
      | txt == "SECURITY_TRADING_STATUS_BREAK_IN_TRADING" = Break
      | txt == "SECURITY_TRADING_STATUS_NOT_AVAILABLE_FOR_TRADING" = NotAvailable
      | txt == "SECURITY_TRADING_STATUS_CLOSED_PERIOD" = Closed
      | otherwise = NotAvailable

-- ============================================================================
-- Trading Schedules
-- ============================================================================

data TradingSchedulesResponse = TradingSchedulesResponse
  { tsrExchanges :: [ExchangeSchedule]
  } deriving (Show, Eq)

data ExchangeSchedule = ExchangeSchedule
  { esExchange :: Text
  , esDays :: [TradingDay]
  } deriving (Show, Eq)

data TradingDay = TradingDay
  { tdDate :: Day
  , tdIsTradingDay :: Bool
  , tdStartTime :: Maybe TimeOfDay
  , tdEndTime :: Maybe TimeOfDay
  } deriving (Show, Eq)

instance FromJSON TradingSchedulesResponse where
  parseJSON = withObject "TradingSchedulesResponse" $ \v -> TradingSchedulesResponse
    <$> v .: "exchanges"

instance FromJSON ExchangeSchedule where
  parseJSON = withObject "ExchangeSchedule" $ \v -> ExchangeSchedule
    <$> v .: "exchange"
    <*> v .: "days"

instance FromJSON TradingDay where
  parseJSON = withObject "TradingDay" $ \v -> TradingDay
    <$> v .: "date"
    <*> v .: "isTradingDay"
    <*> v .:? "startTime"
    <*> v .:? "endTime"

getTradingSchedules :: Text -> Text -> IO (Either Sandbox.TBankError [TradingSession])
getTradingSchedules token exchange = do
  manager <- newManager tlsManagerSettings
  let url = tbankProductionUrl <> "tinkoff.public.invest.api.contract.v1.InstrumentsService/TradingSchedules"
      body = RequestBodyLBS $ encode $ object
        [ "exchange" .= exchange
        , "from" .= ("2024-01-01" :: Text)  -- TODO: Use actual date range
        , "to" .= ("2024-12-31" :: Text)
        ]
  
  initReq <- parseRequest url
  let request = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Authorization", "Bearer " <> encodeUtf8 token)
            , ("Content-Type", "application/json")
            ]
        , requestBody = body
        }
  
  result <- try @SomeException $ httpLbs request manager
  
  case result of
    Left e -> return $ Left $ Sandbox.TBankNetworkError (Text.pack $ show (e :: SomeException))
    Right response -> do
      let bodyBytes = BL.toStrict $ responseBody response
      case statusCode $ responseStatus response of
        200 -> case eitherDecodeStrict bodyBytes of
          Left err -> return $ Left $ Sandbox.TBankParseError (Text.pack err)
          Right schedulesResp -> return $ Right $ map exchangeToSession (tsrExchanges schedulesResp)
        code -> return $ Left $ Sandbox.TBankHttpError code (Text.pack $ "HTTP " <> show code)
  where
    exchangeToSession ExchangeSchedule{..} = TradingSession
      { tsExchange = esExchange
      , tsInstrumentType = "all"  -- TODO: Map properly
      , tsDays = map dayToDaySession esDays
      }
    
    dayToDaySession TradingDay{..} = DaySession
      { dsDate = tdDate
      , dsIsTradingDay = tdIsTradingDay
      , dsSessions = case (tdStartTime, tdEndTime) of
          (Just start, Just end) -> [SessionSegment start end]
          _ -> []
      }

-- ============================================================================
-- Liquidity Assessment
-- ============================================================================

-- | Check liquidity before placing an order
-- CRITICAL for MOEX due to thin liquidity on many instruments
checkLiquidity :: Text -> InstrumentId -> Quantity -> Side -> Scientific -> IO LiquidityCheckResult
checkLiquidity token instId qty side threshold = do
  -- Step 1: Get order book (get 20 levels for depth)
  orderBookResult <- getOrderBook token instId 20
  
  case orderBookResult of
    Left err -> do
      putStrLn $ "Liquidity check failed: " ++ show err
      -- Return critical if we can't check
      currentTime <- getCurrentTime
      return $ LiquidityCritical $ LiquidityAssessment
        { laInstrumentId = instId
        , laTimestamp = currentTime
        , laBidVolume = 0
        , laAskVolume = 0
        , laSpreadPercent = 0
        , laSlippageEstimate = SlippageEstimate
            { seForQuantity = qty
            , seExpectedSlippage = 100  -- 100% worst case
            , seMaxSlippage = 100
            , seConfidence = UnknownConfidence
            }
        , laIsLiquid = False
        }
    
    Right (bids, asks) -> do
      currentTime <- getCurrentTime
      
      -- Step 2: Calculate available volume
      let totalBidVolume = sum $ map priceLevelSize bids
          totalAskVolume = sum $ map priceLevelSize asks
          (relevantLevels, _oppositeLevels) = case side of
            Buy -> (asks, bids)
            Sell -> (bids, asks)
      
      -- Step 3: Estimate slippage
      let expectedSlippage = estimateSlippage qty relevantLevels
          maxSlippage = estimateMaxSlippage qty relevantLevels
          confidence = assessConfidence relevantLevels
          spread = calculateSpread bids asks
      
      -- Step 4: Build assessment
      let assessment = LiquidityAssessment
            { laInstrumentId = instId
            , laTimestamp = currentTime
            , laBidVolume = totalBidVolume
            , laAskVolume = totalAskVolume
            , laSpreadPercent = spread
            , laSlippageEstimate = SlippageEstimate
                { seForQuantity = qty
                , seExpectedSlippage = expectedSlippage
                , seMaxSlippage = maxSlippage
                , seConfidence = confidence
                }
            , laIsLiquid = expectedSlippage < threshold
            }
      
      -- Step 5: Determine result
      return $ classifyLiquidity assessment threshold

-- | Classify liquidity check result
classifyLiquidity :: LiquidityAssessment -> Scientific -> LiquidityCheckResult
classifyLiquidity assessment threshold
  | expectedSlippage < threshold * 0.5 = LiquidityOK assessment
  | expectedSlippage < threshold = LiquidityWarning assessment warning
  | otherwise = LiquidityCritical assessment
  where
    expectedSlippage = seExpectedSlippage (laSlippageEstimate assessment)
    warning = SlippageWarning
      { swExpectedSlippage = expectedSlippage
      , swThreshold = threshold
      , swMessage = Text.pack $ "Expected slippage " ++ show expectedSlippage ++ "% exceeds threshold " ++ show threshold ++ "%"
      }

-- | Estimate expected slippage for a given order size
estimateSlippage :: Quantity -> [PriceLevel] -> Scientific
estimateSlippage targetQty levels = 
  let walkBook _ [] = 0.0
      walkBook remaining (level:rest)
        | remaining <= 0 = 0.0
        | priceLevelSize level >= remaining = 
            -- Order fills within this level
            let ratio = remaining / priceLevelSize level
                bestPrice = priceLevelPrice (head levels)
            in ratio * (priceLevelPrice level - bestPrice) / bestPrice * 100
        | otherwise = 
            -- Consume entire level and continue
            walkBook (remaining - priceLevelSize level) rest
        where
          bestPrice = priceLevelPrice (head levels)
  in walkBook targetQty levels

-- | Estimate worst-case slippage
estimateMaxSlippage :: Quantity -> [PriceLevel] -> Scientific
estimateMaxSlippage targetQty levels =
  case dropWhile ((< targetQty) . priceLevelSize) levels of
    (lastLevel:_) -> priceLevelPrice lastLevel
    [] -> case reverse levels of
            (lastLevel:_) -> priceLevelPrice lastLevel
            [] -> 0

-- | Assess confidence level based on order book depth
assessConfidence :: [PriceLevel] -> SlippageConfidence
assessConfidence levels
  | length levels >= 10 = HighConfidence
  | length levels >= 5 = MediumConfidence
  | length levels >= 2 = LowConfidence
  | otherwise = UnknownConfidence

-- | Calculate bid-ask spread as percentage
calculateSpread :: [PriceLevel] -> [PriceLevel] -> Scientific
calculateSpread bids asks =
  case (bids, asks) of
    ((bestBid:_), (bestAsk:_)) ->
      let spread = priceLevelPrice bestAsk - priceLevelPrice bestBid
          mid = (priceLevelPrice bestBid + priceLevelPrice bestAsk) / 2
      in spread / mid * 100
    _ -> 0
