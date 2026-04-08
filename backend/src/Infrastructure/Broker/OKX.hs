{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Infrastructure.Broker.OKX
  ( runOKXBroker
  , placeOrderOKX
  , cancelOrderOKX
  , getOrderBookOKX
  , getInstrumentsOKX
  , OKXError (..)
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (forM, void)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Scientific (Scientific, toRealFloat, scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Domain.Broker (BrokerConfig (..), PriceLevel (..))
import Domain.Order (CancelRequest (..), OrderRequest (..), OrderResponse (..), OrderType (..))
import Domain.Types (InstrumentId (..), OrderId (..), OrderStatus (..), Quantity, Side (..))
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- ============================================================================
-- OKX Error Types
-- ============================================================================

data OKXError
  = OKXHttpError Int Text
  | OKXParseError Text
  | OKXNetworkError Text
  deriving stock (Eq, Show)

-- ============================================================================
-- OKX API Types
-- ============================================================================

data OKXResponse a = OKXResponse
  { okxCode :: Text
  , okxMsg :: Text
  , okxData :: [a]
  } deriving stock (Eq, Show, Generic)

instance FromJSON a => FromJSON (OKXResponse a) where
  parseJSON = withObject "OKXResponse" $ \v -> OKXResponse
    <$> v .: "code"
    <*> v .: "msg"
    <*> v .: "data"

data OKXInstrument = OKXInstrument
  { instId :: Text
  , instFamily :: Text
  , optType :: Text
  , stk :: Text
  , expTime :: Text
  , tickSz :: Text
  , minSz :: Text
  , state :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON OKXInstrument where
  parseJSON = withObject "OKXInstrument" $ \v -> OKXInstrument
    <$> v .: "instId"
    <*> v .: "instFamily"
    <*> v .: "optType"
    <*> v .: "stk"
    <*> v .: "expTime"
    <*> v .: "tickSz"
    <*> v .: "minSz"
    <*> v .: "state"

data OKXTicker = OKXTicker
  { tickerInstId :: Text
  , bidPx :: Maybe Text
  , askPx :: Maybe Text
  , last :: Maybe Text
  , vol24h :: Maybe Text
  , ts :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON OKXTicker where
  parseJSON = withObject "OKXTicker" $ \v -> OKXTicker
    <$> v .: "instId"
    <*> v .:? "bidPx"
    <*> v .:? "askPx"
    <*> v .:? "last"
    <*> v .:? "vol24h"
    <*> v .: "ts"

-- ============================================================================
-- Broker Interpreter
-- ============================================================================

-- | Run broker operations against OKX API
-- This is the production interpreter for OKX operations
runOKXBroker :: BrokerConfig -> IO ()
runOKXBroker config = do
  putStrLn "OKX Broker interpreter initialized"
  -- This function is a placeholder for initialization logic
  -- Actual operations are handled by individual functions below

-- ============================================================================
-- Order Operations
-- ============================================================================

placeOrderOKX :: BrokerConfig -> OrderRequest -> IO (Either OKXError OrderResponse)
placeOrderOKX config@(OKXConfig _ _ _ _) req = do
  manager <- newManager tlsManagerSettings
  let baseUrl = "https://www.okx.com"
      url = Text.unpack $ baseUrl <> "/api/v5/trade/order"
  
  -- TODO: Implement proper HMAC-SHA256 authentication
  -- TODO: Construct proper order request body
  -- For now, return a placeholder
  currentTime <- getCurrentTime
  return $ Right $ OrderResponse
    { orderResponseOrderId = OrderId "okx-placeholder"
    , orderResponseClientOrderId = Nothing
    , orderResponseStatus = OrderPending
    , orderResponseFilledQty = 0
    , orderResponseAvgPrice = Nothing
    , orderResponseTimestamp = currentTime
    }
placeOrderOKX _ _ = return $ Left $ OKXHttpError 400 "Expected OKXConfig, got TBankConfig"

cancelOrderOKX :: BrokerConfig -> CancelRequest -> IO (Either OKXError Bool)
cancelOrderOKX config@(OKXConfig _ _ _ _) req = do
  manager <- newManager tlsManagerSettings
  let baseUrl = "https://www.okx.com"
      url = Text.unpack $ baseUrl <> "/api/v5/trade/cancel-order"
  
  -- TODO: Implement proper cancellation
  return $ Right True
cancelOrderOKX _ _ = return $ Left $ OKXHttpError 400 "Expected OKXConfig, got TBankConfig"

-- ============================================================================
-- Market Data Operations
-- ============================================================================

getOrderBookOKX :: BrokerConfig -> InstrumentId -> Int -> IO (Either OKXError (Maybe ([PriceLevel], [PriceLevel])))
getOrderBookOKX config (InstrumentId instId) depth = do
  manager <- newManager tlsManagerSettings
  let baseUrl = "https://www.okx.com"
      url = Text.unpack $ baseUrl <> "/api/v5/market/books?instId=" <> instId <> "&sz=" <> Text.pack (show depth)
  
  request <- parseRequest url
  result <- try @SomeException $ httpLbs request manager
  
  case result of
    Left e -> return $ Left $ OKXNetworkError (Text.pack $ show (e :: SomeException))
    Right response -> do
      case statusCode $ responseStatus response of
        200 -> do
          let body = BL.toStrict $ responseBody response
          -- TODO: Parse order book response
          return $ Right Nothing
        code -> return $ Left $ OKXHttpError code "Failed to fetch order book"

getInstrumentsOKX :: BrokerConfig -> Text -> IO (Either OKXError [OKXInstrument])
getInstrumentsOKX config underlying = do
  manager <- newManager tlsManagerSettings
  let baseUrl = "https://www.okx.com"
      url = Text.unpack $ baseUrl <> "/api/v5/public/instruments?instType=OPTION&uly=" <> underlying
  
  request <- parseRequest url
  result <- try @SomeException $ httpLbs request manager
  
  case result of
    Left e -> return $ Left $ OKXNetworkError (Text.pack $ show (e :: SomeException))
    Right response -> do
      case statusCode $ responseStatus response of
        200 -> do
          let body = BL.toStrict $ responseBody response
          case eitherDecodeStrict body of
            Left err -> return $ Left $ OKXParseError (Text.pack err)
            Right (OKXResponse {..}) -> 
              if okxCode == "0"
                then return $ Right okxData
                else return $ Left $ OKXHttpError 200 okxMsg
        code -> return $ Left $ OKXHttpError code "Failed to fetch instruments"

-- ============================================================================
-- Helpers
-- ============================================================================

parseScientific :: Text -> Maybe Scientific
parseScientific txt = 
  case reads (Text.unpack txt) of
    [(n, "")] -> Just n
    _ -> Nothing
