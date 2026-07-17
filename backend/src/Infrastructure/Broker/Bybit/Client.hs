{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Infrastructure.Broker.Bybit.Client
  ( BybitError (..)
  , BybitInstrument (..)
  , BybitTicker (..)
  , BybitOrderResult (..)
  , supportedOptionBaseCoins
  , fetchOptionInstruments
  , fetchOptionTickers
  , fetchIndexPrice
  , placeOptionOrder
  , cancelOptionOrder
  , getOptionPositions
  ) where

import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Domain.Broker (BrokerConfig (..))
import Domain.Order (OrderRequest (..), OrderType (..))
import Domain.Types (InstrumentId (..), OrderId (..), Side (..))
import GHC.Generics (Generic)
import Infrastructure.Broker.Bybit.Auth
  ( bybitMainnetUrl
  , bybitTestnetUrl
  , currentTimestampMs
  , recvWindow
  , signRequest
  )
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
-- ============================================================================
-- Errors / constants
-- ============================================================================

data BybitError
  = BybitHttpError Int Text
  | BybitParseError Text
  | BybitNetworkError Text
  | BybitApiError Text Text  -- retCode retMsg
  | BybitConfigError Text
  deriving stock (Eq, Show)

supportedOptionBaseCoins :: [Text]
supportedOptionBaseCoins = ["BTC", "SOL", "XAUT", "XRP", "MNT", "DOGE"]

baseUrl :: BrokerConfig -> Either BybitError Text
baseUrl BybitConfig{bybitTestnet = True} = Right bybitTestnetUrl
baseUrl BybitConfig{bybitTestnet = False} = Right bybitMainnetUrl
baseUrl _ = Left $ BybitConfigError "Expected BybitConfig"

-- ============================================================================
-- Response envelope
-- ============================================================================

data BybitResponse a = BybitResponse
  { brRetCode :: Int
  , brRetMsg :: Text
  , brResult :: a
  } deriving stock (Eq, Show, Generic)

instance FromJSON a => FromJSON (BybitResponse a) where
  parseJSON = withObject "BybitResponse" $ \v -> BybitResponse
    <$> v .: "retCode"
    <*> v .: "retMsg"
    <*> v .: "result"

data BybitList a = BybitList
  { blList :: [a]
  , blNextPageCursor :: Maybe Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON a => FromJSON (BybitList a) where
  parseJSON = withObject "BybitList" $ \v -> BybitList
    <$> v .:? "list" .!= []
    <*> v .:? "nextPageCursor"

-- ============================================================================
-- Instruments
-- ============================================================================

data BybitInstrument = BybitInstrument
  { biSymbol :: Text
  , biStatus :: Text
  , biBaseCoin :: Text
  , biQuoteCoin :: Text
  , biSettleCoin :: Text
  , biOptionsType :: Text  -- Call / Put
  , biStrike :: Text
  , biDeliveryTime :: Text
  , biTickSize :: Text
  , biMinOrderQty :: Text
  , biQtyStep :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON BybitInstrument where
  parseJSON = withObject "BybitInstrument" $ \v -> do
    lot <- v .: "lotSizeFilter"
    price <- v .: "priceFilter"
    BybitInstrument
      <$> v .: "symbol"
      <*> v .: "status"
      <*> v .: "baseCoin"
      <*> v .: "quoteCoin"
      <*> v .:? "settleCoin" .!= "USDT"
      <*> v .:? "optionsType" .!= ""
      <*> v .:? "strike" .!= "0"
      <*> v .:? "deliveryTime" .!= "0"
      <*> (price .: "tickSize")
      <*> (lot .: "minOrderQty")
      <*> (lot .: "qtyStep")

fetchOptionInstruments :: BrokerConfig -> Text -> IO (Either BybitError [BybitInstrument])
fetchOptionInstruments config baseCoin = go Nothing []
  where
    go mCursor acc = do
      let qs = "category=option&baseCoin=" <> Text.unpack baseCoin
            <> maybe "" (\c -> "&cursor=" <> Text.unpack c) mCursor
            <> "&limit=1000"
      result <- publicGet config ("/v5/market/instruments-info?" <> qs)
      case result of
        Left err -> pure $ Left err
        Right body -> case eitherDecode body of
          Left e -> pure $ Left $ BybitParseError (Text.pack e)
          Right (BybitResponse code msg (BybitList list mNext)) ->
            if code /= 0
              then pure $ Left $ BybitApiError (Text.pack $ show code) msg
              else
                let trading = filter (\i -> biStatus i == "Trading") list
                    acc' = acc ++ trading
                in case mNext of
                     Just c | not (Text.null c) -> go (Just c) acc'
                     _ -> pure $ Right acc'

-- ============================================================================
-- Tickers
-- ============================================================================

data BybitTicker = BybitTicker
  { btSymbol :: Text
  , btLastPrice :: Maybe Text
  , btMarkPrice :: Maybe Text
  , btIndexPrice :: Maybe Text
  , btBid1Price :: Maybe Text
  , btAsk1Price :: Maybe Text
  , btBid1Size :: Maybe Text
  , btAsk1Size :: Maybe Text
  , btVolume24h :: Maybe Text
  , btOpenInterest :: Maybe Text
  , btDelta :: Maybe Text
  , btGamma :: Maybe Text
  , btVega :: Maybe Text
  , btTheta :: Maybe Text
  , btMarkIv :: Maybe Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON BybitTicker where
  parseJSON = withObject "BybitTicker" $ \v -> BybitTicker
    <$> v .: "symbol"
    <*> v .:? "lastPrice"
    <*> v .:? "markPrice"
    <*> v .:? "indexPrice"
    <*> v .:? "bid1Price"
    <*> v .:? "ask1Price"
    <*> v .:? "bid1Size"
    <*> v .:? "ask1Size"
    <*> v .:? "volume24h"
    <*> v .:? "openInterest"
    <*> v .:? "delta"
    <*> v .:? "gamma"
    <*> v .:? "vega"
    <*> v .:? "theta"
    <*> v .:? "markIv"

fetchOptionTickers :: BrokerConfig -> Text -> IO (Either BybitError [BybitTicker])
fetchOptionTickers config baseCoin = do
  let qs = "category=option&baseCoin=" <> Text.unpack baseCoin
  result <- publicGet config ("/v5/market/tickers?" <> qs)
  case result of
    Left err -> pure $ Left err
    Right body -> case eitherDecode body of
      Left e -> pure $ Left $ BybitParseError (Text.pack e)
      Right (BybitResponse code msg (BybitList list _)) ->
        if code /= 0
          then pure $ Left $ BybitApiError (Text.pack $ show code) msg
          else pure $ Right list

fetchIndexPrice :: BrokerConfig -> Text -> IO (Either BybitError Scientific)
fetchIndexPrice config baseCoin = do
  tickers <- fetchOptionTickers config baseCoin
  pure $ case tickers of
    Left err -> Left err
    Right [] -> Left $ BybitParseError "No tickers for index price"
    Right (t:_) -> case btIndexPrice t <|> btMarkPrice t of
      Just p -> case reads (Text.unpack p) of
        [(n, _)] -> Right n
        _ -> Left $ BybitParseError $ "Bad index price: " <> fromMaybe "" (btIndexPrice t)
      Nothing -> Left $ BybitParseError "Missing index price"

-- ============================================================================
-- Orders
-- ============================================================================

data BybitOrderResult = BybitOrderResult
  { borOrderId :: Text
  , borOrderLinkId :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON BybitOrderResult where
  parseJSON = withObject "BybitOrderResult" $ \v -> BybitOrderResult
    <$> v .: "orderId"
    <*> v .:? "orderLinkId" .!= ""

placeOptionOrder
  :: BrokerConfig
  -> OrderRequest
  -> Text  -- orderLinkId (required for options)
  -> Bool  -- reduceOnly
  -> IO (Either BybitError BybitOrderResult)
placeOptionOrder config@BybitConfig{..} req orderLinkId reduceOnly = do
  let InstrumentId symbol = orderRequestInstrumentId req
      side = case orderRequestSide req of
        Buy -> "Buy"
        Sell -> "Sell"
      orderType = case orderRequestOrderType req of
        MarketOrder -> "Market"
        ImmediateOrCancel -> "Market"
        _ -> "Limit"
      qty = Text.pack $ show (orderRequestQuantity req)
      priceField = case orderRequestPrice req of
        Just p | orderType == "Limit" -> ["price" .= Text.pack (show p)]
        _ -> []
      bodyObj = object $
        [ "category" .= ("option" :: Text)
        , "symbol" .= symbol
        , "side" .= (side :: Text)
        , "orderType" .= (orderType :: Text)
        , "qty" .= qty
        , "orderLinkId" .= orderLinkId
        , "reduceOnly" .= reduceOnly
        , "timeInForce" .= if orderType == "Market" then ("IOC" :: Text) else ("GTC" :: Text)
        ] ++ priceField
      body = decodeUtf8 $ BL.toStrict $ encode bodyObj
  privatePost config "/v5/order/create" body
placeOptionOrder _ _ _ _ = pure $ Left $ BybitConfigError "Expected BybitConfig"

cancelOptionOrder :: BrokerConfig -> OrderId -> Text -> IO (Either BybitError Bool)
cancelOptionOrder config@BybitConfig{..} (OrderId oid) symbol = do
  let bodyObj = object
        [ "category" .= ("option" :: Text)
        , "symbol" .= symbol
        , "orderId" .= oid
        ]
      body = decodeUtf8 $ BL.toStrict $ encode bodyObj
  result <- privatePost config "/v5/order/cancel" body
  pure $ case result of
    Left err -> Left err
    Right _ -> Right True
cancelOptionOrder _ _ _ = pure $ Left $ BybitConfigError "Expected BybitConfig"

getOptionPositions :: BrokerConfig -> Text -> IO (Either BybitError [Value])
getOptionPositions config baseCoin = do
  let qs = "category=option&baseCoin=" <> Text.unpack baseCoin
  result <- privateGet config ("/v5/position/list?" <> qs)
  case result of
    Left err -> pure $ Left err
    Right body -> case eitherDecode body of
      Left e -> pure $ Left $ BybitParseError (Text.pack e)
      Right (BybitResponse code msg (BybitList list _)) ->
        if code /= 0
          then pure $ Left $ BybitApiError (Text.pack $ show code) msg
          else pure $ Right list

-- ============================================================================
-- HTTP helpers
-- ============================================================================

publicGet :: BrokerConfig -> String -> IO (Either BybitError BL.ByteString)
publicGet config path = case baseUrl config of
  Left err -> pure $ Left err
  Right url -> do
    manager <- newManager tlsManagerSettings
    let fullUrl = Text.unpack url <> path
    request <- parseRequest fullUrl
    eresult <- try $ httpLbs request manager
    pure $ case eresult of
      Left (e :: SomeException) -> Left $ BybitNetworkError (Text.pack $ show e)
      Right response ->
        let code = statusCode (responseStatus response)
        in if code >= 200 && code < 300
             then Right (responseBody response)
             else Left $ BybitHttpError code (decodeUtf8 $ BL.toStrict $ responseBody response)

privatePost :: BrokerConfig -> String -> Text -> IO (Either BybitError BybitOrderResult)
privatePost config@BybitConfig{..} path body = do
  result <- privateRequest config "POST" path body
  case result of
    Left err -> pure $ Left err
    Right raw -> case eitherDecode raw of
      Left e -> pure $ Left $ BybitParseError (Text.pack e)
      Right (BybitResponse code msg res) ->
        if code /= 0
          then pure $ Left $ BybitApiError (Text.pack $ show code) msg
          else pure $ Right res
privatePost _ _ _ = pure $ Left $ BybitConfigError "Expected BybitConfig"

privateGet :: BrokerConfig -> String -> IO (Either BybitError BL.ByteString)
privateGet config path =
  -- path includes query string; payload for sign is the query without '?'
  let payload = case break (== '?') path of
        (_, '?':qs) -> Text.pack qs
        _ -> ""
  in privateRequestRaw config "GET" path payload

privateRequest :: BrokerConfig -> String -> String -> Text -> IO (Either BybitError BL.ByteString)
privateRequest = privateRequestRaw

privateRequestRaw :: BrokerConfig -> String -> String -> Text -> IO (Either BybitError BL.ByteString)
privateRequestRaw config@BybitConfig{..} method path payload = case baseUrl config of
  Left err -> pure $ Left err
  Right url -> do
    manager <- newManager tlsManagerSettings
    ts <- currentTimestampMs
    let signature = signRequest bybitApiSecret ts bybitApiKey payload
        fullUrl = Text.unpack url <> path
    baseReq <- parseRequest fullUrl
    let req = baseReq
          { method = B8.pack method
          , requestHeaders =
              [ ("X-BAPI-API-KEY", encodeUtf8 bybitApiKey)
              , ("X-BAPI-SIGN", encodeUtf8 signature)
              , ("X-BAPI-TIMESTAMP", encodeUtf8 ts)
              , ("X-BAPI-RECV-WINDOW", encodeUtf8 recvWindow)
              , ("Content-Type", "application/json")
              ]
          , requestBody = if method == "POST" then RequestBodyBS (encodeUtf8 payload) else ""
          }
    eresult <- try $ httpLbs req manager
    pure $ case eresult of
      Left (e :: SomeException) -> Left $ BybitNetworkError (Text.pack $ show e)
      Right response ->
        let code = statusCode (responseStatus response)
        in if code >= 200 && code < 300
             then Right (responseBody response)
             else Left $ BybitHttpError code (decodeUtf8 $ BL.toStrict $ responseBody response)
privateRequestRaw _ _ _ _ = pure $ Left $ BybitConfigError "Expected BybitConfig"

-- Fix missing import for currentTimestampMs - it's in Auth but not exported as used in Client
-- Re-export usage: need to export currentTimestampMs from Auth - already used above

(<|>) :: Maybe a -> Maybe a -> Maybe a
(<|>) (Just x) _ = Just x
(<|>) Nothing y = y
