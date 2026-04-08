{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Infrastructure.Broker.TBank.Sandbox
  ( -- * Sandbox Account Management
    openSandboxAccount
  , closeSandboxAccount
  , getSandboxAccounts
  , SandboxAccount (..)
    -- * Sandbox Funding
  , sandboxPayIn
  , MoneyValue (..)
  , Currency (..)
    -- * Sandbox Orders
  , postSandboxOrder
  , cancelSandboxOrder
  , getSandboxOrderState
  , getSandboxOrders
  , SandboxOrderResponse (..)
    -- * Sandbox Portfolio
  , getSandboxPortfolio
  , getSandboxPositions
  , PortfolioPosition (..)
    -- * Utility Types
  , Quotation (..)
  , quotationToScientific
  , scientificToQuotation
    -- * Order Conversion Helpers
  , OrderDirection (..)
  , TBankOrderType (..)
  , mapSide
  , mapOrderType
  , mapExecutionStatus
  , moneyValueToScientific
  , sandboxOrderResponseToOrderResponse
    -- * HTTP Helpers
  , buildAuthRequest
    -- * Types
  , TBankError (..)
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Domain.Broker (PriceLevel (..), LiquidityAssessment (..))
import Domain.Order (OrderRequest (..), OrderResponse (..), OrderType (..))
import Domain.Types (InstrumentId (..), OrderId (..), OrderStatus (..), Quantity, Side (..))
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hAuthorization, hContentType)
import Network.HTTP.Types.Status (statusCode)

-- ============================================================================
-- Configuration
-- ============================================================================

tbankSandboxBaseUrl :: String
tbankSandboxBaseUrl = "https://invest-public-api.tbank.ru/rest/"

tbankSandboxService :: String -> String
tbankSandboxService method = tbankSandboxBaseUrl <> "tinkoff.public.invest.api.contract.v1.SandboxService/" <> method

-- ============================================================================
-- Error Types
-- ============================================================================

data TBankError
  = TBankHttpError Int Text
  | TBankParseError Text
  | TBankNetworkError Text
  | TBankAuthError Text
  | TBankApiError Text (Maybe Text)  -- message + tracking ID
  | TBankTradingClosed
  | TBankInsufficientLiquidity LiquidityAssessment
  deriving stock (Eq, Show)

-- ============================================================================
-- Currency Types
-- ============================================================================

data Currency = RUB | USD | EUR
  deriving stock (Eq, Show, Generic)

currencyToText :: Currency -> Text
currencyToText RUB = "RUB"
currencyToText USD = "USD"
currencyToText EUR = "EUR"

instance ToJSON Currency where
  toJSON = String . currencyToText

-- ============================================================================
-- Money Value
-- ============================================================================

-- | T-Bank represents money as units + nano (9 decimal places)
data MoneyValue = MoneyValue
  { mvCurrency :: Currency
  , mvUnits :: Integer
  , mvNano :: Int
  } deriving stock (Eq, Show, Generic)

instance ToJSON MoneyValue where
  toJSON (MoneyValue currency units nano) = object
    [ "currency" .= currencyToText currency
    , "units" .= Text.pack (show units)
    , "nano" .= nano
    ]

instance FromJSON MoneyValue where
  parseJSON = withObject "MoneyValue" $ \v -> do
    currencyStr <- v .: "currency"
    currency <- case currencyStr of
      "RUB" -> return RUB
      "USD" -> return USD
      "EUR" -> return EUR
      _ -> fail $ "Unknown currency: " <> Text.unpack currencyStr
    units <- read <$> v .: "units"
    nano <- v .: "nano"
    return $ MoneyValue currency units nano

moneyValueToScientific :: MoneyValue -> Scientific
moneyValueToScientific (MoneyValue _ units nano) =
  fromInteger units + fromIntegral nano / 1000000000

scientificToMoneyValue :: Currency -> Scientific -> MoneyValue
scientificToMoneyValue currency val =
  let units = truncate val
      nano = truncate ((val - fromInteger units) * 1000000000)
  in MoneyValue currency units nano

-- ============================================================================
-- Quotation (for prices)
-- ============================================================================

data Quotation = Quotation
  { qUnits :: Integer
  , qNano :: Int
  } deriving stock (Eq, Show, Generic)

instance ToJSON Quotation where
  toJSON (Quotation units nano) = object
    [ "units" .= Text.pack (show units)
    , "nano" .= nano
    ]

instance FromJSON Quotation where
  parseJSON = withObject "Quotation" $ \v -> do
    unitsStr <- v .: "units"
    units <- case reads (Text.unpack unitsStr) of
      [(n, "")] -> return n
      _ -> fail $ "Invalid units: " <> Text.unpack unitsStr
    nano <- v .: "nano"
    return $ Quotation units nano

quotationToScientific :: Quotation -> Scientific
quotationToScientific (Quotation units nano) =
  fromInteger units + fromIntegral nano / 1000000000

scientificToQuotation :: Scientific -> Quotation
scientificToQuotation val =
  let units = truncate val
      nano = truncate ((val - fromInteger units) * 1000000000)
  in Quotation units nano

-- ============================================================================
-- Sandbox Account
-- ============================================================================

data SandboxAccount = SandboxAccount
  { saAccountId :: Text
  , saName :: Maybe Text
  , saOpenedDate :: Maybe UTCTime
  } deriving stock (Eq, Show, Generic)

instance FromJSON SandboxAccount where
  parseJSON = withObject "SandboxAccount" $ \v -> SandboxAccount
    <$> v .: "accountId"
    <*> v .:? "name"
    <*> v .:? "openedDate"

-- ============================================================================
-- Sandbox Order Types
-- ============================================================================

data OrderDirection = BuyDir | SellDir
  deriving stock (Eq, Show, Generic)

orderDirectionToText :: OrderDirection -> Text
orderDirectionToText BuyDir = "ORDER_DIRECTION_BUY"
orderDirectionToText SellDir = "ORDER_DIRECTION_SELL"

instance ToJSON OrderDirection where
  toJSON = String . orderDirectionToText

data TBankOrderType = LimitOrderTB | MarketOrderTB | BestPriceTB
  deriving stock (Eq, Show, Generic)

orderTypeToText :: TBankOrderType -> Text
orderTypeToText LimitOrderTB = "ORDER_TYPE_LIMIT"
orderTypeToText MarketOrderTB = "ORDER_TYPE_MARKET"
orderTypeToText BestPriceTB = "ORDER_TYPE_BESTPRICE"

instance ToJSON TBankOrderType where
  toJSON = String . orderTypeToText

-- | Map our OrderType to T-Bank's OrderType
mapOrderType :: OrderType -> TBankOrderType
mapOrderType LimitOrder = LimitOrderTB
mapOrderType MarketOrder = MarketOrderTB
mapOrderType _ = LimitOrderTB  -- Default to limit for unsupported types

-- | Map our Side to T-Bank's OrderDirection
mapSide :: Side -> OrderDirection
mapSide Buy = BuyDir
mapSide Sell = SellDir

-- ============================================================================
-- Sandbox Order Response
-- ============================================================================

data SandboxOrderResponse = SandboxOrderResponse
  { sorOrderId :: Text
  , sorExecutionReportStatus :: Text
  , sorLotsRequested :: Integer
  , sorLotsExecuted :: Integer
  , sorInitialOrderPrice :: Maybe MoneyValue
  , sorExecutedOrderPrice :: Maybe MoneyValue
  , sorTotalOrderAmount :: Maybe MoneyValue
  , sorInitialCommission :: Maybe MoneyValue
  , sorExecutedCommission :: Maybe MoneyValue
  , sorDirection :: Text
  , sorInstrumentUid :: Maybe Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON SandboxOrderResponse where
  parseJSON = withObject "SandboxOrderResponse" $ \v -> SandboxOrderResponse
    <$> v .: "orderId"
    <*> v .: "executionReportStatus"
    <*> v .: "lotsRequested"
    <*> v .: "lotsExecuted"
    <*> v .:? "initialOrderPrice"
    <*> v .:? "executedOrderPrice"
    <*> v .:? "totalOrderAmount"
    <*> v .:? "initialCommission"
    <*> v .:? "executedCommission"
    <*> v .: "direction"
    <*> v .:? "instrumentUid"

-- | Convert T-Bank order response to our domain OrderResponse
sandboxOrderResponseToOrderResponse :: SandboxOrderResponse -> IO OrderResponse
sandboxOrderResponseToOrderResponse sor = do
  now <- getCurrentTime
  return $ OrderResponse
    { orderResponseOrderId = OrderId (sorOrderId sor)
    , orderResponseClientOrderId = Nothing
    , orderResponseStatus = mapExecutionStatus (sorExecutionReportStatus sor)
    , orderResponseFilledQty = fromInteger (sorLotsExecuted sor)
    , orderResponseAvgPrice = fmap moneyValueToScientific (sorExecutedOrderPrice sor)
    , orderResponseTimestamp = now
    }

mapExecutionStatus :: Text -> OrderStatus
mapExecutionStatus status
  | status == "EXECUTION_REPORT_STATUS_FILL" = OrderClosed
  | status == "EXECUTION_REPORT_STATUS_PARTIALLYFILL" = OrderActive
  | status == "EXECUTION_REPORT_STATUS_NEW" = OrderActive
  | status == "EXECUTION_REPORT_STATUS_CANCELLED" = OrderCancelled
  | status == "EXECUTION_REPORT_STATUS_REJECTED" = OrderFailed "Order rejected"
  | otherwise = OrderPending

-- ============================================================================
-- Portfolio Types
-- ============================================================================

data PortfolioPosition = PortfolioPosition
  { ppFigi :: Maybe Text
  , ppInstrumentType :: Maybe Text
  , ppQuantity :: Maybe Quotation
  , ppAveragePositionPrice :: Maybe MoneyValue
  , ppExpectedYield :: Maybe Quotation
  , ppCurrentPrice :: Maybe MoneyValue
  } deriving stock (Eq, Show, Generic)

instance FromJSON PortfolioPosition where
  parseJSON = withObject "PortfolioPosition" $ \v -> PortfolioPosition
    <$> v .:? "figi"
    <*> v .:? "instrumentType"
    <*> v .:? "quantity"
    <*> v .:? "averagePositionPrice"
    <*> v .:? "expectedYield"
    <*> v .:? "currentPrice"

-- ============================================================================
-- API Request Builders
-- ============================================================================

-- | Build authenticated request with Bearer token
buildAuthRequest :: Text -> String -> RequestBody -> IO Request
buildAuthRequest token url body = do
  initReq <- parseRequest url
  return $ initReq
    { method = "POST"
    , requestHeaders =
        [ (hAuthorization, "Bearer " <> TE.encodeUtf8 token)
        , (hContentType, "application/json")
        ]
    , requestBody = body
    }

-- ============================================================================
-- Sandbox Account Operations
-- ============================================================================

-- | Open a new sandbox account
-- Returns the account ID which is needed for all other sandbox operations
openSandboxAccount :: Text -> Maybe Text -> IO (Either TBankError Text)
openSandboxAccount token mName = do
  let url = tbankSandboxService "OpenSandboxAccount"
      bodyObj = case mName of
        Just name -> object ["name" .= name]
        Nothing -> object []
      body = RequestBodyLBS $ encode bodyObj
  
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
          Right (Object obj) -> case KM.lookup "accountId" obj of
            Just (String accId) -> return $ Right accId
            _ -> return $ Left $ TBankParseError "Missing accountId in response"
          Right _ -> return $ Left $ TBankParseError "Unexpected response format"
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        403 -> return $ Left $ TBankAuthError "Access forbidden"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)

-- | Close a sandbox account
closeSandboxAccount :: Text -> Text -> IO (Either TBankError ())
closeSandboxAccount token accountId = do
  let url = tbankSandboxService "CloseSandboxAccount"
      body = RequestBodyLBS $ encode $ object ["accountId" .= accountId]
  
  result <- try @SomeException $ do
    request <- buildAuthRequest token url body
    manager <- newManager tlsManagerSettings
    httpLbs request manager
  
  case result of
    Left e -> return $ Left $ TBankNetworkError (Text.pack $ show (e :: SomeException))
    Right response ->
      case statusCode $ responseStatus response of
        200 -> return $ Right ()
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        404 -> return $ Left $ TBankHttpError 404 "Account not found"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)

-- | Get list of sandbox accounts
getSandboxAccounts :: Text -> IO (Either TBankError [SandboxAccount])
getSandboxAccounts token = do
  let url = tbankSandboxService "GetSandboxAccounts"
      body = RequestBodyLBS "{}"
  
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
          Right (Object obj) -> case KM.lookup "accounts" obj of
            Just (Array accounts) -> 
              let parsed = mapMaybe parseAccount (V.toList accounts)
              in return $ Right parsed
            _ -> return $ Right []  -- No accounts
          Right _ -> return $ Left $ TBankParseError "Unexpected response format"
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)
  where
    parseAccount :: Value -> Maybe SandboxAccount
    parseAccount val = case fromJSON val of
      Success a -> Just a
      Error _ -> Nothing

-- ============================================================================
-- Sandbox Funding Operations
-- ============================================================================

-- | Add virtual money to sandbox account
-- This is how users get "fake" money to practice trading
sandboxPayIn :: Text -> Text -> MoneyValue -> IO (Either TBankError ())
sandboxPayIn token accountId amount = do
  let url = tbankSandboxService "SandboxPayIn"
      body = RequestBodyLBS $ encode $ object
        [ "accountId" .= accountId
        , "amount" .= amount
        ]
  
  result <- try @SomeException $ do
    request <- buildAuthRequest token url body
    manager <- newManager tlsManagerSettings
    httpLbs request manager
  
  case result of
    Left e -> return $ Left $ TBankNetworkError (Text.pack $ show (e :: SomeException))
    Right response ->
      case statusCode $ responseStatus response of
        200 -> return $ Right ()
        400 -> return $ Left $ TBankApiError "Invalid request" Nothing
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        404 -> return $ Left $ TBankHttpError 404 "Account not found"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)

-- ============================================================================
-- Sandbox Order Operations
-- ============================================================================

-- | Place an order in the sandbox
-- Mirrors real trading exactly but uses virtual money
postSandboxOrder :: Text -> Text -> OrderRequest -> IO (Either TBankError OrderResponse)
postSandboxOrder token accountId req = do
  -- Generate unique order ID for idempotency
  orderRequestId <- UUID.toText <$> nextRandom
  
  let url = tbankSandboxService "PostSandboxOrder"
      priceObj = case orderRequestPrice req of
        Just p -> scientificToQuotation p
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
          Right sandboxResp -> do
            orderResp <- sandboxOrderResponseToOrderResponse sandboxResp
            return $ Right orderResp
        400 -> return $ Left $ TBankApiError "Invalid order parameters" Nothing
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        404 -> return $ Left $ TBankHttpError 404 "Account or instrument not found"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)

-- | Cancel a sandbox order
cancelSandboxOrder :: Text -> Text -> OrderId -> IO (Either TBankError ())
cancelSandboxOrder token accountId (OrderId orderId) = do
  let url = tbankSandboxService "CancelSandboxOrder"
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
        200 -> return $ Right ()
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        404 -> return $ Left $ TBankHttpError 404 "Order not found"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)

-- | Get the current state of a sandbox order
getSandboxOrderState :: Text -> Text -> OrderId -> IO (Either TBankError (Maybe OrderResponse))
getSandboxOrderState token accountId (OrderId orderId) = do
  let url = tbankSandboxService "GetSandboxOrderState"
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
          Right sandboxResp -> do
            orderResp <- sandboxOrderResponseToOrderResponse sandboxResp
            return $ Right $ Just orderResp
        404 -> return $ Right Nothing  -- Order not found
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)

-- | Get all active sandbox orders
getSandboxOrders :: Text -> Text -> IO (Either TBankError [OrderResponse])
getSandboxOrders token accountId = do
  let url = tbankSandboxService "GetSandboxOrders"
      body = RequestBodyLBS $ encode $ object ["accountId" .= accountId]
  
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
          Right (Object obj) -> case KM.lookup "orders" obj of
            Just (Array orders) -> do
              let parsed = mapMaybe parseOrder (V.toList orders)
              domainOrders <- mapM sandboxOrderResponseToOrderResponse parsed
              return $ Right domainOrders
            _ -> return $ Right []
          Right _ -> return $ Left $ TBankParseError "Unexpected response format"
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)
  where
    parseOrder :: Value -> Maybe SandboxOrderResponse
    parseOrder val = case fromJSON val of
      Success a -> Just a
      Error _ -> Nothing

-- ============================================================================
-- Sandbox Portfolio Operations
-- ============================================================================

-- | Get sandbox portfolio (holdings and cash)
getSandboxPortfolio :: Text -> Text -> Currency -> IO (Either TBankError [PortfolioPosition])
getSandboxPortfolio token accountId currency = do
  let url = tbankSandboxService "GetSandboxPortfolio"
      body = RequestBodyLBS $ encode $ object
        [ "accountId" .= accountId
        , "currency" .= currencyToText currency
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
          Right (Object obj) -> case KM.lookup "positions" obj of
            Just (Array positions) -> 
              let parsed = mapMaybe parsePosition (V.toList positions)
              in return $ Right parsed
            _ -> return $ Right []
          Right _ -> return $ Left $ TBankParseError "Unexpected response format"
        401 -> return $ Left $ TBankAuthError "Invalid or expired token"
        404 -> return $ Left $ TBankHttpError 404 "Account not found"
        code -> return $ Left $ TBankHttpError code (Text.pack $ "HTTP " <> show code)
  where
    parsePosition :: Value -> Maybe PortfolioPosition
    parsePosition val = case fromJSON val of
      Success a -> Just a
      Error _ -> Nothing

-- | Get sandbox positions (alternative view)
getSandboxPositions :: Text -> Text -> IO (Either TBankError [(InstrumentId, Quantity)])
getSandboxPositions token accountId = do
  -- Use GetSandboxPortfolio and extract positions
  result <- getSandboxPortfolio token accountId RUB
  case result of
    Left err -> return $ Left err
    Right positions -> do
      let extracted = mapMaybe extractPosition positions
      return $ Right extracted
  where
    extractPosition :: PortfolioPosition -> Maybe (InstrumentId, Quantity)
    extractPosition pos = do
      figi <- ppFigi pos
      qty <- ppQuantity pos
      return (InstrumentId figi, quotationToScientific qty)
