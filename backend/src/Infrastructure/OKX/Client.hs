{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module Infrastructure.OKX.Client
  ( OKXClientConfig (..)
  , OKXError (..)
  , fetchOptionInstruments
  , fetchOptionChain
  , fetchUnderlyingPrice
  , defaultOKXConfig
  , OKXInstrument (..)
  , OKXTicker (..)
  ) where

import Control.Monad (void, forM)
import Control.Exception (try, SomeException)
import Data.Aeson
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Scientific (Scientific, toRealFloat, scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime, parseTimeM, defaultTimeLocale)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Option (OptionContract (..), OptionType (..), Strike (..), Expiration (..))
import Domain.Types (InstrumentId (..))
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- ============================================================================
-- Configuration
-- ============================================================================

data OKXClientConfig = OKXClientConfig
  { okxBaseUrl :: Text
  , okxDemoMode :: Bool
  } deriving stock (Eq, Show)

defaultOKXConfig :: OKXClientConfig
defaultOKXConfig = OKXClientConfig
  { okxBaseUrl = "https://www.okx.com"
  , okxDemoMode = False
  }

-- ============================================================================
-- Error Handling
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
  , oi :: Maybe Text
  , delta :: Maybe Text
  , gamma :: Maybe Text
  , theta :: Maybe Text
  , vega :: Maybe Text
  , ulyPrice :: Maybe Text
  , ts :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON OKXTicker where
  parseJSON = withObject "OKXTicker" $ \v -> OKXTicker
    <$> v .: "instId"
    <*> v .:? "bidPx"
    <*> v .:? "askPx"
    <*> v .:? "last"
    <*> v .:? "vol24h"
    <*> v .:? "oi"
    <*> v .:? "delta"
    <*> v .:? "gamma"
    <*> v .:? "theta"
    <*> v .:? "vega"
    <*> v .:? "ulyPrice"
    <*> v .: "ts"

data OKXIndexTicker = OKXIndexTicker
  { idxPx :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON OKXIndexTicker where
  parseJSON = withObject "OKXIndexTicker" $ \v -> OKXIndexTicker
    <$> v .: "idxPx"

data OKXMarkPrice = OKXMarkPrice
  { markPx :: Text
  , markInstId :: Text
  } deriving stock (Eq, Show, Generic)

instance FromJSON OKXMarkPrice where
  parseJSON = withObject "OKXMarkPrice" $ \v -> OKXMarkPrice
    <$> v .: "markPx"
    <*> v .: "instId"

-- ============================================================================
-- API Functions
-- ============================================================================

fetchOptionInstruments :: OKXClientConfig -> Text -> IO (Either OKXError [OKXInstrument])
fetchOptionInstruments config underlying = do
  manager <- newManager tlsManagerSettings
  let url = Text.unpack $ okxBaseUrl config <> "/api/v5/public/instruments?instType=OPTION&uly=" <> underlying
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

fetchOptionChain :: OKXClientConfig -> [OKXInstrument] -> IO (Either OKXError [OKXTicker])
fetchOptionChain config instruments = do
  manager <- newManager tlsManagerSettings
  let instIdList = Text.intercalate "," $ map instId instruments
      url = Text.unpack $ okxBaseUrl config <> "/api/v5/public/tickers?instType=OPTION&instId=" <> instIdList
  
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
        code -> return $ Left $ OKXHttpError code "Failed to fetch tickers"

fetchUnderlyingPrice :: OKXClientConfig -> Text -> IO (Either OKXError Scientific)
fetchUnderlyingPrice config underlying = do
  manager <- newManager tlsManagerSettings
  -- Use the spot ticker for the underlying pair (e.g., BTC-USDT or BTC-USD)
  let spotInstId = if "USD" `Text.isSuffixOf` underlying && not ("USDT" `Text.isSuffixOf` underlying)
                   then underlying <> "-SWAP"  -- Use perpetual for USD pairs
                   else underlying
      url = Text.unpack $ okxBaseUrl config <> "/api/v5/public/mark-price?instType=SWAP&instId=" <> spotInstId
  
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
                then case listToMaybe okxData of
                  Just (OKXMarkPrice {..}) -> 
                    case parseScientific markPx of
                      Just px -> return $ Right px
                      Nothing -> return $ Left $ OKXParseError "Invalid price format"
                  Nothing -> return $ Left $ OKXParseError "Empty price data"
                else return $ Left $ OKXHttpError 200 okxMsg
        code -> return $ Left $ OKXHttpError code "Failed to fetch underlying price"

-- ============================================================================
-- Helpers
-- ============================================================================

parseScientific :: Text -> Maybe Scientific
parseScientific txt = 
  case reads (Text.unpack txt) of
    [(n, "")] -> Just n
    _ -> Nothing

parseTimestampMs :: Text -> Maybe UTCTime
parseTimestampMs txt = 
  case parseScientific txt of
    Just ms -> Just $ posixSecondsToUTCTime $ realToFrac ms / 1000
    Nothing -> Nothing
