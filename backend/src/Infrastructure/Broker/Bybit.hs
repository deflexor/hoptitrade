{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Broker.Bybit
  ( module Infrastructure.Broker.Bybit.Client
  , placeOrderBybit
  , cancelOrderBybit
  , getInstrumentsBybit
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (diffUTCTime, getCurrentTime)
import Domain.Broker (BrokerConfig (..))
import Domain.Order (CancelRequest (..), OrderRequest (..), OrderResponse (..))
import Domain.Types (InstrumentId (..), OrderId (..), OrderStatus (..))
import Infrastructure.Broker.Bybit.Client

placeOrderBybit
  :: BrokerConfig
  -> OrderRequest
  -> Bool  -- reduceOnly
  -> IO (Either BybitError OrderResponse)
placeOrderBybit config req reduceOnly = do
  now <- getCurrentTime
  let InstrumentId sym = orderRequestInstrumentId req
      ms = floor (diffUTCTime now epoch * 1000) :: Integer
      linkId = Text.take 36 $ "ht" <> Text.pack (show ms) <> Text.take 8 sym
  result <- placeOptionOrder config req linkId reduceOnly
  pure $ case result of
    Left err -> Left err
    Right BybitOrderResult{..} -> Right OrderResponse
      { orderResponseOrderId = OrderId borOrderId
      , orderResponseClientOrderId = Just borOrderLinkId
      , orderResponseStatus = OrderOpening
      , orderResponseFilledQty = 0
      , orderResponseAvgPrice = Nothing
      , orderResponseTimestamp = now
      }
  where
    epoch = read "1970-01-01 00:00:00 UTC"

cancelOrderBybit :: BrokerConfig -> CancelRequest -> Text -> IO (Either BybitError Bool)
cancelOrderBybit config req symbol =
  cancelOptionOrder config (cancelRequestOrderId req) symbol

getInstrumentsBybit :: BrokerConfig -> Text -> IO (Either BybitError [BybitInstrument])
getInstrumentsBybit = fetchOptionInstruments
