{-# LANGUAGE TemplateHaskell #-}

module Effects.OKX
  ( OKXEffect (..)
  , placeOrder
  , cancelOrder
  , getPositions
  , getOrderBook
  , getInstruments
  , authenticate
  , runOKXIO
  ) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Domain.Option (OptionContract)
import Domain.Order (CancelRequest, OrderRequest, OrderResponse (..))
import Domain.Position (Position)
import Domain.Types (InstrumentId (..), OrderId (..))
import Polysemy
import Polysemy.Embed

-- ============================================================================
-- Types
-- ============================================================================

data PriceLevel = PriceLevel
  { priceLevelPrice :: Scientific
  , priceLevelSize :: Scientific
  } deriving stock (Eq, Show)

-- ============================================================================
-- OKX Effect Definition
-- ============================================================================

data OKXEffect m a where
  PlaceOrder :: OrderRequest -> OKXEffect m OrderResponse
  CancelOrder :: CancelRequest -> OKXEffect m Bool
  GetPositions :: OKXEffect m [Position]
  GetOrderBook :: InstrumentId -> OKXEffect m (Maybe [PriceLevel])
  GetInstruments :: Text -> OKXEffect m [OptionContract]
  Authenticate :: Text -> Text -> Text -> OKXEffect m Bool

makeSem ''OKXEffect

-- ============================================================================
-- IO Interpreter (Production)
-- ============================================================================

runOKXIO :: Members '[Embed IO] r => Sem (OKXEffect ': r) a -> Sem r a
runOKXIO = interpret $ \case
  PlaceOrder _req -> embed $ do
    putStrLn "OKX: Placing order"
    -- TODO: Implement REST API call to POST /api/v5/trade/order
    pure $ OrderResponse
      { orderResponseOrderId = OrderId "placeholder"
      , orderResponseClientOrderId = Nothing
      , orderResponseStatus = undefined
      , orderResponseFilledQty = 0
      , orderResponseAvgPrice = Nothing
      , orderResponseTimestamp = undefined
      }

  CancelOrder _req -> embed $ do
    putStrLn "OKX: Cancelling order"
    -- TODO: Implement REST API call to POST /api/v5/trade/cancel-order
    pure True

  GetPositions -> embed $ do
    putStrLn "OKX: Getting positions"
    -- TODO: Implement REST API call to GET /api/v5/account/positions
    pure []

  GetOrderBook _instId -> embed $ do
    putStrLn "OKX: Getting order book"
    -- TODO: Implement REST API call to GET /api/v5/market/books
    pure Nothing

  GetInstruments _underlying -> embed $ do
    putStrLn "OKX: Getting instruments"
    -- TODO: Implement REST API call to GET /api/v5/public/instruments
    pure []

  Authenticate _apiKey _apiSecret _passphrase -> embed $ do
    putStrLn "OKX: Authenticating"
    -- TODO: Implement authentication logic
    pure True
