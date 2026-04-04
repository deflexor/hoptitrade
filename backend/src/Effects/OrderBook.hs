{-# LANGUAGE TemplateHaskell #-}

module Effects.OrderBook
  ( OrderBookEffect (..)
  , getTopOfBook
  , getSpread
  , subscribeToInstrument
  , unsubscribeFromInstrument
  , runOrderBookIO
  ) where

import Data.Scientific (Scientific)
import Domain.Types (InstrumentId (..), Price)
import Polysemy
import Polysemy.Embed

-- ============================================================================
-- Order Book Effect Definition
-- ============================================================================

data OrderBookEffect m a where
  GetTopOfBook :: InstrumentId -> OrderBookEffect m (Maybe (Price, Price))
  GetSpread :: InstrumentId -> OrderBookEffect m (Maybe Scientific)
  SubscribeToInstrument :: InstrumentId -> OrderBookEffect m ()
  UnsubscribeFromInstrument :: InstrumentId -> OrderBookEffect m ()

makeSem ''OrderBookEffect

-- ============================================================================
-- IO Interpreter (Production)
-- ============================================================================

runOrderBookIO :: Members '[Embed IO] r => Sem (OrderBookEffect ': r) a -> Sem r a
runOrderBookIO = interpret $ \case
  GetTopOfBook instId -> embed $ do
    putStrLn $ "Getting top of book for: " ++ show instId
    -- TODO: Implement order book lookup
    pure Nothing

  GetSpread instId -> embed $ do
    putStrLn $ "Getting spread for: " ++ show instId
    -- TODO: Implement spread calculation
    pure Nothing

  SubscribeToInstrument instId -> embed $ do
    putStrLn $ "Subscribing to instrument: " ++ show instId
    -- TODO: Implement WebSocket subscription
    pure ()

  UnsubscribeFromInstrument instId -> embed $ do
    putStrLn $ "Unsubscribing from instrument: " ++ show instId
    -- TODO: Implement WebSocket unsubscription
    pure ()
