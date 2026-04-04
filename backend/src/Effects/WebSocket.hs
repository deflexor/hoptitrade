{-# LANGUAGE TemplateHaskell #-}

module Effects.WebSocket
  ( WebSocketEffect (..)
  , connectWebSocket
  , disconnectWebSocket
  , sendWebSocketMessage
  , receiveWebSocketMessage
  , subscribeChannel
  , unsubscribeChannel
  , runWebSocketIO
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Polysemy
import Polysemy.Embed

-- ============================================================================
-- WebSocket Effect Definition
-- ============================================================================

data WebSocketEffect m a where
  ConnectWebSocket :: Text -> WebSocketEffect m ()
  DisconnectWebSocket :: WebSocketEffect m ()
  SendWebSocketMessage :: Value -> WebSocketEffect m ()
  ReceiveWebSocketMessage :: WebSocketEffect m (Maybe Value)
  SubscribeChannel :: Text -> WebSocketEffect m ()
  UnsubscribeChannel :: Text -> WebSocketEffect m ()

makeSem ''WebSocketEffect

-- ============================================================================
-- IO Interpreter (Production)
-- ============================================================================

runWebSocketIO :: Members '[Embed IO] r => Sem (WebSocketEffect ': r) a -> Sem r a
runWebSocketIO = interpret $ \case
  ConnectWebSocket url -> embed $ do
    putStrLn $ "Connecting to WebSocket: " ++ show url
    -- TODO: Implement actual WebSocket connection using wuss
    pure ()

  DisconnectWebSocket -> embed $ do
    putStrLn "Disconnecting WebSocket"
    -- TODO: Implement disconnection
    pure ()

  SendWebSocketMessage msg -> embed $ do
    putStrLn $ "Sending WebSocket message: " ++ show msg
    -- TODO: Implement message sending
    pure ()

  ReceiveWebSocketMessage -> embed @IO $ do
    -- TODO: Implement message receiving
    pure (Nothing :: Maybe Value)

  SubscribeChannel channel -> embed $ do
    putStrLn $ "Subscribing to channel: " ++ show channel
    -- TODO: Implement channel subscription
    pure ()

  UnsubscribeChannel channel -> embed $ do
    putStrLn $ "Unsubscribing from channel: " ++ show channel
    -- TODO: Implement channel unsubscription
    pure ()
