{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Server
  ( API
  , api
  , server
  , app
  ) where

import API.Auth (AuthAPI, authServer)
import API.Health (HealthAPI, healthServer)
import API.Orders (OrdersAPI, ordersServer)
import API.Positions (PositionsAPI, positionsServer)
import API.Settings (SettingsAPI, settingsServer)
import API.Strategies (StrategiesAPI, strategiesServer)
import Data.Time (getCurrentTime)
import Effects.Auth (AuthEffect, runAuthIO)
import Effects.Log (LogEffect, runLogIO)
import Effects.OKX (OKXEffect, runOKXIO)
import Effects.OrderBook (OrderBookEffect, runOrderBookIO)
import Effects.Position (PositionEffect, runPositionIO)
import Effects.Settings (SettingsEffect, runSettingsIO)
import Effects.WebSocket (WebSocketEffect, runWebSocketIO)
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Polysemy
import Polysemy.Embed
import Servant

-- ============================================================================
-- Combined API Type
-- ============================================================================

type API =
  HealthAPI
  :<|> AuthAPI
  :<|> StrategiesAPI
  :<|> PositionsAPI
  :<|> OrdersAPI
  :<|> SettingsAPI

api :: Proxy API
api = Proxy

-- ============================================================================
-- Server Implementation
-- ============================================================================

type AppEffects =
  '[ AuthEffect
   , LogEffect
   , OKXEffect
   , OrderBookEffect
   , PositionEffect
   , SettingsEffect
   , WebSocketEffect
   , Embed IO
   ]

server :: ServerT API (Sem AppEffects)
server =
  healthServer
  :<|> authServer
  :<|> strategiesServer
  :<|> positionsServer
  :<|> ordersServer
  :<|> settingsServer

-- ============================================================================
-- Natural Transformation (Sem r -> Handler)
-- ============================================================================

nt :: Sem AppEffects a -> Handler a
nt sem = do
  now <- liftIO getCurrentTime
  result <- liftIO $ runM
    $ runWebSocketIO
    $ runSettingsIO
    $ runPositionIO
    $ runOrderBookIO
    $ runOKXIO
    $ runLogIO
    $ runAuthIO now
    $ sem
  pure result

-- ============================================================================
-- Application
-- ============================================================================

app :: Application
app = simpleCors $ serve api $ hoistServer api nt server
