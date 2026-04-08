{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App.Server
  ( API
  , api
  , server
  , app
  , initializeApp
  , defaultDbPath
  ) where

import API.Auth (AuthAPI, authServer)
import API.Health (HealthAPI, healthServer)
import API.Orders (OrdersAPI, ordersServer)
import API.Positions (PositionsAPI, positionsServer)
import API.Settings (SettingsAPI, settingsServer)
import API.Strategies (StrategiesAPI, strategiesServer)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Database.Persist.Sql (ConnectionPool)
import Effects.Auth (AuthEffect, runAuthIO)
import Effects.Broker (BrokerEffect, runBrokerIO)
import Effects.Log (LogEffect, runLogIO)
import Effects.OrderBook (OrderBookEffect, runOrderBookIO)
import Effects.Position (PositionEffect, runPositionIO)
import Effects.Settings (SettingsEffect, runSettingsWithPool, runSettingsIO, initializeDatabase)
import Infrastructure.Encryption (EncryptionContext, initializeEncryption)
import Effects.WebSocket (WebSocketEffect, runWebSocketIO)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.Cors (simpleCors)
import Polysemy
import Servant
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

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
   , BrokerEffect
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
-- Natural Transformation with Database Pool
-- ============================================================================

nt :: ConnectionPool -> EncryptionContext -> Sem AppEffects a -> Handler a
nt pool encCtx sem = do
  now <- liftIO getCurrentTime
  result <- liftIO $ runM
    $ runWebSocketIO
    $ runSettingsWithPool pool encCtx
    $ runPositionIO
    $ runOrderBookIO
    $ runBrokerIO
    $ runLogIO
    $ runAuthIO now
    $ sem
  pure result

-- ============================================================================
-- Application Initialization
-- ============================================================================

-- | Default database path
defaultDbPath :: Text
defaultDbPath = "data/hoptitrade.db"

-- | Initialize the application with database
initializeApp :: IO (Application, ConnectionPool, EncryptionContext)
initializeApp = do
  -- Ensure data directory exists
  let dbPath = Text.unpack defaultDbPath
  createDirectoryIfMissing True (takeDirectory dbPath)
  
  -- Initialize database
  putStrLn $ "Initializing database at: " ++ dbPath
  pool <- initializeDatabase defaultDbPath
  putStrLn "Database initialized successfully"
  
  -- Initialize encryption
  putStrLn "Initializing encryption..."
  encCtx <- initializeEncryption
  putStrLn "Encryption initialized"
  
  -- Create the application with pool and encryption
  let application = simpleCors $ serve api $ hoistServer api (nt pool encCtx) server
  
  pure (application, pool, encCtx)

-- ============================================================================
-- Legacy Application (without database - for testing)
-- ============================================================================

ntLegacy :: Sem AppEffects a -> Handler a
ntLegacy sem = do
  now <- liftIO getCurrentTime
  result <- liftIO $ runM
    $ runWebSocketIO
    $ runSettingsIO
    $ runPositionIO
    $ runOrderBookIO
    $ runBrokerIO
    $ runLogIO
    $ runAuthIO now
    $ sem
  pure result

-- | Legacy app without database (for testing)
app :: Application
app = simpleCors $ serve api $ hoistServer api ntLegacy server
