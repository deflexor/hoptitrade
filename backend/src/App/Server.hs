{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Effects.Position (PositionEffect, runPositionWithPool, runPositionIO)
import Effects.Settings (SettingsEffect, runSettingsWithPool, runSettingsIO, initializeDatabase)
import Infrastructure.Encryption (EncryptionContext, initializeEncryption)
import Effects.WebSocket (WebSocketEffect, runWebSocketIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import Network.HTTP.Types.Header (hAuthorization)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..)
  , cors
  , simpleCorsResourcePolicy
  )
import Polysemy
import Servant
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
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

server :: ConnectionPool -> ServerT API (Sem AppEffects)
server pool =
  healthServer
  :<|> authServer
  :<|> strategiesServer
  :<|> positionsServer pool
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
    $ runPositionWithPool pool
    $ runOrderBookIO
    $ runBrokerIO
    $ runLogIO
    $ runAuthIO now
    $ sem
  pure result

-- ============================================================================
-- CORS
-- ============================================================================

loadCorsOrigins :: IO [B8.ByteString]
loadCorsOrigins = do
  mOrigins <- lookupEnv "HOPTITRADE_CORS_ORIGINS"
  case mOrigins of
    Just raw | not (null raw) ->
      pure $ map (B8.pack . Text.unpack . Text.strip) $ Text.splitOn "," (Text.pack raw)
    _ -> do
      mEnv <- lookupEnv "HOPTITRADE_ENV"
      case mEnv of
        Just "dev" -> pure ["http://localhost:5173", "http://localhost:3000", "http://127.0.0.1:5173"]
        _ -> pure ["http://localhost:5173"]

makeCorsPolicy :: [B8.ByteString] -> CorsResourcePolicy
makeCorsPolicy origins = simpleCorsResourcePolicy
  { corsOrigins = Just (origins, True)
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsRequestHeaders = [hAuthorization, "Content-Type"]
  }

-- ============================================================================
-- Application Initialization
-- ============================================================================

defaultDbPath :: Text
defaultDbPath = "data/hoptitrade.db"

initializeApp :: IO (Application, ConnectionPool, EncryptionContext)
initializeApp = do
  let dbPath = Text.unpack defaultDbPath
  createDirectoryIfMissing True (takeDirectory dbPath)

  putStrLn $ "Initializing database at: " ++ dbPath
  pool <- initializeDatabase defaultDbPath
  putStrLn "Database initialized successfully"

  putStrLn "Initializing encryption..."
  encCtx <- initializeEncryption
  putStrLn "Encryption initialized"

  origins <- loadCorsOrigins
  putStrLn $ "CORS origins: " ++ show origins
  let corsMiddleware = cors (const $ Just $ makeCorsPolicy origins)
      application = corsMiddleware $ serve api $ hoistServer api (nt pool encCtx) (server pool)

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

-- Dummy pool for legacy — use in-memory stub only; positions open won't work.
-- For tests we keep positionsServer with error if pool needed — use undefined carefully.
-- Prefer initializeApp for real runs.

app :: Application
app = cors (const $ Just $ makeCorsPolicy ["http://localhost:5173"]) $
  -- Legacy path uses stub position server without DB pool open/close exchange
  serve api $ hoistServer api ntLegacy (server undefined)
