module Infrastructure.Config where

import Data.Text (Text)

-- ============================================================================
-- Configuration Types
-- ============================================================================

data ServerConfig = ServerConfig
  { serverPort :: Int
  , serverHost :: Text
  } deriving stock (Eq, Show)

data OKXConfig = OKXConfig
  { okxBaseUrl :: Text
  , okxWebSocketUrl :: Text
  , okxDemo :: Bool
  } deriving stock (Eq, Show)

data DatabaseConfig = DatabaseConfig
  { dbHost :: Text
  , dbPort :: Int
  , dbName :: Text
  , dbUser :: Text
  , dbPassword :: Text
  } deriving stock (Eq, Show)

data AppConfig = AppConfig
  { appServer :: ServerConfig
  , appOKX :: OKXConfig
  , appDatabase :: DatabaseConfig
  } deriving stock (Eq, Show)

-- ============================================================================
-- Default Configuration
-- ============================================================================

defaultConfig :: AppConfig
defaultConfig = AppConfig
  { appServer = ServerConfig
    { serverPort = 8080
    , serverHost = "0.0.0.0"
    }
  , appOKX = OKXConfig
    { okxBaseUrl = "https://www.okx.com"
    , okxWebSocketUrl = "wss://ws.okx.com:8443/ws/v5/public"
    , okxDemo = True
    }
  , appDatabase = DatabaseConfig
    { dbHost = "localhost"
    , dbPort = 5432
    , dbName = "hoptitrade"
    , dbUser = "hoptitrade"
    , dbPassword = ""
    }
  }

-- ============================================================================
-- Environment-based Configuration
-- ============================================================================

loadConfig :: IO AppConfig
loadConfig = do
  -- TODO: Load from environment variables or config file
  pure defaultConfig
