{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Health
  ( HealthAPI
  , healthServer
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Polysemy
import Servant

-- ============================================================================
-- API Type
-- ============================================================================

type HealthAPI =
  "health" :> Get '[JSON] HealthStatus
  :<|> "health" :> "ready" :> Get '[JSON] ReadyStatus

-- ============================================================================
-- Response Types
-- ============================================================================

data HealthStatus = HealthStatus
  { healthStatus :: Text
  , healthTimestamp :: UTCTime
  , healthVersion :: Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReadyStatus = ReadyStatus
  { readyStatus :: Text
  , readyOKXConnection :: Bool
  , readyDatabase :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Server
-- ============================================================================

healthServer :: Members '[Embed IO] r => ServerT HealthAPI (Sem r)
healthServer = getHealth :<|> getReady
  where
    getHealth = do
      now <- embed $ getCurrentTime
      pure $ HealthStatus
        { healthStatus = "healthy"
        , healthTimestamp = now
        , healthVersion = "0.1.0.0"
        }

    getReady = do
      pure $ ReadyStatus
        { readyStatus = "ready"
        , readyOKXConnection = True  -- TODO: Check actual connection
        , readyDatabase = True       -- TODO: Check actual DB
        }
