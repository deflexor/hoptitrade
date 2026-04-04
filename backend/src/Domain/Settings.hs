{-# LANGUAGE DerivingStrategies #-}

module Domain.Settings
  ( Settings (..)
  , OKXCredentials (..)
  , RiskParameters (..)
  , defaultSettings
  , maskCredentials
  ) where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Domain.Types
  ( ApiKeyId (..)
  , UserId (..)
  )
import GHC.Generics (Generic)

-- ============================================================================
-- Settings Model
-- ============================================================================

data Settings = Settings
  { settingsUserId :: UserId
  , settingsOKXCredentials :: Maybe OKXCredentials
  , settingsRiskParams :: RiskParameters
  } deriving stock (Eq, Show, Generic)

instance FromJSON Settings
instance ToJSON Settings where
  toJSON settings = toJSON $ maskCredentials settings

-- ============================================================================
-- OKX API Credentials
-- ============================================================================

data OKXCredentials = OKXCredentials
  { okxApiKeyId :: ApiKeyId
  , okxApiKey :: Text
  , okxApiSecret :: Text
  , okxPassphrase :: Text
  , okxIsDemo :: Bool
  } deriving stock (Eq, Show, Generic)

instance FromJSON OKXCredentials
instance ToJSON OKXCredentials where
  toJSON _ = toJSON ("***REDACTED***" :: Text)

-- ============================================================================
-- Risk Parameters
-- ============================================================================

data RiskParameters = RiskParameters
  { riskMaxLossPercent :: Scientific
  , riskMaxPositionSize :: Scientific
  , riskMaxOpenPositions :: Int
  , riskAutoModeEnabled :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Defaults
-- ============================================================================

defaultSettings :: UserId -> Settings
defaultSettings uid = Settings
  { settingsUserId = uid
  , settingsOKXCredentials = Nothing
  , settingsRiskParams = defaultRiskParams
  }

defaultRiskParams :: RiskParameters
defaultRiskParams = RiskParameters
  { riskMaxLossPercent = 2.0
  , riskMaxPositionSize = 1000.0
  , riskMaxOpenPositions = 5
  , riskAutoModeEnabled = False
  }

-- ============================================================================
-- Security Helpers
-- ============================================================================

maskCredentials :: Settings -> Settings
maskCredentials settings = settings
  { settingsOKXCredentials = Nothing
  }
