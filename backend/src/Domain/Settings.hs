{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

module Domain.Settings
  ( -- * Settings
    Settings (..)
  , defaultSettings
  , maskCredentials
    -- * Broker Configuration
  , BrokerPreference (..)
  , SelectedBroker (..)
    -- * Credentials
  , OKXCredentials (..)
  , TBankCredentials (..)
  , TBankSandboxInfo (..)
    -- * Risk Parameters
  , RiskParameters (..)
  , defaultRiskParams
    -- * Conversion Helpers
  , settingsToBrokerConfig
  , hasActiveBroker
  ) where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Domain.Broker as Broker
import Domain.Types (ApiKeyId (..), UserId (..))
import GHC.Generics (Generic)

-- ============================================================================
-- Settings Model
-- ============================================================================

data Settings = Settings
  { settingsUserId :: UserId
  , settingsBrokerPreference :: BrokerPreference
  , settingsOKXCredentials :: Maybe OKXCredentials
  , settingsTBankCredentials :: Maybe TBankCredentials
  , settingsRiskParams :: RiskParameters
  } deriving stock (Eq, Show, Generic)

instance FromJSON Settings
instance ToJSON Settings where
  toJSON settings = toJSON $ maskCredentials settings

-- ============================================================================
-- Broker Preference
-- ============================================================================

-- | Which broker the user prefers to use
data BrokerPreference = BrokerPreference
  { bpSelectedBroker :: SelectedBroker
  , bpUseSandbox :: Bool  -- ^ Whether to use sandbox mode (if available)
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SelectedBroker
  = BrokerOKX
  | BrokerTBank
  | BrokerNone
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Smart constructors to avoid confusion with Domain.Broker.Broker
pattern OKXBroker :: SelectedBroker
pattern OKXBroker = BrokerOKX

pattern TBankBroker :: SelectedBroker
pattern TBankBroker = BrokerTBank

pattern NoBroker :: SelectedBroker
pattern NoBroker = BrokerNone

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
-- T-Bank Credentials
-- ============================================================================

-- | T-Bank credentials include both sandbox and real trading tokens
-- Users MUST use sandbox first, then can enable real trading
data TBankCredentials = TBankCredentials
  { tbankSandboxToken :: Maybe Text        -- ^ Token for sandbox practice
  , tbankRealToken :: Maybe Text           -- ^ Token for real trading (separate!)
  , tbankSandboxAccounts :: [TBankSandboxInfo]  -- ^ Sandbox account IDs
  , tbankDefaultSandboxAccount :: Maybe Text    -- ^ Preferred sandbox account
  , tbankRealTradingEnabled :: Bool        -- ^ Has user enabled real trading?
  } deriving stock (Eq, Show, Generic)

instance FromJSON TBankCredentials
instance ToJSON TBankCredentials where
  toJSON _ = toJSON ("***REDACTED***" :: Text)

-- | Sandbox account information
data TBankSandboxInfo = TBankSandboxInfo
  { tsiAccountId :: Text
  , tsiName :: Maybe Text
  , tsiBalance :: Maybe Scientific  -- ^ Last known balance
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

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
  , settingsBrokerPreference = BrokerPreference BrokerNone True
  , settingsOKXCredentials = Nothing
  , settingsTBankCredentials = Nothing
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
-- Conversion Helpers
-- ============================================================================

-- | Convert Settings to BrokerConfig for use with Effects.Broker
-- Returns Nothing if broker not configured or credentials missing
settingsToBrokerConfig :: Settings -> Maybe Broker.BrokerConfig
settingsToBrokerConfig settings =
  case bpSelectedBroker (settingsBrokerPreference settings) of
    BrokerOKX -> do
      creds <- settingsOKXCredentials settings
      return $ Broker.OKXConfig
        { Broker.okxApiKey = okxApiKey creds
        , Broker.okxSecret = okxApiSecret creds
        , Broker.okxPassphrase = okxPassphrase creds
        , Broker.okxDemoMode = okxIsDemo creds
        }
    
    BrokerTBank -> do
      creds <- settingsTBankCredentials settings
      if bpUseSandbox (settingsBrokerPreference settings)
        then do
          -- Use sandbox mode
          token <- tbankSandboxToken creds
          accId <- tbankDefaultSandboxAccount creds
          return $ Broker.TBankConfig
            { Broker.tbankToken = token
            , Broker.tbankAccountId = Just accId
            , Broker.tbankMode = Broker.Sandbox
            }
        else do
          -- Use real trading (only if enabled)
          if not (tbankRealTradingEnabled creds)
            then Nothing  -- Real trading not enabled
            else do
              token <- tbankRealToken creds
              return $ Broker.TBankConfig
                { Broker.tbankToken = token
                , Broker.tbankAccountId = Nothing  -- Will be fetched from API
                , Broker.tbankMode = Broker.Real
                }
    
    BrokerNone -> Nothing

-- | Check if user has configured an active broker
hasActiveBroker :: Settings -> Bool
hasActiveBroker settings =
  case bpSelectedBroker (settingsBrokerPreference settings) of
    BrokerNone -> False
    BrokerOKX -> case settingsOKXCredentials settings of
      Just _ -> True
      Nothing -> False
    BrokerTBank -> case settingsTBankCredentials settings of
      Just creds ->
        if bpUseSandbox (settingsBrokerPreference settings)
          then case (tbankSandboxToken creds, tbankDefaultSandboxAccount creds) of
            (Just _, Just _) -> True
            _ -> False
          else tbankRealTradingEnabled creds && tbankRealToken creds /= Nothing
      Nothing -> False

-- ============================================================================
-- Security Helpers
-- ============================================================================

maskCredentials :: Settings -> Settings
maskCredentials settings = settings
  { settingsOKXCredentials = Nothing
  , settingsTBankCredentials = Nothing
  }
