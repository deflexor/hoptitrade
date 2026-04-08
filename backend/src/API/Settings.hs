{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Settings
  ( SettingsAPI
  , settingsServer
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Domain.Broker (Broker (..), BrokerConfig (..), BrokerMode (..))
import qualified Domain.Settings as DS
import Domain.Types (ApiKeyId (..), UserId (..))
import Effects.Settings
import GHC.Generics (Generic)
import Polysemy
import Servant

-- ============================================================================
-- API Type
-- ============================================================================

type SettingsAPI =
  -- Get current settings
  "settings" :> Get '[JSON] SettingsResponse
  
  -- Update general settings
  :<|> "settings" :> ReqBody '[JSON] UpdateSettingsRequest :> Post '[JSON] SettingsResponse
  
  -- OKX credentials
  :<|> "settings" :> "credentials" :> "okx" :> ReqBody '[JSON] UpdateOKXCredentialsRequest :> Post '[JSON] CredentialsResponse
  
  -- T-Bank credentials
  :<|> "settings" :> "credentials" :> "tbank" :> ReqBody '[JSON] UpdateTBankCredentialsRequest :> Post '[JSON] CredentialsResponse
  
  -- T-Bank sandbox account management
  :<|> "settings" :> "tbank" :> "sandbox" :> "accounts" :> Get '[JSON] TBankSandboxAccountsResponse
  :<|> "settings" :> "tbank" :> "sandbox" :> "accounts" :> ReqBody '[JSON] SaveTBankSandboxAccountRequest :> Post '[JSON] CredentialsResponse
  :<|> "settings" :> "tbank" :> "sandbox" :> "accounts" :> "default" :> ReqBody '[JSON] SetDefaultAccountRequest :> Post '[JSON] CredentialsResponse
  
  -- Broker selection
  :<|> "settings" :> "broker" :> ReqBody '[JSON] SetBrokerRequest :> Post '[JSON] SettingsResponse
  
  -- Get active broker config (for internal use)
  :<|> "settings" :> "broker" :> "config" :> Get '[JSON] (Maybe BrokerConfigResponse)

-- ============================================================================
-- Request/Response Types
-- ============================================================================

data SettingsResponse = SettingsResponse
  { settingsRiskMaxLossPercent :: Scientific
  , settingsRiskMaxPositionSize :: Scientific
  , settingsRiskMaxOpenPositions :: Int
  , settingsRiskAutoModeEnabled :: Bool
  , settingsSelectedBroker :: DS.SelectedBroker
  , settingsUseSandbox :: Bool
  , settingsHasOKXCredentials :: Bool
  , settingsHasTBankSandboxToken :: Bool
  , settingsHasTBankRealToken :: Bool
  , settingsTBankRealTradingEnabled :: Bool
  , settingsHasActiveBroker :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UpdateSettingsRequest = UpdateSettingsRequest
  { updateMaxLossPercent :: Scientific
  , updateMaxPositionSize :: Scientific
  , updateMaxOpenPositions :: Int
  , updateAutoModeEnabled :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- OKX Credentials
data UpdateOKXCredentialsRequest = UpdateOKXCredentialsRequest
  { okxReqApiKey :: Text
  , okxReqApiSecret :: Text
  , okxReqPassphrase :: Text
  , okxReqIsDemo :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- T-Bank Credentials
data UpdateTBankCredentialsRequest = UpdateTBankCredentialsRequest
  { tbankReqSandboxToken :: Maybe Text
  , tbankReqRealToken :: Maybe Text
  , tbankReqEnableRealTrading :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- T-Bank Sandbox Account Management
data TBankSandboxAccountsResponse = TBankSandboxAccountsResponse
  { tsarAccounts :: [DS.TBankSandboxInfo]
  , tsarDefaultAccountId :: Maybe Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SaveTBankSandboxAccountRequest = SaveTBankSandboxAccountRequest
  { stbarAccountId :: Text
  , stbarName :: Maybe Text
  , stbarBalance :: Maybe Scientific
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SetDefaultAccountRequest = SetDefaultAccountRequest
  { sdarAccountId :: Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Broker Selection
data SetBrokerRequest = SetBrokerRequest
  { sbrBroker :: DS.SelectedBroker
  , sbrUseSandbox :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Broker Config Response (for internal use)
data BrokerConfigResponse = BrokerConfigResponse
  { bcrBroker :: Broker
  , bcrMode :: BrokerMode
  , bcrIsConfigured :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Generic credentials response
data CredentialsResponse = CredentialsResponse
  { credSuccess :: Bool
  , credError :: Maybe Text
  , credMessage :: Maybe Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Server
-- ============================================================================

settingsServer :: Members '[SettingsEffect, Embed IO] r => ServerT SettingsAPI (Sem r)
settingsServer = 
  getSettingsHandler 
  :<|> updateSettingsHandler 
  :<|> updateOKXCredentialsHandler
  :<|> updateTBankCredentialsHandler
  :<|> getTBankSandboxAccountsHandler
  :<|> saveTBankSandboxAccountHandler
  :<|> setDefaultTBankSandboxAccountHandler
  :<|> setBrokerHandler
  :<|> getBrokerConfigHandler
  where
    
    -- Helper to get current user ID (placeholder until auth is implemented)
    getCurrentUserId :: UserId
    getCurrentUserId = UserId $ read "550e8400-e29b-41d4-a716-446655440000"

    -- GET /settings
    getSettingsHandler = do
      let uid = getCurrentUserId
      settings <- getSettings uid
      pure $ toSettingsResponse settings

    -- POST /settings
    updateSettingsHandler req = do
      let uid = getCurrentUserId
      currentSettings <- getSettings uid
      let newSettings = currentSettings
            { DS.settingsRiskParams = DS.RiskParameters
              { DS.riskMaxLossPercent = updateMaxLossPercent req
              , DS.riskMaxPositionSize = updateMaxPositionSize req
              , DS.riskMaxOpenPositions = updateMaxOpenPositions req
              , DS.riskAutoModeEnabled = updateAutoModeEnabled req
              }
            }
      _updatedSettings <- updateSettings uid newSettings
      pure $ toSettingsResponse newSettings

    -- POST /settings/credentials/okx
    updateOKXCredentialsHandler req = do
      let uid = getCurrentUserId
      let creds = DS.OKXCredentials
            { DS.okxApiKeyId = ApiKeyId $ read "00000000-0000-0000-0000-000000000000"
            , DS.okxApiKey = okxReqApiKey req
            , DS.okxApiSecret = okxReqApiSecret req
            , DS.okxPassphrase = okxReqPassphrase req
            , DS.okxIsDemo = okxReqIsDemo req
            }
      success <- saveOKXCredentials uid creds
      pure $ CredentialsResponse
        { credSuccess = success
        , credError = if success then Nothing else Just "Failed to save OKX credentials"
        , credMessage = Just "OKX credentials saved successfully"
        }

    -- POST /settings/credentials/tbank
    updateTBankCredentialsHandler req = do
      let uid = getCurrentUserId
      -- Get existing credentials to preserve sandbox accounts
      existingCreds <- getTBankCredentials uid
      let creds = DS.TBankCredentials
            { DS.tbankSandboxToken = tbankReqSandboxToken req
            , DS.tbankRealToken = tbankReqRealToken req
            , DS.tbankSandboxAccounts = maybe [] DS.tbankSandboxAccounts existingCreds
            , DS.tbankDefaultSandboxAccount = DS.tbankDefaultSandboxAccount =<< existingCreds
            , DS.tbankRealTradingEnabled = tbankReqEnableRealTrading req
            }
      
      -- Validate: if enabling real trading, must have real token
      if tbankReqEnableRealTrading req && tbankReqRealToken req == Nothing
        then pure $ CredentialsResponse
          { credSuccess = False
          , credError = Just "Cannot enable real trading without real API token"
          , credMessage = Nothing
          }
        else do
          success <- saveTBankCredentials uid creds
          pure $ CredentialsResponse
            { credSuccess = success
            , credError = if success then Nothing else Just "Failed to save T-Bank credentials"
            , credMessage = Just $ if tbankReqEnableRealTrading req
                then "T-Bank credentials saved. REAL TRADING ENABLED - USE WITH CAUTION!"
                else "T-Bank credentials saved"
            }

    -- GET /settings/tbank/sandbox/accounts
    getTBankSandboxAccountsHandler = do
      let uid = getCurrentUserId
      accounts <- getTBankSandboxAccounts uid
      mCreds <- getTBankCredentials uid
      pure $ TBankSandboxAccountsResponse
        { tsarAccounts = accounts
        , tsarDefaultAccountId = DS.tbankDefaultSandboxAccount =<< mCreds
        }

    -- POST /settings/tbank/sandbox/accounts
    saveTBankSandboxAccountHandler req = do
      let uid = getCurrentUserId
      let accInfo = DS.TBankSandboxInfo
            { DS.tsiAccountId = stbarAccountId req
            , DS.tsiName = stbarName req
            , DS.tsiBalance = stbarBalance req
            }
      success <- saveTBankSandboxAccount uid accInfo
      pure $ CredentialsResponse
        { credSuccess = success
        , credError = if success then Nothing else Just "Failed to save sandbox account"
        , credMessage = Just "Sandbox account saved"
        }

    -- POST /settings/tbank/sandbox/accounts/default
    setDefaultTBankSandboxAccountHandler req = do
      let uid = getCurrentUserId
      success <- setDefaultTBankSandboxAccount uid (sdarAccountId req)
      pure $ CredentialsResponse
        { credSuccess = success
        , credError = if success then Nothing else Just "Failed to set default account"
        , credMessage = Just "Default sandbox account updated"
        }

    -- POST /settings/broker
    setBrokerHandler req = do
      let uid = getCurrentUserId
      -- Set broker selection
      _ <- setSelectedBroker uid (sbrBroker req)
      _ <- setUseSandbox uid (sbrUseSandbox req)
      
      -- Get updated settings
      settings <- getSettings uid
      pure $ toSettingsResponse settings

    -- GET /settings/broker/config
    getBrokerConfigHandler = do
      let uid = getCurrentUserId
      settings <- getSettings uid
      
      case DS.settingsToBrokerConfig settings of
        Just (OKXConfig {}) -> pure $ Just $ BrokerConfigResponse
          { bcrBroker = OKX
          , bcrMode = if DS.okxIsDemo (case DS.settingsOKXCredentials settings of Just c -> c; Nothing -> error "Missing OKX creds")
                      then Sandbox else Real
          , bcrIsConfigured = True
          }
        Just (TBankConfig _ _ mode) -> pure $ Just $ BrokerConfigResponse
          { bcrBroker = TBank
          , bcrMode = mode
          , bcrIsConfigured = True
          }
        Nothing -> pure Nothing

    toSettingsResponse :: DS.Settings -> SettingsResponse
    toSettingsResponse settings = SettingsResponse
      { settingsRiskMaxLossPercent = DS.riskMaxLossPercent $ DS.settingsRiskParams settings
      , settingsRiskMaxPositionSize = DS.riskMaxPositionSize $ DS.settingsRiskParams settings
      , settingsRiskMaxOpenPositions = DS.riskMaxOpenPositions $ DS.settingsRiskParams settings
      , settingsRiskAutoModeEnabled = DS.riskAutoModeEnabled $ DS.settingsRiskParams settings
      , settingsSelectedBroker = DS.bpSelectedBroker $ DS.settingsBrokerPreference settings
      , settingsUseSandbox = DS.bpUseSandbox $ DS.settingsBrokerPreference settings
      , settingsHasOKXCredentials = case DS.settingsOKXCredentials settings of
          Just _ -> True
          Nothing -> False
      , settingsHasTBankSandboxToken = case DS.settingsTBankCredentials settings of
          Just creds -> DS.tbankSandboxToken creds /= Nothing
          Nothing -> False
      , settingsHasTBankRealToken = case DS.settingsTBankCredentials settings of
          Just creds -> DS.tbankRealToken creds /= Nothing
          Nothing -> False
      , settingsTBankRealTradingEnabled = case DS.settingsTBankCredentials settings of
          Just creds -> DS.tbankRealTradingEnabled creds
          Nothing -> False
      , settingsHasActiveBroker = DS.hasActiveBroker settings
      }
