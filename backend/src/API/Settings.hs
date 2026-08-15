{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Settings
  ( SettingsAPI
  , settingsServer
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Domain.Broker (Broker (..), BrokerConfig (..), BrokerMode (..))
import qualified Domain.Settings as DS
import Domain.Types (ApiKeyId (..), UserId (..))
import Domain.User (AuthToken (..))
import Effects.Auth (AuthEffect, extractBearerToken, verifyToken)
import Effects.Settings
import GHC.Generics (Generic)
import Polysemy
import Servant

-- ============================================================================
-- API Type
-- ============================================================================

type SettingsAPI =
  -- Get current settings
  "settings" :> Header "Authorization" Text :> Get '[JSON] SettingsResponse
  
  -- Update general settings
  :<|> "settings" :> Header "Authorization" Text :> ReqBody '[JSON] UpdateSettingsRequest :> Post '[JSON] SettingsResponse
  
  -- OKX credentials
  :<|> "settings" :> "credentials" :> "okx" :> Header "Authorization" Text :> ReqBody '[JSON] UpdateOKXCredentialsRequest :> Post '[JSON] CredentialsResponse
  
  -- T-Bank credentials
  :<|> "settings" :> "credentials" :> "tbank" :> Header "Authorization" Text :> ReqBody '[JSON] UpdateTBankCredentialsRequest :> Post '[JSON] CredentialsResponse
  
  -- Bybit credentials
  :<|> "settings" :> "credentials" :> "bybit" :> Header "Authorization" Text :> ReqBody '[JSON] UpdateBybitCredentialsRequest :> Post '[JSON] CredentialsResponse
  
  -- T-Bank sandbox account management
  :<|> "settings" :> "tbank" :> "sandbox" :> "accounts" :> Header "Authorization" Text :> Get '[JSON] TBankSandboxAccountsResponse
  :<|> "settings" :> "tbank" :> "sandbox" :> "accounts" :> Header "Authorization" Text :> ReqBody '[JSON] SaveTBankSandboxAccountRequest :> Post '[JSON] CredentialsResponse
  :<|> "settings" :> "tbank" :> "sandbox" :> "accounts" :> "default" :> Header "Authorization" Text :> ReqBody '[JSON] SetDefaultAccountRequest :> Post '[JSON] CredentialsResponse
  
  -- Broker selection
  :<|> "settings" :> "broker" :> Header "Authorization" Text :> ReqBody '[JSON] SetBrokerRequest :> Post '[JSON] SettingsResponse
  
  -- Get active broker config (for internal use)
  :<|> "settings" :> "broker" :> "config" :> Header "Authorization" Text :> Get '[JSON] (Maybe BrokerConfigResponse)

-- ============================================================================
-- Request/Response Types
-- ============================================================================

data SettingsResponse = SettingsResponse
  { settingsRiskMaxLossPercent :: Scientific
  , settingsRiskMaxPositionSize :: Scientific
  , settingsRiskMaxOpenPositions :: Int
  , settingsRiskAutoModeEnabled :: Bool
  , settingsRiskTakeProfitPercent :: Scientific
  , settingsRiskKellyFraction :: Scientific
  , settingsRiskRebalanceEnabled :: Bool
  , settingsRiskMinRebalanceImprovement :: Scientific
  , settingsSelectedBroker :: DS.SelectedBroker
  , settingsUseSandbox :: Bool
  , settingsHasOKXCredentials :: Bool
  , settingsHasTBankSandboxToken :: Bool
  , settingsHasTBankRealToken :: Bool
  , settingsTBankRealTradingEnabled :: Bool
  , settingsHasBybitCredentials :: Bool
  , settingsHasActiveBroker :: Bool
  , settingsSupportedBybitCoins :: [Text]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UpdateSettingsRequest = UpdateSettingsRequest
  { updateMaxLossPercent :: Scientific
  , updateMaxPositionSize :: Scientific
  , updateMaxOpenPositions :: Int
  , updateAutoModeEnabled :: Bool
  , updateTakeProfitPercent :: Maybe Scientific
  , updateKellyFraction :: Maybe Scientific
  , updateRebalanceEnabled :: Maybe Bool
  , updateMinRebalanceImprovement :: Maybe Scientific
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

-- Bybit Credentials
data UpdateBybitCredentialsRequest = UpdateBybitCredentialsRequest
  { bybitReqApiKey :: Text
  , bybitReqApiSecret :: Text
  , bybitReqTestnet :: Bool
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

settingsServer :: Members '[AuthEffect, SettingsEffect, Embed IO] r => ServerT SettingsAPI (Sem r)
settingsServer = 
  getSettingsHandler 
  :<|> updateSettingsHandler 
  :<|> updateOKXCredentialsHandler
  :<|> updateTBankCredentialsHandler
  :<|> updateBybitCredentialsHandler
  :<|> getTBankSandboxAccountsHandler
  :<|> saveTBankSandboxAccountHandler
  :<|> setDefaultTBankSandboxAccountHandler
  :<|> setBrokerHandler
  :<|> getBrokerConfigHandler
  where
    -- Resolve user from Authorization header
    resolveUser :: Member AuthEffect r => Maybe Text -> Sem r (Maybe UserId)
    resolveUser mAuthHeader = case mAuthHeader >>= extractBearerToken of
      Nothing -> pure Nothing
      Just token -> verifyToken (AuthToken token)
    
    -- GET /settings
    getSettingsHandler mAuthHeader = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ errorSettingsResponse
        Just uid -> do
          settings <- getSettings uid
          pure $ toSettingsResponse settings

    -- POST /settings
    updateSettingsHandler mAuthHeader req = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ errorSettingsResponse
        Just uid -> do
          currentSettings <- getSettings uid
          let oldRisk = DS.settingsRiskParams currentSettings
              newSettings = currentSettings
                { DS.settingsRiskParams = DS.RiskParameters
                  { DS.riskMaxLossPercent = updateMaxLossPercent req
                  , DS.riskMaxPositionSize = updateMaxPositionSize req
                  , DS.riskMaxOpenPositions = updateMaxOpenPositions req
                  , DS.riskAutoModeEnabled = updateAutoModeEnabled req
                  , DS.riskTakeProfitPercent = fromMaybe (DS.riskTakeProfitPercent oldRisk) (updateTakeProfitPercent req)
                  , DS.riskKellyFraction = fromMaybe (DS.riskKellyFraction oldRisk) (updateKellyFraction req)
                  , DS.riskRebalanceEnabled = fromMaybe (DS.riskRebalanceEnabled oldRisk) (updateRebalanceEnabled req)
                  , DS.riskMinRebalanceImprovement = fromMaybe (DS.riskMinRebalanceImprovement oldRisk) (updateMinRebalanceImprovement req)
                  }
                }
          _updatedSettings <- updateSettings uid newSettings
          pure $ toSettingsResponse newSettings

    -- POST /settings/credentials/okx
    updateOKXCredentialsHandler mAuthHeader req = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ CredentialsResponse False (Just "Unauthorized") Nothing
        Just uid -> do
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

    -- POST /settings/credentials/bybit
    updateBybitCredentialsHandler mAuthHeader req = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ CredentialsResponse False (Just "Unauthorized") Nothing
        Just uid -> do
          let creds = DS.BybitCredentials
                { DS.bybitApiKeyId = ApiKeyId $ read "00000000-0000-0000-0000-000000000001"
                , DS.bybitApiKey = bybitReqApiKey req
                , DS.bybitApiSecret = bybitReqApiSecret req
                , DS.bybitTestnet = bybitReqTestnet req
                }
          success <- saveBybitCredentials uid creds
          pure $ CredentialsResponse
            { credSuccess = success
            , credError = if success then Nothing else Just "Failed to save Bybit credentials"
            , credMessage = Just "Bybit credentials saved successfully"
            }

    -- POST /settings/credentials/tbank
    updateTBankCredentialsHandler mAuthHeader req = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ CredentialsResponse False (Just "Unauthorized") Nothing
        Just uid -> do
          existingCreds <- getTBankCredentials uid
          let creds = DS.TBankCredentials
                { DS.tbankSandboxToken = tbankReqSandboxToken req
                , DS.tbankRealToken = tbankReqRealToken req
                , DS.tbankSandboxAccounts = maybe [] DS.tbankSandboxAccounts existingCreds
                , DS.tbankDefaultSandboxAccount = DS.tbankDefaultSandboxAccount =<< existingCreds
                , DS.tbankRealTradingEnabled = tbankReqEnableRealTrading req
                }
          
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
    getTBankSandboxAccountsHandler mAuthHeader = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ TBankSandboxAccountsResponse [] Nothing
        Just uid -> do
          accounts <- getTBankSandboxAccounts uid
          mCreds <- getTBankCredentials uid
          pure $ TBankSandboxAccountsResponse
            { tsarAccounts = accounts
            , tsarDefaultAccountId = DS.tbankDefaultSandboxAccount =<< mCreds
            }

    -- POST /settings/tbank/sandbox/accounts
    saveTBankSandboxAccountHandler mAuthHeader req = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ CredentialsResponse False (Just "Unauthorized") Nothing
        Just uid -> do
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
    setDefaultTBankSandboxAccountHandler mAuthHeader req = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ CredentialsResponse False (Just "Unauthorized") Nothing
        Just uid -> do
          success <- setDefaultTBankSandboxAccount uid (sdarAccountId req)
          pure $ CredentialsResponse
            { credSuccess = success
            , credError = if success then Nothing else Just "Failed to set default account"
            , credMessage = Just "Default sandbox account updated"
            }

    -- POST /settings/broker
    setBrokerHandler mAuthHeader req = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ errorSettingsResponse
        Just uid -> do
          _ <- setSelectedBroker uid (sbrBroker req)
          _ <- setUseSandbox uid (sbrUseSandbox req)
          settings <- getSettings uid
          pure $ toSettingsResponse settings

    -- GET /settings/broker/config
    getBrokerConfigHandler mAuthHeader = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure Nothing
        Just uid -> do
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
            Just (BybitConfig _ _ testnet) -> pure $ Just $ BrokerConfigResponse
              { bcrBroker = Bybit
              , bcrMode = if testnet then Sandbox else Real
              , bcrIsConfigured = True
              }
            Nothing -> pure Nothing

    errorSettingsResponse :: SettingsResponse
    errorSettingsResponse = SettingsResponse
      { settingsRiskMaxLossPercent = 0
      , settingsRiskMaxPositionSize = 0
      , settingsRiskMaxOpenPositions = 0
      , settingsRiskAutoModeEnabled = False
      , settingsRiskTakeProfitPercent = 50
      , settingsRiskKellyFraction = 0.5
      , settingsRiskRebalanceEnabled = True
      , settingsRiskMinRebalanceImprovement = 0.10
      , settingsSelectedBroker = DS.BrokerNone
      , settingsUseSandbox = True
      , settingsHasOKXCredentials = False
      , settingsHasTBankSandboxToken = False
      , settingsHasTBankRealToken = False
      , settingsTBankRealTradingEnabled = False
      , settingsHasBybitCredentials = False
      , settingsHasActiveBroker = False
      , settingsSupportedBybitCoins = DS.supportedBybitOptionCoins
      }

    toSettingsResponse :: DS.Settings -> SettingsResponse
    toSettingsResponse settings = SettingsResponse
      { settingsRiskMaxLossPercent = DS.riskMaxLossPercent $ DS.settingsRiskParams settings
      , settingsRiskMaxPositionSize = DS.riskMaxPositionSize $ DS.settingsRiskParams settings
      , settingsRiskMaxOpenPositions = DS.riskMaxOpenPositions $ DS.settingsRiskParams settings
      , settingsRiskAutoModeEnabled = DS.riskAutoModeEnabled $ DS.settingsRiskParams settings
      , settingsRiskTakeProfitPercent = DS.riskTakeProfitPercent $ DS.settingsRiskParams settings
      , settingsRiskKellyFraction = DS.riskKellyFraction $ DS.settingsRiskParams settings
      , settingsRiskRebalanceEnabled = DS.riskRebalanceEnabled $ DS.settingsRiskParams settings
      , settingsRiskMinRebalanceImprovement = DS.riskMinRebalanceImprovement $ DS.settingsRiskParams settings
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
      , settingsHasBybitCredentials = case DS.settingsBybitCredentials settings of
          Just _ -> True
          Nothing -> False
      , settingsHasActiveBroker = DS.hasActiveBroker settings
      , settingsSupportedBybitCoins = DS.supportedBybitOptionCoins
      }
