{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Effects.Settings
  ( -- * Settings Effect
    SettingsEffect (..)
    -- * General Settings
  , getSettings
  , updateSettings
    -- * OKX Credentials
  , saveOKXCredentials
  , getOKXCredentials
  , deleteOKXCredentials
    -- * T-Bank Credentials
  , saveTBankCredentials
  , getTBankCredentials
  , deleteTBankCredentials
    -- * T-Bank Sandbox Management
  , saveTBankSandboxToken
  , saveTBankSandboxAccount
  , getTBankSandboxAccounts
  , setDefaultTBankSandboxAccount
  , enableTBankRealTrading
    -- * Broker Selection
  , setSelectedBroker
  , setUseSandbox
    -- * Interpreters
  , runSettingsWithPool
  , runSettingsIO
    -- * Database initialization
  , initializeDatabase
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (ConnectionPool, Entity(..))
import Domain.Settings
  ( BrokerPreference (..)
  , OKXCredentials (..)
  , RiskParameters (..)
  , SelectedBroker (..)
  , Settings (..)
  , TBankCredentials (..)
  , TBankSandboxInfo (..)
  , defaultSettings
  )
import Domain.Types (UserId (..))
import Infrastructure.Encryption (EncryptionContext, initializeEncryption, encryptCredential, decryptCredential)
import Infrastructure.Persistence (initializeDatabase, getUserSettings, saveUserSettings, UserSettings(..))
import qualified Infrastructure.Persistence as Persistence
import Polysemy
import Polysemy.Embed

-- ============================================================================
-- Settings Effect Definition
-- ============================================================================

data SettingsEffect m a where
  -- General settings
  GetSettings :: UserId -> SettingsEffect m Settings
  UpdateSettings :: UserId -> Settings -> SettingsEffect m Settings
  
  -- OKX credentials
  SaveOKXCredentials :: UserId -> OKXCredentials -> SettingsEffect m Bool
  GetOKXCredentials :: UserId -> SettingsEffect m (Maybe OKXCredentials)
  DeleteOKXCredentials :: UserId -> SettingsEffect m Bool
  
  -- T-Bank credentials
  SaveTBankCredentials :: UserId -> TBankCredentials -> SettingsEffect m Bool
  GetTBankCredentials :: UserId -> SettingsEffect m (Maybe TBankCredentials)
  DeleteTBankCredentials :: UserId -> SettingsEffect m Bool
  
  -- T-Bank sandbox management
  SaveTBankSandboxToken :: UserId -> Text -> SettingsEffect m Bool
  SaveTBankSandboxAccount :: UserId -> TBankSandboxInfo -> SettingsEffect m Bool
  GetTBankSandboxAccounts :: UserId -> SettingsEffect m [TBankSandboxInfo]
  SetDefaultTBankSandboxAccount :: UserId -> Text -> SettingsEffect m Bool
  EnableTBankRealTrading :: UserId -> Bool -> SettingsEffect m Bool
  
  -- Broker selection
  SetSelectedBroker :: UserId -> SelectedBroker -> SettingsEffect m Bool
  SetUseSandbox :: UserId -> Bool -> SettingsEffect m Bool

makeSem ''SettingsEffect

-- ============================================================================
-- IO Interpreter with Database Pool
-- ============================================================================

runSettingsWithPool :: Members '[Embed IO] r => ConnectionPool -> EncryptionContext -> Sem (SettingsEffect ': r) a -> Sem r a
runSettingsWithPool pool encCtx = interpret $ \case
  -- General settings
  GetSettings uid -> embed @IO $ do
    mSettingsEntity <- Persistence.getUserSettings pool uid
    case mSettingsEntity of
      Just entity -> persistenceEntityToSettings entity
      Nothing -> pure $ defaultSettings uid

  UpdateSettings uid newSettings -> embed @IO $ do
    _ <- Persistence.saveUserSettings pool 
           uid 
           (bpSelectedBroker $ settingsBrokerPreference newSettings)
           (bpUseSandbox $ settingsBrokerPreference newSettings)
           (settingsRiskParams newSettings)
    pure newSettings

  -- OKX credentials
  SaveOKXCredentials uid creds -> embed @IO $ do
    Persistence.saveOKXCredentials pool encCtx uid creds
    pure True

  GetOKXCredentials uid -> embed @IO $ do
    Persistence.getOKXCredentials pool encCtx uid

  DeleteOKXCredentials uid -> embed @IO $ do
    Persistence.deleteOKXCredentials pool uid
    pure True

  -- T-Bank credentials
  SaveTBankCredentials uid creds -> embed @IO $ do
    Persistence.saveTBankCredentials pool encCtx uid creds
    pure True

  GetTBankCredentials uid -> embed @IO $ do
    Persistence.getTBankCredentials pool encCtx uid

  DeleteTBankCredentials uid -> embed @IO $ do
    Persistence.deleteTBankCredentials pool uid
    pure True

  -- T-Bank sandbox management
  SaveTBankSandboxToken uid token -> embed @IO $ do
    -- Get existing credentials
    mCreds <- Persistence.getTBankCredentials pool encCtx uid
    let newCreds = case mCreds of
          Just creds -> creds { tbankSandboxToken = Just token }
          Nothing -> TBankCredentials
            { tbankSandboxToken = Just token
            , tbankRealToken = Nothing
            , tbankSandboxAccounts = []
            , tbankDefaultSandboxAccount = Nothing
            , tbankRealTradingEnabled = False
            }
    Persistence.saveTBankCredentials pool encCtx uid newCreds
    pure True

  SaveTBankSandboxAccount uid accInfo -> embed @IO $ do
    Persistence.saveTBankSandboxAccount pool uid accInfo
    pure True

  GetTBankSandboxAccounts uid -> embed @IO $ do
    Persistence.getTBankSandboxAccounts pool uid

  SetDefaultTBankSandboxAccount uid accountId -> embed @IO $ do
    Persistence.setDefaultTBankSandboxAccount pool uid accountId
    pure True

  EnableTBankRealTrading uid enabled -> embed @IO $ do
    mCreds <- Persistence.getTBankCredentials pool encCtx uid
    case mCreds of
      Just creds -> do
        let newCreds = creds { tbankRealTradingEnabled = enabled }
        Persistence.saveTBankCredentials pool encCtx uid newCreds
        pure True
      Nothing -> pure False

  -- Broker selection
  SetSelectedBroker uid broker -> embed @IO $ do
    mSettingsEntity <- Persistence.getUserSettings pool uid
    case mSettingsEntity of
      Just (Entity _ settings) -> do
        let currentBroker = userSettingsSelectedBroker settings
            currentUseSandbox = userSettingsUseSandbox settings
            riskParams = RiskParameters
              { riskMaxLossPercent = fromRational $ toRational $ userSettingsMaxLossPercent settings
              , riskMaxPositionSize = fromRational $ toRational $ userSettingsMaxPositionSize settings
              , riskMaxOpenPositions = userSettingsMaxOpenPositions settings
              , riskAutoModeEnabled = userSettingsAutoModeEnabled settings
              }
        _ <- Persistence.saveUserSettings pool uid broker currentUseSandbox riskParams
        pure True
      Nothing -> do
        -- Create new settings
        let riskParams = RiskParameters 2.0 1000.0 5 False
        _ <- Persistence.saveUserSettings pool uid broker True riskParams
        pure True

  SetUseSandbox uid useSandbox -> embed @IO $ do
    mSettingsEntity <- Persistence.getUserSettings pool uid
    case mSettingsEntity of
      Just (Entity _ settings) -> do
        let currentBroker = textToSelectedBroker $ userSettingsSelectedBroker settings
            riskParams = RiskParameters
              { riskMaxLossPercent = fromRational $ toRational $ userSettingsMaxLossPercent settings
              , riskMaxPositionSize = fromRational $ toRational $ userSettingsMaxPositionSize settings
              , riskMaxOpenPositions = userSettingsMaxOpenPositions settings
              , riskAutoModeEnabled = userSettingsAutoModeEnabled settings
              }
        _ <- Persistence.saveUserSettings pool uid currentBroker useSandbox riskParams
        pure True
      Nothing -> pure True
  where
    persistenceEntityToSettings (Entity _ settings) = pure $ Settings
      { settingsUserId = UserId $ read $ Text.unpack $ userSettingsUserId settings
      , settingsBrokerPreference = BrokerPreference
          { bpSelectedBroker = textToSelectedBroker $ userSettingsSelectedBroker settings
          , bpUseSandbox = userSettingsUseSandbox settings
          }
      , settingsOKXCredentials = Nothing  -- Will be fetched separately
      , settingsTBankCredentials = Nothing  -- Will be fetched separately
      , settingsRiskParams = RiskParameters
          { riskMaxLossPercent = fromRational $ toRational $ userSettingsMaxLossPercent settings
          , riskMaxPositionSize = fromRational $ toRational $ userSettingsMaxPositionSize settings
          , riskMaxOpenPositions = userSettingsMaxOpenPositions settings
          , riskAutoModeEnabled = userSettingsAutoModeEnabled settings
          }
      }
    
    textToSelectedBroker "okx" = BrokerOKX
    textToSelectedBroker "tbank" = BrokerTBank
    textToSelectedBroker _ = BrokerNone

-- ============================================================================
-- Legacy IO Interpreter (without database - for testing/development)
-- ============================================================================

runSettingsIO :: Members '[Embed IO] r => Sem (SettingsEffect ': r) a -> Sem r a
runSettingsIO = interpret $ \case
  GetSettings uid -> embed @IO $ pure $ defaultSettings uid
  UpdateSettings _ newSettings -> embed @IO $ pure newSettings
  SaveOKXCredentials _ _ -> embed @IO $ pure True
  GetOKXCredentials _ -> embed @IO $ pure (Nothing :: Maybe OKXCredentials)
  DeleteOKXCredentials _ -> embed @IO $ pure True
  SaveTBankCredentials _ _ -> embed @IO $ pure True
  GetTBankCredentials _ -> embed @IO $ pure (Nothing :: Maybe TBankCredentials)
  DeleteTBankCredentials _ -> embed @IO $ pure True
  SaveTBankSandboxToken _ _ -> embed @IO $ pure True
  SaveTBankSandboxAccount _ _ -> embed @IO $ pure True
  GetTBankSandboxAccounts _ -> embed @IO $ pure ([] :: [TBankSandboxInfo])
  SetDefaultTBankSandboxAccount _ _ -> embed @IO $ pure True
  EnableTBankRealTrading _ _ -> embed @IO $ pure True
  SetSelectedBroker _ _ -> embed @IO $ pure True
  SetUseSandbox _ _ -> embed @IO $ pure True
