{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Effects.Settings
  ( SettingsEffect (..)
  , getSettings
  , updateSettings
  , saveOKXCredentials
  , getOKXCredentials
  , deleteOKXCredentials
  , saveTBankCredentials
  , getTBankCredentials
  , deleteTBankCredentials
  , saveBybitCredentials
  , getBybitCredentials
  , deleteBybitCredentials
  , saveTBankSandboxToken
  , saveTBankSandboxAccount
  , getTBankSandboxAccounts
  , setDefaultTBankSandboxAccount
  , enableTBankRealTrading
  , setSelectedBroker
  , setUseSandbox
  , runSettingsWithPool
  , runSettingsIO
  , initializeDatabase
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (ConnectionPool, Entity(..))
import Domain.Settings
  ( BrokerPreference (..)
  , BybitCredentials (..)
  , OKXCredentials (..)
  , RiskParameters (..)
  , SelectedBroker (..)
  , Settings (..)
  , TBankCredentials (..)
  , TBankSandboxInfo (..)
  , defaultRiskParams
  , defaultSettings
  )
import Domain.Types (UserId (..))
import Infrastructure.Encryption (EncryptionContext)
import Infrastructure.Persistence (initializeDatabase, getUserSettings, saveUserSettings, UserSettings(..))
import qualified Infrastructure.Persistence as Persistence
import Polysemy
import Polysemy.Embed

data SettingsEffect m a where
  GetSettings :: UserId -> SettingsEffect m Settings
  UpdateSettings :: UserId -> Settings -> SettingsEffect m Settings
  SaveOKXCredentials :: UserId -> OKXCredentials -> SettingsEffect m Bool
  GetOKXCredentials :: UserId -> SettingsEffect m (Maybe OKXCredentials)
  DeleteOKXCredentials :: UserId -> SettingsEffect m Bool
  SaveTBankCredentials :: UserId -> TBankCredentials -> SettingsEffect m Bool
  GetTBankCredentials :: UserId -> SettingsEffect m (Maybe TBankCredentials)
  DeleteTBankCredentials :: UserId -> SettingsEffect m Bool
  SaveBybitCredentials :: UserId -> BybitCredentials -> SettingsEffect m Bool
  GetBybitCredentials :: UserId -> SettingsEffect m (Maybe BybitCredentials)
  DeleteBybitCredentials :: UserId -> SettingsEffect m Bool
  SaveTBankSandboxToken :: UserId -> Text -> SettingsEffect m Bool
  SaveTBankSandboxAccount :: UserId -> TBankSandboxInfo -> SettingsEffect m Bool
  GetTBankSandboxAccounts :: UserId -> SettingsEffect m [TBankSandboxInfo]
  SetDefaultTBankSandboxAccount :: UserId -> Text -> SettingsEffect m Bool
  EnableTBankRealTrading :: UserId -> Bool -> SettingsEffect m Bool
  SetSelectedBroker :: UserId -> SelectedBroker -> SettingsEffect m Bool
  SetUseSandbox :: UserId -> Bool -> SettingsEffect m Bool

makeSem ''SettingsEffect

riskFromEntity :: UserSettings -> RiskParameters
riskFromEntity settings = RiskParameters
  { riskMaxLossPercent = fromRational $ toRational $ userSettingsMaxLossPercent settings
  , riskMaxPositionSize = fromRational $ toRational $ userSettingsMaxPositionSize settings
  , riskMaxOpenPositions = userSettingsMaxOpenPositions settings
  , riskAutoModeEnabled = userSettingsAutoModeEnabled settings
  , riskTakeProfitPercent = fromRational $ toRational $ userSettingsTakeProfitPercent settings
  , riskRebalanceEnabled = userSettingsRebalanceEnabled settings
  , riskMinRebalanceImprovement = fromRational $ toRational $ userSettingsMinRebalanceImprovement settings
  }

textToSelectedBroker :: Text -> SelectedBroker
textToSelectedBroker "okx" = BrokerOKX
textToSelectedBroker "tbank" = BrokerTBank
textToSelectedBroker "bybit" = BrokerBybit
textToSelectedBroker _ = BrokerNone

persistenceEntityToSettings :: Entity UserSettings -> IO Settings
persistenceEntityToSettings (Entity _ settings) = do
  let uid = UserId $ read $ Text.unpack $ userSettingsUserId settings
  pure $ Settings
    { settingsUserId = uid
    , settingsBrokerPreference = BrokerPreference
        { bpSelectedBroker = textToSelectedBroker $ userSettingsSelectedBroker settings
        , bpUseSandbox = userSettingsUseSandbox settings
        }
    , settingsOKXCredentials = Nothing
    , settingsTBankCredentials = Nothing
    , settingsBybitCredentials = Nothing
    , settingsRiskParams = riskFromEntity settings
    }

runSettingsWithPool :: Members '[Embed IO] r => ConnectionPool -> EncryptionContext -> Sem (SettingsEffect ': r) a -> Sem r a
runSettingsWithPool pool encCtx = interpret $ \case
  GetSettings uid -> embed @IO $ do
    mSettingsEntity <- Persistence.getUserSettings pool uid
    mOkx <- Persistence.getOKXCredentials pool encCtx uid
    mTbank <- Persistence.getTBankCredentials pool encCtx uid
    mBybit <- Persistence.getBybitCredentials pool encCtx uid
    base <- case mSettingsEntity of
      Just entity -> persistenceEntityToSettings entity
      Nothing -> pure $ defaultSettings uid
    pure base
      { settingsOKXCredentials = mOkx
      , settingsTBankCredentials = mTbank
      , settingsBybitCredentials = mBybit
      }

  UpdateSettings uid newSettings -> embed @IO $ do
    _ <- Persistence.saveUserSettings pool
           uid
           (bpSelectedBroker $ settingsBrokerPreference newSettings)
           (bpUseSandbox $ settingsBrokerPreference newSettings)
           (settingsRiskParams newSettings)
    pure newSettings

  SaveOKXCredentials uid creds -> embed @IO $ do
    Persistence.saveOKXCredentials pool encCtx uid creds
    pure True

  GetOKXCredentials uid -> embed @IO $
    Persistence.getOKXCredentials pool encCtx uid

  DeleteOKXCredentials uid -> embed @IO $ do
    Persistence.deleteOKXCredentials pool uid
    pure True

  SaveTBankCredentials uid creds -> embed @IO $ do
    Persistence.saveTBankCredentials pool encCtx uid creds
    pure True

  GetTBankCredentials uid -> embed @IO $
    Persistence.getTBankCredentials pool encCtx uid

  DeleteTBankCredentials uid -> embed @IO $ do
    Persistence.deleteTBankCredentials pool uid
    pure True

  SaveBybitCredentials uid creds -> embed @IO $ do
    Persistence.saveBybitCredentials pool encCtx uid creds
    pure True

  GetBybitCredentials uid -> embed @IO $
    Persistence.getBybitCredentials pool encCtx uid

  DeleteBybitCredentials uid -> embed @IO $ do
    Persistence.deleteBybitCredentials pool uid
    pure True

  SaveTBankSandboxToken uid token -> embed @IO $ do
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

  GetTBankSandboxAccounts uid -> embed @IO $
    Persistence.getTBankSandboxAccounts pool uid

  SetDefaultTBankSandboxAccount uid accountId -> embed @IO $ do
    Persistence.setDefaultTBankSandboxAccount pool uid accountId
    pure True

  EnableTBankRealTrading uid enabled -> embed @IO $ do
    mCreds <- Persistence.getTBankCredentials pool encCtx uid
    case mCreds of
      Just creds -> do
        Persistence.saveTBankCredentials pool encCtx uid (creds { tbankRealTradingEnabled = enabled })
        pure True
      Nothing -> pure False

  SetSelectedBroker uid broker -> embed @IO $ do
    mSettingsEntity <- Persistence.getUserSettings pool uid
    case mSettingsEntity of
      Just (Entity _ settings) -> do
        _ <- Persistence.saveUserSettings pool uid broker (userSettingsUseSandbox settings) (riskFromEntity settings)
        pure True
      Nothing -> do
        _ <- Persistence.saveUserSettings pool uid broker True defaultRiskParams
        pure True

  SetUseSandbox uid useSandbox -> embed @IO $ do
    mSettingsEntity <- Persistence.getUserSettings pool uid
    case mSettingsEntity of
      Just (Entity _ settings) -> do
        let currentBroker = textToSelectedBroker $ userSettingsSelectedBroker settings
        _ <- Persistence.saveUserSettings pool uid currentBroker useSandbox (riskFromEntity settings)
        pure True
      Nothing -> pure True

runSettingsIO :: Members '[Embed IO] r => Sem (SettingsEffect ': r) a -> Sem r a
runSettingsIO = interpret $ \case
  GetSettings uid -> embed @IO $ pure $ defaultSettings uid
  UpdateSettings _ newSettings -> embed @IO $ pure newSettings
  SaveOKXCredentials _ _ -> embed @IO $ pure True
  GetOKXCredentials _ -> embed @IO $ pure Nothing
  DeleteOKXCredentials _ -> embed @IO $ pure True
  SaveTBankCredentials _ _ -> embed @IO $ pure True
  GetTBankCredentials _ -> embed @IO $ pure Nothing
  DeleteTBankCredentials _ -> embed @IO $ pure True
  SaveBybitCredentials _ _ -> embed @IO $ pure True
  GetBybitCredentials _ -> embed @IO $ pure Nothing
  DeleteBybitCredentials _ -> embed @IO $ pure True
  SaveTBankSandboxToken _ _ -> embed @IO $ pure True
  SaveTBankSandboxAccount _ _ -> embed @IO $ pure True
  GetTBankSandboxAccounts _ -> embed @IO $ pure []
  SetDefaultTBankSandboxAccount _ _ -> embed @IO $ pure True
  EnableTBankRealTrading _ _ -> embed @IO $ pure True
  SetSelectedBroker _ _ -> embed @IO $ pure True
  SetUseSandbox _ _ -> embed @IO $ pure True
