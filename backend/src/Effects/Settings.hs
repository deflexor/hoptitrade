{-# LANGUAGE TemplateHaskell #-}

module Effects.Settings
  ( SettingsEffect (..)
  , getSettings
  , updateSettings
  , saveOKXCredentials
  , getOKXCredentials
  , runSettingsIO
  ) where

import Domain.Settings (OKXCredentials (..), RiskParameters (..), Settings (..), defaultSettings)
import Domain.Types (UserId (..))
import Polysemy
import Polysemy.Embed

-- ============================================================================
-- Settings Effect Definition
-- ============================================================================

data SettingsEffect m a where
  GetSettings :: UserId -> SettingsEffect m Settings
  UpdateSettings :: UserId -> Settings -> SettingsEffect m Settings
  SaveOKXCredentials :: UserId -> OKXCredentials -> SettingsEffect m Bool
  GetOKXCredentials :: UserId -> SettingsEffect m (Maybe OKXCredentials)

makeSem ''SettingsEffect

-- ============================================================================
-- IO Interpreter (Production)
-- ============================================================================

runSettingsIO :: Members '[Embed IO] r => Sem (SettingsEffect ': r) a -> Sem r a
runSettingsIO = interpret $ \case
  GetSettings uid -> embed $ do
    putStrLn $ "Getting settings for user: " ++ show uid
    -- TODO: Implement database fetch
    pure $ defaultSettings uid

  UpdateSettings uid newSettings -> embed $ do
    putStrLn $ "Updating settings for user: " ++ show uid
    -- TODO: Implement database update
    pure newSettings

  SaveOKXCredentials uid creds -> embed $ do
    putStrLn $ "Saving OKX credentials for user: " ++ show uid
    -- TODO: Implement secure credential storage (encryption)
    pure True

  GetOKXCredentials uid -> embed $ do
    putStrLn $ "Getting OKX credentials for user: " ++ show uid
    -- TODO: Implement secure credential retrieval (decryption)
    pure Nothing
