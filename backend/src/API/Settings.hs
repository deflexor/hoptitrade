{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Settings
  ( SettingsAPI
  , settingsServer
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Domain.Settings (OKXCredentials (..), RiskParameters (..), Settings (..))
import Domain.Types (ApiKeyId (..), UserId (..))
import Effects.Settings
import GHC.Generics (Generic)
import Polysemy
import Servant

-- ============================================================================
-- API Type
-- ============================================================================

type SettingsAPI =
  "settings" :> Get '[JSON] SettingsResponse
  :<|> "settings" :> ReqBody '[JSON] UpdateSettingsRequest :> Post '[JSON] SettingsResponse
  :<|> "settings" :> "credentials" :> ReqBody '[JSON] UpdateCredentialsRequest :> Post '[JSON] CredentialsResponse

-- ============================================================================
-- Request/Response Types
-- ============================================================================

data SettingsResponse = SettingsResponse
  { settingsRiskMaxLossPercent :: Scientific
  , settingsRiskMaxPositionSize :: Scientific
  , settingsRiskMaxOpenPositions :: Int
  , settingsRiskAutoModeEnabled :: Bool
  , settingsHasOKXCredentials :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UpdateSettingsRequest = UpdateSettingsRequest
  { updateMaxLossPercent :: Scientific
  , updateMaxPositionSize :: Scientific
  , updateMaxOpenPositions :: Int
  , updateAutoModeEnabled :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data UpdateCredentialsRequest = UpdateCredentialsRequest
  { credApiKey :: Text
  , credApiSecret :: Text
  , credPassphrase :: Text
  , credIsDemo :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CredentialsResponse = CredentialsResponse
  { credSuccess :: Bool
  , credError :: Maybe Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Server
-- ============================================================================

settingsServer :: Members '[SettingsEffect, Embed IO] r => ServerT SettingsAPI (Sem r)
settingsServer = getSettingsHandler :<|> updateSettingsHandler :<|> updateCredentialsHandler
  where
    getSettingsHandler = do
      -- TODO: Get current user ID from auth context
      let uid = UserId $ read "550e8400-e29b-41d4-a716-446655440000"
      settings <- getSettings uid
      pure $ toSettingsResponse settings

    updateSettingsHandler req = do
      let uid = UserId $ read "550e8400-e29b-41d4-a716-446655440000"
      currentSettings <- getSettings uid
      let newSettings = currentSettings
            { settingsRiskParams = RiskParameters
              { riskMaxLossPercent = updateMaxLossPercent req
              , riskMaxPositionSize = updateMaxPositionSize req
              , riskMaxOpenPositions = updateMaxOpenPositions req
              , riskAutoModeEnabled = updateAutoModeEnabled req
              }
            }
      _updatedSettings <- updateSettings uid newSettings
      pure $ toSettingsResponse newSettings

    updateCredentialsHandler req = do
      let uid = UserId $ read "550e8400-e29b-41d4-a716-446655440000"
      let creds = OKXCredentials
            { okxApiKeyId = ApiKeyId $ read "00000000-0000-0000-0000-000000000000"
            , okxApiKey = credApiKey req
            , okxApiSecret = credApiSecret req
            , okxPassphrase = credPassphrase req
            , okxIsDemo = credIsDemo req
            }
      success <- saveOKXCredentials uid creds
      pure $ CredentialsResponse
        { credSuccess = success
        , credError = if success then Nothing else Just "Failed to save credentials"
        }

toSettingsResponse :: Settings -> SettingsResponse
toSettingsResponse settings = SettingsResponse
  { settingsRiskMaxLossPercent = riskMaxLossPercent $ settingsRiskParams settings
  , settingsRiskMaxPositionSize = riskMaxPositionSize $ settingsRiskParams settings
  , settingsRiskMaxOpenPositions = riskMaxOpenPositions $ settingsRiskParams settings
  , settingsRiskAutoModeEnabled = riskAutoModeEnabled $ settingsRiskParams settings
  , settingsHasOKXCredentials = case settingsOKXCredentials settings of
      Just _ -> True
      Nothing -> False
  }
