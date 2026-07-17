{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

module Domain.Settings
  ( Settings (..)
  , defaultSettings
  , maskCredentials
  , BrokerPreference (..)
  , SelectedBroker (..)
  , OKXCredentials (..)
  , TBankCredentials (..)
  , TBankSandboxInfo (..)
  , BybitCredentials (..)
  , RiskParameters (..)
  , defaultRiskParams
  , settingsToBrokerConfig
  , hasActiveBroker
  , supportedBybitOptionCoins
  , pattern OKXBroker
  , pattern TBankBroker
  , pattern BybitBroker
  , pattern NoBroker
  ) where

import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Domain.Broker as Broker
import Domain.Types (ApiKeyId (..), UserId (..))
import GHC.Generics (Generic)

data Settings = Settings
  { settingsUserId :: UserId
  , settingsBrokerPreference :: BrokerPreference
  , settingsOKXCredentials :: Maybe OKXCredentials
  , settingsTBankCredentials :: Maybe TBankCredentials
  , settingsBybitCredentials :: Maybe BybitCredentials
  , settingsRiskParams :: RiskParameters
  } deriving stock (Eq, Show, Generic)

instance FromJSON Settings
instance ToJSON Settings where
  toJSON settings = toJSON $ maskCredentials settings

data BrokerPreference = BrokerPreference
  { bpSelectedBroker :: SelectedBroker
  , bpUseSandbox :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SelectedBroker
  = BrokerOKX
  | BrokerTBank
  | BrokerBybit
  | BrokerNone
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

pattern OKXBroker :: SelectedBroker
pattern OKXBroker = BrokerOKX

pattern TBankBroker :: SelectedBroker
pattern TBankBroker = BrokerTBank

pattern BybitBroker :: SelectedBroker
pattern BybitBroker = BrokerBybit

pattern NoBroker :: SelectedBroker
pattern NoBroker = BrokerNone

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

data TBankCredentials = TBankCredentials
  { tbankSandboxToken :: Maybe Text
  , tbankRealToken :: Maybe Text
  , tbankSandboxAccounts :: [TBankSandboxInfo]
  , tbankDefaultSandboxAccount :: Maybe Text
  , tbankRealTradingEnabled :: Bool
  } deriving stock (Eq, Show, Generic)

instance FromJSON TBankCredentials
instance ToJSON TBankCredentials where
  toJSON _ = toJSON ("***REDACTED***" :: Text)

data TBankSandboxInfo = TBankSandboxInfo
  { tsiAccountId :: Text
  , tsiName :: Maybe Text
  , tsiBalance :: Maybe Scientific
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BybitCredentials = BybitCredentials
  { bybitApiKeyId :: ApiKeyId
  , bybitApiKey :: Text
  , bybitApiSecret :: Text
  , bybitTestnet :: Bool
  } deriving stock (Eq, Show, Generic)

instance FromJSON BybitCredentials
instance ToJSON BybitCredentials where
  toJSON _ = toJSON ("***REDACTED***" :: Text)

data RiskParameters = RiskParameters
  { riskMaxLossPercent :: Scientific
  , riskMaxPositionSize :: Scientific
  , riskMaxOpenPositions :: Int
  , riskAutoModeEnabled :: Bool  -- auto-open only; manage always runs
  , riskTakeProfitPercent :: Scientific  -- close when unrealized >= this % of max profit
  , riskRebalanceEnabled :: Bool
  , riskMinRebalanceImprovement :: Scientific  -- e.g. 0.10 = 10%
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

supportedBybitOptionCoins :: [Text]
supportedBybitOptionCoins = ["BTC", "SOL", "XAUT", "XRP", "MNT", "DOGE"]

defaultSettings :: UserId -> Settings
defaultSettings uid = Settings
  { settingsUserId = uid
  , settingsBrokerPreference = BrokerPreference BrokerNone True
  , settingsOKXCredentials = Nothing
  , settingsTBankCredentials = Nothing
  , settingsBybitCredentials = Nothing
  , settingsRiskParams = defaultRiskParams
  }

defaultRiskParams :: RiskParameters
defaultRiskParams = RiskParameters
  { riskMaxLossPercent = 2.0
  , riskMaxPositionSize = 1000.0
  , riskMaxOpenPositions = 5
  , riskAutoModeEnabled = False
  , riskTakeProfitPercent = 50.0
  , riskRebalanceEnabled = True
  , riskMinRebalanceImprovement = 0.10
  }

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
          token <- tbankSandboxToken creds
          accId <- tbankDefaultSandboxAccount creds
          return $ Broker.TBankConfig
            { Broker.tbankToken = token
            , Broker.tbankAccountId = Just accId
            , Broker.tbankMode = Broker.Sandbox
            }
        else do
          if not (tbankRealTradingEnabled creds)
            then Nothing
            else do
              token <- tbankRealToken creds
              return $ Broker.TBankConfig
                { Broker.tbankToken = token
                , Broker.tbankAccountId = Nothing
                , Broker.tbankMode = Broker.Real
                }
    BrokerBybit -> do
      creds <- settingsBybitCredentials settings
      return $ Broker.BybitConfig
        { Broker.bybitApiKey = bybitApiKey creds
        , Broker.bybitApiSecret = bybitApiSecret creds
        , Broker.bybitTestnet = bybitTestnet creds
        }
    BrokerNone -> Nothing

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
    BrokerBybit -> case settingsBybitCredentials settings of
      Just _ -> True
      Nothing -> False

maskCredentials :: Settings -> Settings
maskCredentials settings = settings
  { settingsOKXCredentials = Nothing
  , settingsTBankCredentials = Nothing
  , settingsBybitCredentials = Nothing
  }
