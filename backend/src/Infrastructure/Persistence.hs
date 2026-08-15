{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.Persistence
  ( -- * Database Initialization
    initializeDatabase
  , withDatabase
    -- * Settings Operations
  , getUserSettings
  , saveUserSettings
  , getOKXCredentials
  , saveOKXCredentials
  , deleteOKXCredentials
  , getTBankCredentials
  , saveTBankCredentials
  , deleteTBankCredentials
  , getBybitCredentials
  , saveBybitCredentials
  , deleteBybitCredentials
  , getTBankSandboxAccounts
  , saveTBankSandboxAccount
  , deleteTBankSandboxAccount
  , setDefaultTBankSandboxAccount
    -- * Position Operations
  , getPositions
  , getOpenPositions
  , getPositionById
  , getPositionLegs
  , savePosition
  , insertPositionLegs
  , trackPositionInstruments
  , updatePosition
  , deletePosition
  , hasPositionForInstrument
    -- * Database Types
  , UserSettingsId
  , UserSettings (..)
  , PositionEntityId
  , PositionEntity (..)
  , PositionLegEntityId
  , PositionLegEntity (..)
  , TBankSandboxAccountId
  , TBankSandboxAccount (..)
  , OKXCredential (..)
  , TBankCredential (..)
  , ActiveInstrumentId
  , ActiveInstrument (..)
  , migrateAll
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Domain.Broker (BrokerMode (..))
import Domain.Position (Position (..), PositionLeg (..))
import Domain.Settings
  ( BybitCredentials (..)
  , OKXCredentials (..)
  , RiskParameters (..)
  , SelectedBroker (..)
  , TBankCredentials (..)
  , TBankSandboxInfo (..)
  )
import Domain.Types (ApiKeyId (..), InstrumentId (..), OrderId (..), PositionId (..), PositionStatus (..), Quantity, Side (..), UserId (..))
import Infrastructure.Encryption (EncryptionContext, encryptCredential, decryptCredential)

-- ============================================================================
-- Database Schema
-- ============================================================================

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- User settings entity
UserSettings
    userId Text
    selectedBroker Text
    useSandbox Bool default=True
    maxLossPercent Double default=2.0
    maxPositionSize Double default=1000.0
    maxOpenPositions Int default=5
    autoModeEnabled Bool default=False
    takeProfitPercent Double default=50.0
    kellyFraction Double default=0.5
    rebalanceEnabled Bool default=True
    minRebalanceImprovement Double default=0.10
    UniqueUserSettings userId
    deriving Show Eq

-- OKX Credentials (encrypted at rest in production)
OKXCredential
    userId Text
    apiKeyId Text
    apiKey Text
    apiSecret Text
    passphrase Text
    isDemo Bool default=True
    UniqueOKXCredential userId
    deriving Show Eq

-- T-Bank Credentials (encrypted at rest in production)
TBankCredential
    userId Text
    sandboxToken Text Maybe
    realToken Text Maybe
    realTradingEnabled Bool default=False
    UniqueTBankCredential userId
    deriving Show Eq

-- Bybit Credentials (encrypted at rest)
BybitCredential
    userId Text
    apiKeyId Text
    apiKey Text
    apiSecret Text
    testnet Bool default=True
    UniqueBybitCredential userId
    deriving Show Eq

-- T-Bank Sandbox Accounts
TBankSandboxAccount
    userId Text
    accountId Text
    name Text Maybe
    balance Double Maybe
    isDefault Bool default=False
    UniqueTBankSandboxAccount userId accountId
    deriving Show Eq

-- Positions (open and closed)
PositionEntity
    positionId Text
    userId Text
    strategyId Text
    status Text
    realizedPL Double Maybe
    unrealizedPL Double Maybe
    marginUsed Double
    maxProfit Double Maybe
    maxLoss Double Maybe
    entryPremium Double Maybe
    entryPop Double Maybe
    openedAt UTCTime Maybe
    closedAt UTCTime Maybe
    notes Text Maybe
    UniquePositionEntity positionId
    deriving Show Eq

-- Position Legs (individual orders in a position)
PositionLegEntity
    positionId Text
    orderId Text
    instrumentId Text
    side Text
    quantity Double
    filledPrice Double
    filledAt UTCTime
    deriving Show Eq

-- Active position tracking (for duplicate detection)
ActiveInstrument
    userId Text
    instrumentId Text
    positionId Text
    openedAt UTCTime
    UniqueActiveInstrument userId instrumentId
    deriving Show Eq
|]

-- ============================================================================
-- Database Connection
-- ============================================================================

-- | Initialize the database and run migrations
initializeDatabase :: MonadIO m => Text -> m (ConnectionPool)
initializeDatabase dbPath = liftIO $ runNoLoggingT $ do
  pool <- createSqlitePool dbPath 5
  liftIO $ flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    -- Create indexes for performance
    rawExecute "CREATE INDEX IF NOT EXISTS idx_positions_user ON position_entity(user_id)" []
    rawExecute "CREATE INDEX IF NOT EXISTS idx_positions_status ON position_entity(status)" []
    rawExecute "CREATE INDEX IF NOT EXISTS idx_active_instruments_user ON active_instrument(user_id)" []
  return pool

-- | Run a database action with the connection pool
withDatabase :: MonadIO m => ConnectionPool -> SqlPersistT (NoLoggingT IO) a -> m a
withDatabase pool action = liftIO $ runNoLoggingT $ runSqlPool action pool

-- ============================================================================
-- Settings Operations
-- ============================================================================

-- | Get settings for a user
getUserSettings :: MonadIO m => ConnectionPool -> UserId -> m (Maybe (Entity UserSettings))
getUserSettings pool (UserId uid) = withDatabase pool $ do
  getBy $ UniqueUserSettings (Text.pack $ show uid)

-- | Save or update user settings
saveUserSettings :: MonadIO m => ConnectionPool -> UserId -> SelectedBroker -> Bool -> RiskParameters -> m (Key UserSettings)
saveUserSettings pool (UserId uid) broker useSandbox RiskParameters{..} = withDatabase pool $ do
  let userIdText = Text.pack $ show uid
      brokerText = case broker of
        BrokerOKX -> "okx"
        BrokerTBank -> "tbank"
        BrokerBybit -> "bybit"
        BrokerNone -> "none"
  
  mExisting <- getBy $ UniqueUserSettings userIdText
  case mExisting of
    Just (Entity key _) -> do
      update key
        [ UserSettingsSelectedBroker =. brokerText
        , UserSettingsUseSandbox =. useSandbox
        , UserSettingsMaxLossPercent =. realToFrac riskMaxLossPercent
        , UserSettingsMaxPositionSize =. realToFrac riskMaxPositionSize
        , UserSettingsMaxOpenPositions =. riskMaxOpenPositions
        , UserSettingsAutoModeEnabled =. riskAutoModeEnabled
        , UserSettingsTakeProfitPercent =. realToFrac riskTakeProfitPercent
        , UserSettingsKellyFraction =. realToFrac riskKellyFraction
        , UserSettingsRebalanceEnabled =. riskRebalanceEnabled
        , UserSettingsMinRebalanceImprovement =. realToFrac riskMinRebalanceImprovement
        ]
      return key
    Nothing -> do
      insert $ UserSettings
        { userSettingsUserId = userIdText
        , userSettingsSelectedBroker = brokerText
        , userSettingsUseSandbox = useSandbox
        , userSettingsMaxLossPercent = realToFrac riskMaxLossPercent
        , userSettingsMaxPositionSize = realToFrac riskMaxPositionSize
        , userSettingsMaxOpenPositions = riskMaxOpenPositions
        , userSettingsAutoModeEnabled = riskAutoModeEnabled
        , userSettingsTakeProfitPercent = realToFrac riskTakeProfitPercent
        , userSettingsKellyFraction = realToFrac riskKellyFraction
        , userSettingsRebalanceEnabled = riskRebalanceEnabled
        , userSettingsMinRebalanceImprovement = realToFrac riskMinRebalanceImprovement
        }

-- | Get OKX credentials for a user (decrypts sensitive fields)
getOKXCredentials :: MonadIO m => ConnectionPool -> EncryptionContext -> UserId -> m (Maybe OKXCredentials)
getOKXCredentials pool encCtx (UserId uid) = liftIO $ withDatabase pool $ do
  mEntity <- getBy $ UniqueOKXCredential (Text.pack $ show uid)
  case mEntity of
    Nothing -> pure Nothing
    Just (Entity _ OKXCredential{..}) -> liftIO $ do
      -- Decrypt sensitive fields
      mApiKey <- decryptCredential encCtx oKXCredentialApiKey
      mApiSecret <- decryptCredential encCtx oKXCredentialApiSecret
      mPassphrase <- decryptCredential encCtx oKXCredentialPassphrase
      
      case (mApiKey, mApiSecret, mPassphrase) of
        (Just apiKey, Just apiSecret, Just passphrase) ->
          pure $ Just $ OKXCredentials
            { okxApiKeyId = ApiKeyId $ read $ Text.unpack oKXCredentialApiKeyId
            , okxApiKey = apiKey
            , okxApiSecret = apiSecret
            , okxPassphrase = passphrase
            , okxIsDemo = oKXCredentialIsDemo
            }
        _ -> do
          putStrLn "WARNING: Failed to decrypt OKX credentials"
          pure Nothing

-- | Save OKX credentials for a user (encrypts sensitive fields)
saveOKXCredentials :: MonadIO m => ConnectionPool -> EncryptionContext -> UserId -> OKXCredentials -> m ()
saveOKXCredentials pool encCtx (UserId uid) OKXCredentials{..} = liftIO $ withDatabase pool $ do
  let userIdText = Text.pack $ show uid
  
  -- Encrypt sensitive fields
  encApiKey <- liftIO $ encryptCredential encCtx okxApiKey
  encApiSecret <- liftIO $ encryptCredential encCtx okxApiSecret
  encPassphrase <- liftIO $ encryptCredential encCtx okxPassphrase
  
  mExisting <- getBy $ UniqueOKXCredential userIdText
  case mExisting of
    Just (Entity key _) -> do
      update key
        [ OKXCredentialApiKeyId =. Text.pack (show $ apiKeyIdToText okxApiKeyId)
        , OKXCredentialApiKey =. encApiKey
        , OKXCredentialApiSecret =. encApiSecret
        , OKXCredentialPassphrase =. encPassphrase
        , OKXCredentialIsDemo =. okxIsDemo
        ]
    Nothing -> do
      _ <- insert $ OKXCredential
        { oKXCredentialUserId = userIdText
        , oKXCredentialApiKeyId = Text.pack (show $ apiKeyIdToText okxApiKeyId)
        , oKXCredentialApiKey = encApiKey
        , oKXCredentialApiSecret = encApiSecret
        , oKXCredentialPassphrase = encPassphrase
        , oKXCredentialIsDemo = okxIsDemo
        }
      return ()
  where
    apiKeyIdToText (ApiKeyId t) = t

-- | Delete OKX credentials for a user
deleteOKXCredentials :: MonadIO m => ConnectionPool -> UserId -> m ()
deleteOKXCredentials pool (UserId uid) = withDatabase pool $ do
  mEntity <- getBy $ UniqueOKXCredential (Text.pack $ show uid)
  case mEntity of
    Just (Entity key _) -> delete key
    Nothing -> return ()

-- | Get T-Bank credentials for a user (decrypts tokens)
getTBankCredentials :: MonadIO m => ConnectionPool -> EncryptionContext -> UserId -> m (Maybe TBankCredentials)
getTBankCredentials pool encCtx (UserId uid) = liftIO $ withDatabase pool $ do
  mCred <- getBy $ UniqueTBankCredential (Text.pack $ show uid)
  accounts <- selectList [TBankSandboxAccountUserId ==. Text.pack (show uid)] []
  case mCred of
    Nothing -> pure Nothing
    Just (Entity _ cred) -> liftIO $ do
      -- Decrypt tokens
      mSandboxToken <- case tBankCredentialSandboxToken cred of
        Nothing -> pure Nothing
        Just token -> decryptCredential encCtx token
      mRealToken <- case tBankCredentialRealToken cred of
        Nothing -> pure Nothing
        Just token -> decryptCredential encCtx token
      
      pure $ Just $ TBankCredentials
        { tbankSandboxToken = mSandboxToken
        , tbankRealToken = mRealToken
        , tbankSandboxAccounts = map (entityToSandboxInfo . entityVal) accounts
        , tbankDefaultSandboxAccount = listToMaybe 
            $ map (tBankSandboxAccountAccountId . entityVal)
            $ filter (tBankSandboxAccountIsDefault . entityVal) accounts
        , tbankRealTradingEnabled = tBankCredentialRealTradingEnabled cred
        }
  where
    entityToSandboxInfo TBankSandboxAccount{..} = TBankSandboxInfo
      { tsiAccountId = tBankSandboxAccountAccountId
      , tsiName = tBankSandboxAccountName
      , tsiBalance = realToFrac <$> tBankSandboxAccountBalance
      }

-- | Save T-Bank credentials for a user (encrypts tokens)
saveTBankCredentials :: MonadIO m => ConnectionPool -> EncryptionContext -> UserId -> TBankCredentials -> m ()
saveTBankCredentials pool encCtx (UserId uid) TBankCredentials{..} = liftIO $ withDatabase pool $ do
  let userIdText = Text.pack $ show uid
  
  -- Encrypt tokens
  encSandboxToken <- case tbankSandboxToken of
    Nothing -> pure Nothing
    Just token -> Just <$> liftIO (encryptCredential encCtx token)
  encRealToken <- case tbankRealToken of
    Nothing -> pure Nothing
    Just token -> Just <$> liftIO (encryptCredential encCtx token)
  
  mExisting <- getBy $ UniqueTBankCredential userIdText
  case mExisting of
    Just (Entity key _) -> do
      update key
        [ TBankCredentialSandboxToken =. encSandboxToken
        , TBankCredentialRealToken =. encRealToken
        , TBankCredentialRealTradingEnabled =. tbankRealTradingEnabled
        ]
    Nothing -> do
      _ <- insert $ TBankCredential
        { tBankCredentialUserId = userIdText
        , tBankCredentialSandboxToken = encSandboxToken
        , tBankCredentialRealToken = tbankRealToken
        , tBankCredentialRealTradingEnabled = tbankRealTradingEnabled
        }
      return ()

-- | Delete T-Bank credentials for a user
deleteTBankCredentials :: MonadIO m => ConnectionPool -> UserId -> m ()
deleteTBankCredentials pool (UserId uid) = withDatabase pool $ do
  let userIdText = Text.pack $ show uid
  mCred <- getBy $ UniqueTBankCredential userIdText
  case mCred of
    Just (Entity key _) -> do
      deleteWhere [TBankSandboxAccountUserId ==. userIdText]
      delete key
    Nothing -> return ()

-- | Get Bybit credentials (decrypts)
getBybitCredentials :: MonadIO m => ConnectionPool -> EncryptionContext -> UserId -> m (Maybe BybitCredentials)
getBybitCredentials pool encCtx (UserId uid) = liftIO $ withDatabase pool $ do
  mEntity <- getBy $ UniqueBybitCredential (Text.pack $ show uid)
  case mEntity of
    Nothing -> pure Nothing
    Just (Entity _ BybitCredential{..}) -> liftIO $ do
      mApiKey <- decryptCredential encCtx bybitCredentialApiKey
      mApiSecret <- decryptCredential encCtx bybitCredentialApiSecret
      case (mApiKey, mApiSecret) of
        (Just apiKey, Just apiSecret) ->
          pure $ Just $ BybitCredentials
            { bybitApiKeyId = ApiKeyId $ read $ Text.unpack bybitCredentialApiKeyId
            , bybitApiKey = apiKey
            , bybitApiSecret = apiSecret
            , bybitTestnet = bybitCredentialTestnet
            }
        _ -> do
          putStrLn "WARNING: Failed to decrypt Bybit credentials"
          pure Nothing

saveBybitCredentials :: MonadIO m => ConnectionPool -> EncryptionContext -> UserId -> BybitCredentials -> m ()
saveBybitCredentials pool encCtx (UserId uid) BybitCredentials{..} = liftIO $ withDatabase pool $ do
  let userIdText = Text.pack $ show uid
  encApiKey <- liftIO $ encryptCredential encCtx bybitApiKey
  encApiSecret <- liftIO $ encryptCredential encCtx bybitApiSecret
  mExisting <- getBy $ UniqueBybitCredential userIdText
  case mExisting of
    Just (Entity key _) ->
      update key
        [ BybitCredentialApiKeyId =. Text.pack (show $ apiKeyIdToText bybitApiKeyId)
        , BybitCredentialApiKey =. encApiKey
        , BybitCredentialApiSecret =. encApiSecret
        , BybitCredentialTestnet =. bybitTestnet
        ]
    Nothing -> do
      _ <- insert $ BybitCredential
        { bybitCredentialUserId = userIdText
        , bybitCredentialApiKeyId = Text.pack (show $ apiKeyIdToText bybitApiKeyId)
        , bybitCredentialApiKey = encApiKey
        , bybitCredentialApiSecret = encApiSecret
        , bybitCredentialTestnet = bybitTestnet
        }
      return ()
  where
    apiKeyIdToText (ApiKeyId t) = t

deleteBybitCredentials :: MonadIO m => ConnectionPool -> UserId -> m ()
deleteBybitCredentials pool (UserId uid) = withDatabase pool $ do
  mEntity <- getBy $ UniqueBybitCredential (Text.pack $ show uid)
  case mEntity of
    Just (Entity key _) -> delete key
    Nothing -> return ()

-- | Get all T-Bank sandbox accounts for a user
getTBankSandboxAccounts :: MonadIO m => ConnectionPool -> UserId -> m [TBankSandboxInfo]
getTBankSandboxAccounts pool (UserId uid) = withDatabase pool $ do
  entities <- selectList [TBankSandboxAccountUserId ==. Text.pack (show uid)] []
  return $ map (entityToSandboxInfo . entityVal) entities
  where
    entityToSandboxInfo TBankSandboxAccount{..} = TBankSandboxInfo
      { tsiAccountId = tBankSandboxAccountAccountId
      , tsiName = tBankSandboxAccountName
      , tsiBalance = realToFrac <$> tBankSandboxAccountBalance
      }

-- | Save a T-Bank sandbox account
saveTBankSandboxAccount :: MonadIO m => ConnectionPool -> UserId -> TBankSandboxInfo -> m ()
saveTBankSandboxAccount pool (UserId uid) TBankSandboxInfo{..} = withDatabase pool $ do
  let userIdText = Text.pack $ show uid
  mExisting <- getBy $ UniqueTBankSandboxAccount userIdText tsiAccountId
  case mExisting of
    Just (Entity key _) -> do
      update key
        [ TBankSandboxAccountName =. tsiName
        , TBankSandboxAccountBalance =. realToFrac <$> tsiBalance
        ]
    Nothing -> do
      _ <- insert $ TBankSandboxAccount
        { tBankSandboxAccountUserId = userIdText
        , tBankSandboxAccountAccountId = tsiAccountId
        , tBankSandboxAccountName = tsiName
        , tBankSandboxAccountBalance = realToFrac <$> tsiBalance
        , tBankSandboxAccountIsDefault = False
        }
      return ()

-- | Delete a T-Bank sandbox account
deleteTBankSandboxAccount :: MonadIO m => ConnectionPool -> UserId -> Text -> m ()
deleteTBankSandboxAccount pool (UserId uid) accountId = withDatabase pool $ do
  mExisting <- getBy $ UniqueTBankSandboxAccount (Text.pack $ show uid) accountId
  case mExisting of
    Just (Entity key _) -> delete key
    Nothing -> return ()

-- | Set the default T-Bank sandbox account
setDefaultTBankSandboxAccount :: MonadIO m => ConnectionPool -> UserId -> Text -> m ()
setDefaultTBankSandboxAccount pool (UserId uid) accountId = withDatabase pool $ do
  let userIdText = Text.pack $ show uid
  -- First, clear all defaults for this user
  updateWhere [TBankSandboxAccountUserId ==. userIdText]
    [TBankSandboxAccountIsDefault =. False]
  -- Then set the new default
  mAccount <- getBy $ UniqueTBankSandboxAccount userIdText accountId
  case mAccount of
    Just (Entity key _) -> update key [TBankSandboxAccountIsDefault =. True]
    Nothing -> return ()

-- ============================================================================
-- Position Operations
-- ============================================================================

-- | Get all positions for a user
getPositions :: MonadIO m => ConnectionPool -> UserId -> Maybe PositionStatus -> m [PositionEntity]
getPositions pool (UserId uid) mStatus = withDatabase pool $ do
  let filters = [PositionEntityUserId ==. Text.pack (show uid)]
      statusFilter = case mStatus of
        Just status -> [PositionEntityStatus ==. positionStatusToText status]
        Nothing -> []
  entities <- selectList (filters ++ statusFilter) []
  return $ map entityVal entities
  where
    positionStatusToText PositionOpening = "opening"
    positionStatusToText PositionActive = "active"
    positionStatusToText (PositionPartial _) = "partial"
    positionStatusToText PositionClosing = "closing"
    positionStatusToText PositionClosed = "closed"
    positionStatusToText PositionCancelled = "cancelled"

-- | Get open positions for a user (convenience function)
getOpenPositions :: MonadIO m => ConnectionPool -> UserId -> m [PositionEntity]
getOpenPositions pool uid = withDatabase pool $ do
  let filters = [PositionEntityUserId ==. Text.pack (show uid)]
      statusFilter = [PositionEntityStatus <-. ["opening", "active", "partial"]]
  entities <- selectList (filters ++ statusFilter) []
  return $ map entityVal entities

-- | Get a position by ID
getPositionById :: MonadIO m => ConnectionPool -> PositionId -> m (Maybe PositionEntity)
getPositionById pool pid = withDatabase pool $ do
  let pidText = Text.pack $ show pid
  mEntity <- getBy $ UniquePositionEntity pidText
  return $ entityVal <$> mEntity

-- | Save a new position
savePosition :: MonadIO m => ConnectionPool -> UserId -> Position -> m (Key PositionEntity)
savePosition pool (UserId uid) Position{..} = withDatabase pool $ do
  let pidText = Text.pack $ show positionId
      uidText = Text.pack $ show uid
  now <- liftIO getCurrentTime
  
  -- Insert position
  key <- insert $ PositionEntity
    { positionEntityPositionId = pidText
    , positionEntityUserId = uidText
    , positionEntityStrategyId = Text.pack $ show positionStrategyId
    , positionEntityStatus = positionStatusToText positionStatus
    , positionEntityRealizedPL = realToFrac <$> positionRealizedPL
    , positionEntityUnrealizedPL = realToFrac <$> positionUnrealizedPL
    , positionEntityMarginUsed = realToFrac positionMarginUsed
    , positionEntityMaxProfit = realToFrac <$> positionMaxProfit
    , positionEntityMaxLoss = realToFrac <$> positionMaxLoss
    , positionEntityEntryPremium = realToFrac <$> positionEntryPremium
    , positionEntityEntryPop = realToFrac <$> positionEntryPop
    , positionEntityOpenedAt = positionOpenedAt
    , positionEntityClosedAt = positionClosedAt
    , positionEntityNotes = positionNotes
    }
  
  -- Insert position legs
  mapM_ (insertLeg pidText) positionLegs
  
  -- Track active instruments (for duplicate detection)
  when (isActiveStatus positionStatus) $ do
    let instrumentIds = getInstrumentIdsFromLegs positionLegs
    mapM_ (trackActiveInstrument uidText now pidText) instrumentIds
  
  return key
  where
    positionStatusToText PositionOpening = "opening"
    positionStatusToText PositionActive = "active"
    positionStatusToText (PositionPartial _) = "partial"
    positionStatusToText PositionClosing = "closing"
    positionStatusToText PositionClosed = "closed"
    positionStatusToText PositionCancelled = "cancelled"
    
    isActiveStatus s = s `elem` [PositionOpening, PositionActive, PositionPartial 0]
    
    insertLeg = insertLegRow
    
    getInstrumentIdsFromLegs = map (\(PositionLeg{posLegInstrumentId = InstrumentId i}) -> i)

insertLegRow :: MonadIO m => Text -> PositionLeg -> SqlPersistT m ()
insertLegRow pid PositionLeg{..} = insert_ $ PositionLegEntity
  { positionLegEntityPositionId = pid
  , positionLegEntityOrderId = Text.pack $ show posLegOrderId
  , positionLegEntityInstrumentId = let InstrumentId i = posLegInstrumentId in i
  , positionLegEntitySide = case posLegSide of Buy -> "buy"; Sell -> "sell"
  , positionLegEntityQuantity = realToFrac posLegQuantity
  , positionLegEntityFilledPrice = realToFrac posLegFilledPrice
  , positionLegEntityFilledAt = posLegFilledAt
  }

trackActiveInstrument :: MonadIO m => Text -> UTCTime -> Text -> Text -> SqlPersistT m ()
trackActiveInstrument userId time posId instId = do
  mExisting <- getBy $ UniqueActiveInstrument userId instId
  case mExisting of
    Just _ -> return ()
    Nothing -> insert_ $ ActiveInstrument
      { activeInstrumentUserId = userId
      , activeInstrumentInstrumentId = instId
      , activeInstrumentPositionId = posId
      , activeInstrumentOpenedAt = time
      }

trackPositionInstruments :: MonadIO m => ConnectionPool -> UserId -> PositionId -> [InstrumentId] -> m ()
trackPositionInstruments pool (UserId uid) pid insts = withDatabase pool $ do
  now <- liftIO getCurrentTime
  let uidText = Text.pack $ show uid
      pidText = Text.pack $ show pid
  mapM_ (\(InstrumentId i) -> trackActiveInstrument uidText now pidText i) insts

-- | Attach filled legs after Opening and occupy instruments for dupe checks.
insertPositionLegs :: MonadIO m => ConnectionPool -> UserId -> Position -> m ()
insertPositionLegs pool (UserId uid) Position{..} = withDatabase pool $ do
  let pidText = Text.pack $ show positionId
      uidText = Text.pack $ show uid
  now <- liftIO getCurrentTime
  mapM_ (insertLegRow pidText) positionLegs
  let instrumentIds = map (\(PositionLeg{posLegInstrumentId = InstrumentId i}) -> i) positionLegs
  mapM_ (trackActiveInstrument uidText now pidText) instrumentIds

-- | Update an existing position
updatePosition :: MonadIO m => ConnectionPool -> Position -> m ()
updatePosition pool Position{..} = withDatabase pool $ do
  mEntity <- getBy $ UniquePositionEntity (Text.pack $ show positionId)
  case mEntity of
    Just (Entity key _) -> do
      update key
        [ PositionEntityStatus =. positionStatusToText positionStatus
        , PositionEntityRealizedPL =. realToFrac <$> positionRealizedPL
        , PositionEntityUnrealizedPL =. realToFrac <$> positionUnrealizedPL
        , PositionEntityMarginUsed =. realToFrac positionMarginUsed
        , PositionEntityMaxProfit =. realToFrac <$> positionMaxProfit
        , PositionEntityMaxLoss =. realToFrac <$> positionMaxLoss
        , PositionEntityEntryPremium =. realToFrac <$> positionEntryPremium
        , PositionEntityEntryPop =. realToFrac <$> positionEntryPop
        , PositionEntityOpenedAt =. positionOpenedAt
        , PositionEntityClosedAt =. positionClosedAt
        , PositionEntityNotes =. positionNotes
        ]
      
      -- Clean up active instruments if position is no longer active
      when (positionStatus `elem` [PositionClosed, PositionCancelled]) $ do
        let pidText = Text.pack $ show positionId
        deleteWhere [ActiveInstrumentPositionId ==. pidText]
        
    Nothing -> return ()
  where
    positionStatusToText PositionOpening = "opening"
    positionStatusToText PositionActive = "active"
    positionStatusToText (PositionPartial _) = "partial"
    positionStatusToText PositionClosing = "closing"
    positionStatusToText PositionClosed = "closed"
    positionStatusToText PositionCancelled = "cancelled"

-- | Delete a position
deletePosition :: MonadIO m => ConnectionPool -> PositionId -> m ()
deletePosition pool pid = withDatabase pool $ do
  let pidText = Text.pack $ show pid
  mEntity <- getBy $ UniquePositionEntity pidText
  case mEntity of
    Just (Entity key _) -> do
      -- Clean up related records
      deleteWhere [PositionLegEntityPositionId ==. pidText]
      deleteWhere [ActiveInstrumentPositionId ==. pidText]
      delete key
    Nothing -> return ()

-- | Check if user has an active position for a specific instrument
-- This prevents duplicate positions for the same option/strike
hasPositionForInstrument :: MonadIO m => ConnectionPool -> UserId -> Text -> m Bool
hasPositionForInstrument pool (UserId uid) instrumentId = withDatabase pool $ do
  mActive <- getBy $ UniqueActiveInstrument (Text.pack $ show uid) instrumentId
  return $ case mActive of
    Just _ -> True
    Nothing -> False

-- | Get all active instruments for a user (for duplicate checking)
getActiveInstruments :: MonadIO m => ConnectionPool -> UserId -> m [Text]
getActiveInstruments pool (UserId uid) = withDatabase pool $ do
  entities <- selectList [ActiveInstrumentUserId ==. Text.pack (show uid)] []
  return $ map (activeInstrumentInstrumentId . entityVal) entities

-- | Get legs for a position by position ID text
getPositionLegs :: MonadIO m => ConnectionPool -> Text -> m [PositionLegEntity]
getPositionLegs pool pidText = withDatabase pool $ do
  entities <- selectList [PositionLegEntityPositionId ==. pidText] []
  return $ map entityVal entities
