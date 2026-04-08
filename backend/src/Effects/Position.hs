{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Position
  ( PositionEffect (..)
  , createPosition
  , updatePosition
  , closePosition
  , getPosition
  , listPositions
  , listOpenPositions
  , checkDuplicatePosition
  , recoverOpenPositions
  , runPositionWithPool
  , runPositionIO
  ) where

import Data.Maybe (listToMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist.Sql (ConnectionPool)
import Domain.Position (Position (..), PositionLeg (..), PositionUpdate)
import Domain.Strategy (Strategy)
import Domain.Types (InstrumentId (..), OrderId (..), PositionId (..), PositionStatus (..), Side (..), StrategyId (..), UserId (..))
import qualified Infrastructure.Persistence as DB
import Polysemy
import Polysemy.Embed

data PositionEffect m a where
  CreatePosition :: UserId -> Strategy -> PositionEffect m Position
  UpdatePosition :: PositionId -> PositionUpdate -> PositionEffect m (Maybe Position)
  ClosePosition :: PositionId -> PositionEffect m (Maybe Position)
  GetPosition :: PositionId -> PositionEffect m (Maybe Position)
  ListPositions :: UserId -> Maybe PositionStatus -> PositionEffect m [Position]
  CheckDuplicatePosition :: UserId -> InstrumentId -> PositionEffect m Bool
  RecoverOpenPositions :: UserId -> PositionEffect m [Position]

makeSem ''PositionEffect

-- ============================================================================
-- Entity-to-Domain Conversion
-- ============================================================================

-- | Convert a DB PositionEntity + its legs to a domain Position
entityToPosition :: DB.PositionEntity -> [DB.PositionLegEntity] -> Maybe Position
entityToPosition entity legs = do
  pid <- readUUID (DB.positionEntityPositionId entity)
  sid <- readUUID (DB.positionEntityStrategyId entity)
  status <- textToStatus (DB.positionEntityStatus entity)
  pure Position
    { positionId = PositionId pid
    , positionStrategyId = StrategyId sid
    , positionStatus = status
    , positionLegs = map legEntityToLeg legs
    , positionGreeks = Nothing
    , positionRealizedPL = realToFrac <$> DB.positionEntityRealizedPL entity
    , positionUnrealizedPL = realToFrac <$> DB.positionEntityUnrealizedPL entity
    , positionMarginUsed = realToFrac (DB.positionEntityMarginUsed entity)
    , positionOpenedAt = DB.positionEntityOpenedAt entity
    , positionClosedAt = DB.positionEntityClosedAt entity
    , positionNotes = DB.positionEntityNotes entity
    }

-- | Convert a DB leg entity to domain PositionLeg
legEntityToLeg :: DB.PositionLegEntity -> PositionLeg
legEntityToLeg leg =
  PositionLeg
    { posLegOrderId = OrderId (DB.positionLegEntityOrderId leg)
    , posLegSide = textToSide (DB.positionLegEntitySide leg)
    , posLegQuantity = realToFrac (DB.positionLegEntityQuantity leg)
    , posLegFilledPrice = realToFrac (DB.positionLegEntityFilledPrice leg)
    , posLegFilledAt = DB.positionLegEntityFilledAt leg
    }

-- | Parse status text back to PositionStatus
textToStatus :: Text -> Maybe PositionStatus
textToStatus "opening"   = Just PositionOpening
textToStatus "active"    = Just PositionActive
textToStatus "partial"   = Just (PositionPartial 0)  -- exact qty lost in text roundtrip
textToStatus "closing"   = Just PositionClosing
textToStatus "closed"    = Just PositionClosed
textToStatus "cancelled" = Just PositionCancelled
textToStatus _           = Nothing

-- | Parse side text back to Side
textToSide :: Text -> Side
textToSide "buy"  = Buy
textToSide "sell" = Sell
textToSide _      = Buy

-- | Safely parse a Text UUID
readUUID :: Text -> Maybe UUID
readUUID = UUID.fromString . Text.unpack

-- ============================================================================
-- Interpreter with ConnectionPool
-- ============================================================================

runPositionWithPool :: Members '[Embed IO] r => ConnectionPool -> Sem (PositionEffect ': r) a -> Sem r a
runPositionWithPool pool = interpret $ \case
  CreatePosition uid _strategy -> embed @IO $ do
    now <- getCurrentTime
    pid <- nextRandom
    let position = Position
          { positionId = PositionId pid
          , positionStrategyId = undefined  -- TODO: wire strategy ID
          , positionStatus = PositionOpening
          , positionLegs = []
          , positionGreeks = Nothing
          , positionRealizedPL = Nothing
          , positionUnrealizedPL = Nothing
          , positionMarginUsed = 0
          , positionOpenedAt = Just now
          , positionClosedAt = Nothing
          , positionNotes = Nothing
          }
    _ <- DB.savePosition pool uid position
    pure position

  UpdatePosition pid update -> embed @IO $ do
    mEntity <- DB.getPositionById pool pid
    case mEntity of
      Nothing -> pure Nothing
      Just entity -> do
        -- TODO: Apply update to entity and save
        let pidText = DB.positionEntityPositionId entity
        legs <- DB.getPositionLegs pool pidText
        pure $ entityToPosition entity legs

  ClosePosition pid -> embed @IO $ do
    mEntity <- DB.getPositionById pool pid
    case mEntity of
      Nothing -> pure Nothing
      Just entity -> do
        let pidText = DB.positionEntityPositionId entity
        legs <- DB.getPositionLegs pool pidText
        pure $ entityToPosition entity legs

  GetPosition pid -> embed @IO $ do
    mEntity <- DB.getPositionById pool pid
    case mEntity of
      Nothing -> pure Nothing
      Just entity -> do
        let pidText = DB.positionEntityPositionId entity
        legs <- DB.getPositionLegs pool pidText
        pure $ entityToPosition entity legs

  ListPositions uid mStatus -> embed @IO $ do
    entities <- case mStatus of
      Just status -> DB.getPositions pool uid (Just status)
      Nothing -> DB.getPositions pool uid Nothing
    -- Fetch legs for each position and convert
    mapM convertEntity entities
    where
      convertEntity entity = do
        let pidText = DB.positionEntityPositionId entity
        legs <- DB.getPositionLegs pool pidText
        pure $ case entityToPosition entity legs of
          Just pos -> pos
          Nothing -> error $ "Failed to convert position entity: " ++ show pidText

  CheckDuplicatePosition uid instId -> embed @IO $ do
    DB.hasPositionForInstrument pool uid (unInstrumentId instId)

  RecoverOpenPositions uid -> embed @IO $ do
    putStrLn $ "Recovering open positions for user: " ++ show uid
    entities <- DB.getOpenPositions pool uid
    putStrLn $ "Found " ++ show (length entities) ++ " open positions in database"
    mapM convertEntity entities
    where
      convertEntity entity = do
        let pidText = DB.positionEntityPositionId entity
        legs <- DB.getPositionLegs pool pidText
        pure $ case entityToPosition entity legs of
          Just pos -> pos
          Nothing -> error $ "Failed to convert position entity: " ++ show pidText

-- ============================================================================
-- Stub Interpreter (for development/testing without DB)
-- ============================================================================

runPositionIO :: Members '[Embed IO] r => Sem (PositionEffect ': r) a -> Sem r a
runPositionIO = interpret $ \case
  CreatePosition _uid _strategy -> embed @IO $ do
    now <- getCurrentTime
    pid <- nextRandom
    pure $ Position (PositionId pid) undefined PositionOpening [] Nothing Nothing Nothing 0 (Just now) Nothing Nothing
  UpdatePosition _pid _update -> embed @IO $ pure Nothing
  ClosePosition _pid -> embed @IO $ pure Nothing
  GetPosition _pid -> embed @IO $ pure Nothing
  ListPositions _uid _status -> embed @IO $ pure []
  CheckDuplicatePosition _uid _instId -> embed @IO $ pure False
  RecoverOpenPositions _uid -> embed @IO $ pure []

listOpenPositions :: Member PositionEffect r => UserId -> Sem r [Position]
listOpenPositions uid = listPositions uid Nothing
