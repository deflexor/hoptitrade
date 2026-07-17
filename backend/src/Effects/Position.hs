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
  , entityToPosition
  ) where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist.Sql (ConnectionPool)
import Domain.Position (Position (..), PositionLeg (..), PositionUpdate (..))
import Domain.Strategy (Strategy (..), StrategyMetrics (..))
import Domain.Types (InstrumentId (..), OrderId (..), PositionId (..), PositionStatus (..), Side (..), StrategyId (..), UserId (..))
import qualified Infrastructure.Persistence as DB
import Polysemy

data PositionEffect m a where
  CreatePosition :: UserId -> Strategy -> PositionEffect m Position
  UpdatePosition :: PositionId -> PositionUpdate -> PositionEffect m (Maybe Position)
  ClosePosition :: PositionId -> PositionEffect m (Maybe Position)
  GetPosition :: PositionId -> PositionEffect m (Maybe Position)
  ListPositions :: UserId -> Maybe PositionStatus -> PositionEffect m [Position]
  CheckDuplicatePosition :: UserId -> InstrumentId -> PositionEffect m Bool
  RecoverOpenPositions :: UserId -> PositionEffect m [Position]

makeSem ''PositionEffect

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
    , positionMaxProfit = realToFrac <$> DB.positionEntityMaxProfit entity
    , positionMaxLoss = realToFrac <$> DB.positionEntityMaxLoss entity
    , positionEntryPremium = realToFrac <$> DB.positionEntityEntryPremium entity
    , positionOpenedAt = DB.positionEntityOpenedAt entity
    , positionClosedAt = DB.positionEntityClosedAt entity
    , positionNotes = DB.positionEntityNotes entity
    }

legEntityToLeg :: DB.PositionLegEntity -> PositionLeg
legEntityToLeg leg =
  PositionLeg
    { posLegOrderId = OrderId (DB.positionLegEntityOrderId leg)
    , posLegInstrumentId = InstrumentId (DB.positionLegEntityInstrumentId leg)
    , posLegSide = textToSide (DB.positionLegEntitySide leg)
    , posLegQuantity = realToFrac (DB.positionLegEntityQuantity leg)
    , posLegFilledPrice = realToFrac (DB.positionLegEntityFilledPrice leg)
    , posLegFilledAt = DB.positionLegEntityFilledAt leg
    }

textToStatus :: Text -> Maybe PositionStatus
textToStatus "opening"   = Just PositionOpening
textToStatus "active"    = Just PositionActive
textToStatus "partial"   = Just (PositionPartial 0)
textToStatus "closing"   = Just PositionClosing
textToStatus "closed"    = Just PositionClosed
textToStatus "cancelled" = Just PositionCancelled
textToStatus _           = Nothing

textToSide :: Text -> Side
textToSide "buy"  = Buy
textToSide "sell" = Sell
textToSide _      = Buy

readUUID :: Text -> Maybe UUID
readUUID = UUID.fromString . Text.unpack

applyPositionUpdate :: Position -> PositionUpdate -> Position
applyPositionUpdate pos update = pos
  { positionStatus = posUpdateStatus update
  }

runPositionWithPool :: Members '[Embed IO] r => ConnectionPool -> Sem (PositionEffect ': r) a -> Sem r a
runPositionWithPool pool = interpret $ \case
  CreatePosition uid strategy -> embed @IO $ do
    now <- getCurrentTime
    pid <- nextRandom
    let metrics = strategyMetrics strategy
        position = Position
          { positionId = PositionId pid
          , positionStrategyId = strategyId strategy
          , positionStatus = PositionOpening
          , positionLegs = []
          , positionGreeks = Nothing
          , positionRealizedPL = Nothing
          , positionUnrealizedPL = Nothing
          , positionMarginUsed = strategyMarginRequired strategy
          , positionMaxProfit = metricsMaxProfit metrics
          , positionMaxLoss = metricsMaxLoss metrics
          , positionEntryPremium = Just (strategyNetPremium strategy)
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
        let pidText = DB.positionEntityPositionId entity
        legs <- DB.getPositionLegs pool pidText
        case entityToPosition entity legs of
          Nothing -> pure Nothing
          Just pos -> do
            let updatedPos = applyPositionUpdate pos update
            DB.updatePosition pool updatedPos
            pure $ Just updatedPos

  ClosePosition pid -> embed @IO $ do
    mEntity <- DB.getPositionById pool pid
    case mEntity of
      Nothing -> pure Nothing
      Just entity -> do
        let pidText = DB.positionEntityPositionId entity
        legs <- DB.getPositionLegs pool pidText
        case entityToPosition entity legs of
          Nothing -> pure Nothing
          Just pos -> do
            now <- getCurrentTime
            let closedPos = pos
                  { positionStatus = PositionClosing
                  , positionClosedAt = Just now
                  }
            DB.updatePosition pool closedPos
            pure $ Just closedPos

  GetPosition pid -> embed @IO $ do
    mEntity <- DB.getPositionById pool pid
    case mEntity of
      Nothing -> pure Nothing
      Just entity -> do
        let pidText = DB.positionEntityPositionId entity
        legs <- DB.getPositionLegs pool pidText
        pure $ entityToPosition entity legs

  ListPositions uid mStatus -> embed @IO $ do
    entities <- DB.getPositions pool uid mStatus
    mapMaybe id <$> mapM convertEntity entities
    where
      convertEntity entity = do
        let pidText = DB.positionEntityPositionId entity
        legs <- DB.getPositionLegs pool pidText
        pure $ entityToPosition entity legs

  CheckDuplicatePosition uid (InstrumentId instId) -> embed @IO $
    DB.hasPositionForInstrument pool uid instId

  RecoverOpenPositions uid -> embed @IO $ do
    putStrLn $ "Recovering open positions for user: " ++ show uid
    entities <- DB.getOpenPositions pool uid
    putStrLn $ "Found " ++ show (length entities) ++ " open positions in database"
    mapMaybe id <$> mapM convertEntity entities
    where
      convertEntity entity = do
        let pidText = DB.positionEntityPositionId entity
        legs <- DB.getPositionLegs pool pidText
        pure $ entityToPosition entity legs

runPositionIO :: Members '[Embed IO] r => Sem (PositionEffect ': r) a -> Sem r a
runPositionIO = interpret $ \case
  CreatePosition _uid strategy -> embed @IO $ do
    now <- getCurrentTime
    pid <- nextRandom
    let metrics = strategyMetrics strategy
    pure $ Position
      { positionId = PositionId pid
      , positionStrategyId = strategyId strategy
      , positionStatus = PositionOpening
      , positionLegs = []
      , positionGreeks = Nothing
      , positionRealizedPL = Nothing
      , positionUnrealizedPL = Nothing
      , positionMarginUsed = 0
      , positionMaxProfit = metricsMaxProfit metrics
      , positionMaxLoss = metricsMaxLoss metrics
      , positionEntryPremium = Just (strategyNetPremium strategy)
      , positionOpenedAt = Just now
      , positionClosedAt = Nothing
      , positionNotes = Nothing
      }
  UpdatePosition _pid _update -> embed @IO $ pure Nothing
  ClosePosition _pid -> embed @IO $ pure Nothing
  GetPosition _pid -> embed @IO $ pure Nothing
  ListPositions _uid _status -> embed @IO $ pure []
  CheckDuplicatePosition _uid _instId -> embed @IO $ pure False
  RecoverOpenPositions _uid -> embed @IO $ pure []

listOpenPositions :: Member PositionEffect r => UserId -> Sem r [Position]
listOpenPositions uid = listPositions uid Nothing
