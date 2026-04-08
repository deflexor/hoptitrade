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

import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Database.Persist.Sql (ConnectionPool)
import Domain.Position (Position (..), PositionUpdate)
import Domain.Strategy (Strategy)
import Domain.Types (InstrumentId (..), PositionId (..), PositionStatus (..), UserId (..))
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

runPositionWithPool :: Members '[Embed IO] r => ConnectionPool -> Sem (PositionEffect ': r) a -> Sem r a
runPositionWithPool pool = interpret $ \case
  CreatePosition uid _strategy -> embed @IO $ do
    now <- getCurrentTime
    pid <- nextRandom
    let position = Position
          { positionId = PositionId pid
          , positionStrategyId = undefined
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

  UpdatePosition _pid _update -> embed @IO $ pure Nothing
  ClosePosition _pid -> embed @IO $ pure Nothing
  GetPosition pid -> embed @IO $ do
    mEntity <- DB.getPositionById pool pid
    pure Nothing
  ListPositions uid mStatus -> embed @IO $ do
    _entities <- case mStatus of
      Just status -> DB.getPositions pool uid (Just status)
      Nothing -> DB.getPositions pool uid Nothing
    pure []
  CheckDuplicatePosition uid instId -> embed @IO $ do
    DB.hasPositionForInstrument pool uid (unInstrumentId instId)
  RecoverOpenPositions uid -> embed @IO $ do
    putStrLn $ "Recovering open positions for user: " ++ show uid
    _entities <- DB.getOpenPositions pool uid
    putStrLn $ "Found positions in database"
    pure []

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
