{-# LANGUAGE TemplateHaskell #-}

module Effects.Position
  ( PositionEffect (..)
  , createPosition
  , updatePosition
  , closePosition
  , getPosition
  , listPositions
  , runPositionIO
  ) where

import Data.Text (Text)
import Domain.Order (OrderRequest)
import Domain.Position (Position (..), PositionUpdate)
import Domain.Strategy (Strategy)
import Domain.Types (PositionId (..), PositionStatus (..), UserId (..))
import Polysemy
import Polysemy.Embed

-- ============================================================================
-- Position Effect Definition
-- ============================================================================

data PositionEffect m a where
  CreatePosition :: UserId -> Strategy -> PositionEffect m Position
  UpdatePosition :: PositionId -> PositionUpdate -> PositionEffect m (Maybe Position)
  ClosePosition :: PositionId -> PositionEffect m (Maybe Position)
  GetPosition :: PositionId -> PositionEffect m (Maybe Position)
  ListPositions :: UserId -> PositionStatus -> PositionEffect m [Position]

makeSem ''PositionEffect

-- ============================================================================
-- IO Interpreter (Production)
-- ============================================================================

runPositionIO :: Members '[Embed IO] r => Sem (PositionEffect ': r) a -> Sem r a
runPositionIO = interpret $ \case
  CreatePosition _uid _strategy -> embed $ do
    putStrLn "Creating position"
    -- TODO: Implement position creation with database
    pure $ Position
      { positionId = PositionId $ read "00000000-0000-0000-0000-000000000000"
      , positionStrategyId = undefined
      , positionStatus = PositionOpening
      , positionLegs = []
      , positionGreeks = Nothing
      , positionRealizedPL = Nothing
      , positionUnrealizedPL = Nothing
      , positionMarginUsed = 0
      , positionOpenedAt = Nothing
      , positionClosedAt = Nothing
      , positionNotes = Nothing
      }

  UpdatePosition _pid _update -> embed $ do
    putStrLn "Updating position"
    -- TODO: Implement position update
    pure Nothing

  ClosePosition _pid -> embed $ do
    putStrLn "Closing position"
    -- TODO: Implement position close
    pure Nothing

  GetPosition _pid -> embed $ do
    putStrLn "Getting position"
    -- TODO: Implement position fetch
    pure Nothing

  ListPositions _uid _status -> embed $ do
    putStrLn "Listing positions"
    -- TODO: Implement position list
    pure []
