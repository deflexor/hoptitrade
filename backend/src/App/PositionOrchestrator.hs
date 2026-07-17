{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.PositionOrchestrator
  ( OpenLegSpec (..)
  , OpenPositionSpec (..)
  , openMultiLegPosition
  , closePositionOnExchange
  ) where

import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Domain.Broker (BrokerConfig (..))
import Domain.Order (OrderRequest (..), OrderResponse (..), OrderType (..))
import Domain.Position (Position (..), PositionLeg (..))
import Domain.Types
  ( InstrumentId (..)
  , OrderId (..)
  , OrderStatus (..)
  , PositionId (..)
  , PositionStatus (..)
  , Side (..)
  , StrategyId (..)
  , UserId (..)
  )
import qualified Effects.Broker as Broker
import qualified Infrastructure.Broker.Bybit as Bybit
import qualified Infrastructure.Persistence as DB
import Database.Persist.Sql (ConnectionPool)

data OpenLegSpec = OpenLegSpec
  { olsInstrumentId :: InstrumentId
  , olsSide :: Side
  , olsQuantity :: Scientific
  , olsLimitPrice :: Maybe Scientific
  } deriving stock (Eq, Show)

data OpenPositionSpec = OpenPositionSpec
  { opsStrategyId :: StrategyId
  , opsUnderlying :: Text
  , opsLegs :: [OpenLegSpec]
  , opsMaxProfit :: Maybe Scientific
  , opsMaxLoss :: Maybe Scientific
  , opsEntryPremium :: Maybe Scientific
  , opsMargin :: Scientific
  } deriving stock (Eq, Show)

-- | Place all legs via broker and persist the position.
openMultiLegPosition
  :: ConnectionPool
  -> BrokerConfig
  -> UserId
  -> OpenPositionSpec
  -> IO (Either Text Position)
openMultiLegPosition pool config uid OpenPositionSpec{..} = do
  -- Duplicate check
  let instIds = map olsInstrumentId opsLegs
  dupes <- mapM (\(InstrumentId i) -> DB.hasPositionForInstrument pool uid i) instIds
  if or dupes
    then pure $ Left "One or more instruments already have an open position"
    else do
      now <- getCurrentTime
      pid <- nextRandom
      let positionId = PositionId pid

      -- Place orders sequentially
      results <- mapM (placeLeg config positionId) opsLegs
      case sequence results of
        Left err -> pure $ Left err
        Right filledLegs -> do
          let position = Position
                { positionId = positionId
                , positionStrategyId = opsStrategyId
                , positionStatus = PositionActive
                , positionLegs = filledLegs
                , positionGreeks = Nothing
                , positionRealizedPL = Nothing
                , positionUnrealizedPL = Just 0
                , positionMarginUsed = opsMargin
                , positionMaxProfit = opsMaxProfit
                , positionMaxLoss = opsMaxLoss
                , positionEntryPremium = opsEntryPremium
                , positionOpenedAt = Just now
                , positionClosedAt = Nothing
                , positionNotes = Just $ "Opened on " <> opsUnderlying
                }
          _ <- DB.savePosition pool uid position
          pure $ Right position

placeLeg :: BrokerConfig -> PositionId -> OpenLegSpec -> IO (Either Text PositionLeg)
placeLeg config pid OpenLegSpec{..} = do
  now <- getCurrentTime
  let req = OrderRequest
        { orderRequestPositionId = pid
        , orderRequestInstrumentId = olsInstrumentId
        , orderRequestSide = olsSide
        , orderRequestQuantity = olsQuantity
        , orderRequestPrice = olsLimitPrice
        , orderRequestOrderType = case olsLimitPrice of
            Just _ -> LimitOrder
            Nothing -> MarketOrder
        , orderRequestTPPrice = Nothing
        , orderRequestSLPrice = Nothing
        }
  result <- case config of
    BybitConfig{} -> Bybit.placeOrderBybit config req False
    _ -> do
      -- Fall through to generic broker path via Bybit-style failure for non-Bybit
      pure $ Left $ Bybit.BybitConfigError "Open orchestrator currently requires Bybit"
  case result of
    Left err -> pure $ Left $ Text.pack $ show err
    Right OrderResponse{..} ->
      case orderResponseStatus of
        OrderFailed msg -> pure $ Left msg
        _ -> pure $ Right PositionLeg
          { posLegOrderId = orderResponseOrderId
          , posLegInstrumentId = olsInstrumentId
          , posLegSide = olsSide
          , posLegQuantity = olsQuantity
          , posLegFilledPrice = fromMaybe (fromMaybe 0 olsLimitPrice) orderResponseAvgPrice
          , posLegFilledAt = now
          }

-- | Close all legs with reduceOnly orders, mark position closed.
closePositionOnExchange
  :: ConnectionPool
  -> BrokerConfig
  -> UserId
  -> Position
  -> IO (Either Text Position)
closePositionOnExchange pool config _uid pos = do
  results <- mapM (closeLeg config (positionId pos)) (positionLegs pos)
  case sequence results of
    Left err -> pure $ Left err
    Right _ -> do
      now <- getCurrentTime
      let closed = pos
            { positionStatus = PositionClosed
            , positionClosedAt = Just now
            , positionRealizedPL = positionUnrealizedPL pos
            }
      DB.updatePosition pool closed
      -- Clear active instruments
      let uidText = Text.pack $ show _uid  -- unused properly; cleanup via status
      pure $ Right closed

closeLeg :: BrokerConfig -> PositionId -> PositionLeg -> IO (Either Text ())
closeLeg config pid PositionLeg{..} = do
  let opposite = case posLegSide of Buy -> Sell; Sell -> Buy
      req = OrderRequest
        { orderRequestPositionId = pid
        , orderRequestInstrumentId = posLegInstrumentId
        , orderRequestSide = opposite
        , orderRequestQuantity = posLegQuantity
        , orderRequestPrice = Nothing
        , orderRequestOrderType = MarketOrder
        , orderRequestTPPrice = Nothing
        , orderRequestSLPrice = Nothing
        }
  result <- case config of
    BybitConfig{} -> Bybit.placeOrderBybit config req True
    _ -> pure $ Left $ Bybit.BybitConfigError "Close requires Bybit"
  case result of
    Left err -> pure $ Left $ Text.pack $ show err
    Right OrderResponse{orderResponseStatus = OrderFailed msg} -> pure $ Left msg
    Right _ -> pure $ Right ()
