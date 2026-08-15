{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.PositionOrchestrator
  ( OpenLegSpec (..)
  , OpenPositionSpec (..)
  , openMultiLegPosition
  , closePositionOnExchange
  , sizeLegs
  , mkOpenLeg
  ) where

import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import Domain.Broker (BrokerConfig (..))
import Domain.Kelly (kellyQuantity)
import Domain.Order (OrderRequest (..), OrderResponse (..), OrderType (..))
import Domain.Position (Position (..), PositionLeg (..))
import Domain.Settings (RiskParameters (..))
import Domain.Types
  ( InstrumentId (..)
  , OrderStatus (..)
  , PositionId (..)
  , PositionStatus (..)
  , Side (..)
  , StrategyId (..)
  , UserId (..)
  )
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
  , opsEntryPop :: Maybe Scientific
  , opsMargin :: Scientific
  } deriving stock (Eq, Show)

mkOpenLeg :: InstrumentId -> Side -> Scientific -> Maybe Scientific -> OpenLegSpec
mkOpenLeg = OpenLegSpec

-- | Scale every leg (and P/L / margin) by Kelly qty. Nothing if qty < 1.
sizeLegs :: RiskParameters -> Scientific -> Scientific -> Scientific -> OpenPositionSpec -> Maybe OpenPositionSpec
sizeLegs RiskParameters{..} p maxP maxL spec@OpenPositionSpec{..} =
  let q = kellyQuantity p maxP maxL riskKellyFraction riskMaxPositionSize riskMaxLossPercent
  in if q < 1 then Nothing
     else let n = fromInteger q
          in Just spec
               { opsLegs = [l { olsQuantity = olsQuantity l * n } | l <- opsLegs]
               , opsMaxProfit = Just (maxP * n)
               , opsMaxLoss = Just (maxL * n)
               , opsEntryPremium = (* n) <$> opsEntryPremium
               , opsMargin = opsMargin * n
               }

-- | Place all legs via broker and persist the position.
openMultiLegPosition
  :: ConnectionPool
  -> BrokerConfig
  -> UserId
  -> OpenPositionSpec
  -> IO (Either Text Position)
openMultiLegPosition pool config uid OpenPositionSpec{..} = do
  let instIds = map olsInstrumentId opsLegs
  dupes <- mapM (\(InstrumentId i) -> DB.hasPositionForInstrument pool uid i) instIds
  if or dupes
    then pure $ Left "One or more instruments already have an open position"
    else do
      now <- getCurrentTime
      pid <- nextRandom
      let positionId = PositionId pid
          opening = Position
            { positionId = positionId
            , positionStrategyId = opsStrategyId
            , positionStatus = PositionOpening
            , positionLegs = []
            , positionGreeks = Nothing
            , positionRealizedPL = Nothing
            , positionUnrealizedPL = Just 0
            , positionMarginUsed = opsMargin
            , positionMaxProfit = opsMaxProfit
            , positionMaxLoss = opsMaxLoss
            , positionEntryPremium = opsEntryPremium
            , positionEntryPop = opsEntryPop
            , positionOpenedAt = Just now
            , positionClosedAt = Nothing
            , positionNotes = Just $ "Opened on " <> opsUnderlying
            }
      _ <- DB.savePosition pool uid opening
      DB.trackPositionInstruments pool uid positionId instIds
      -- ponytail: leftover fills on hard fail; add unwind if orphans show up
      results <- mapM (placeLeg config positionId) opsLegs
      case sequence results of
        Left err -> do
          DB.updatePosition pool opening { positionStatus = PositionCancelled }
          pure $ Left err
        Right filledLegs -> do
          let active = opening
                { positionStatus = PositionActive
                , positionLegs = filledLegs
                }
          DB.insertPositionLegs pool uid active
          DB.updatePosition pool active
          pure $ Right active

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
  let closing = pos { positionStatus = PositionClosing }
  DB.updatePosition pool closing
  results <- mapM (closeLeg config (positionId pos)) (positionLegs pos)
  case sequence results of
    Left err -> pure $ Left err
    Right _ -> do
      now <- getCurrentTime
      let closed = closing
            { positionStatus = PositionClosed
            , positionClosedAt = Just now
            , positionRealizedPL = positionUnrealizedPL pos
            }
      DB.updatePosition pool closed
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
