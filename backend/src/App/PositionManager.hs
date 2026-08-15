{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.PositionManager
  ( startPositionManager
  , ManagerConfig (..)
  , defaultManagerConfig
  ) where

import API.Strategies
  ( StrategyLegInfo (..)
  , StrategyResponse (..)
  , defaultFilters
  , fetchRealTimeStrategies
  )
import App.PositionOrchestrator
  ( OpenPositionSpec (..)
  , closePositionOnExchange
  , mkOpenLeg
  , openMultiLegPosition
  , sizeLegs
  )
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forever, when, forM_, void)
import Data.List (maximumBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Scientific (Scientific)
import qualified Data.Text as Text
import Database.Persist.Sql (ConnectionPool)
import Domain.Broker (BrokerConfig (..))
import Domain.Kelly (kellyEdgeGone, kellyFraction)
import Domain.Position (Position (..), PositionLeg (..), calculateUnrealizedPL, isPositionActive)
import Domain.Settings
  ( RiskParameters (..)
  , Settings (..)
  , settingsToBrokerConfig
  )
import Domain.Strategy (StrategyMetrics (..))
import Domain.Types (InstrumentId (..), UserId (..))
import Effects.Auth (defaultUserId)
import Effects.Position (entityToPosition)
import qualified Effects.Settings as SettingsEffect
import Infrastructure.Encryption (EncryptionContext)
import qualified Infrastructure.Broker.Bybit.Client as Bybit
import qualified Infrastructure.Persistence as DB
import Polysemy (runM)

data ManagerConfig = ManagerConfig
  { mcPollIntervalMicros :: Int
  , mcUserId :: UserId
  } deriving stock (Eq, Show)

defaultManagerConfig :: ManagerConfig
defaultManagerConfig = ManagerConfig
  { mcPollIntervalMicros = 30 * 1000 * 1000  -- 30s
  , mcUserId = defaultUserId
  }

startPositionManager :: ConnectionPool -> EncryptionContext -> ManagerConfig -> IO ()
startPositionManager pool encCtx cfg = void $ forkIO $ forever $ do
  eresult <- try $ runManagerTick pool encCtx cfg
  case eresult of
    Left (e :: SomeException) -> putStrLn $ "PositionManager error: " ++ show e
    Right () -> pure ()
  threadDelay (mcPollIntervalMicros cfg)

runManagerTick :: ConnectionPool -> EncryptionContext -> ManagerConfig -> IO ()
runManagerTick pool encCtx ManagerConfig{..} = do
  settings <- runM $ SettingsEffect.runSettingsWithPool pool encCtx $
    SettingsEffect.getSettings mcUserId
  let risk = settingsRiskParams settings
      mConfig = settingsToBrokerConfig settings

  entities <- DB.getOpenPositions pool mcUserId
  positions <- fmap (mapMaybe id) $ mapM (\e -> do
    legs <- DB.getPositionLegs pool (DB.positionEntityPositionId e)
    pure $ entityToPosition e legs) entities

  let allInsts = [posLegInstrumentId l | p <- positions, l <- positionLegs p]
  marks <- fetchMarks mConfig allInsts

  forM_ positions $ \pos -> do
    let mUpl = calculateUnrealizedPL pos marks
        updated = pos { positionUnrealizedPL = mUpl }
    DB.updatePosition pool updated

    case (mUpl, positionMaxProfit pos, positionMaxLoss pos, positionEntryPop pos, mConfig) of
      (Just upl, Just maxProfit, _, _, Just config)
        | maxProfit > 0
        , upl >= maxProfit * (riskTakeProfitPercent risk / 100) -> do
            putStrLn $ "PositionManager: take-profit hit for " ++ show (positionId pos)
            void $ closePositionOnExchange pool config mcUserId updated
      (Just upl, Just maxProfit, Just maxLoss, Just pop, Just config)
        | kellyEdgeGone pop maxProfit maxLoss upl -> do
            putStrLn $ "PositionManager: Kelly f*<=0 for " ++ show (positionId pos)
            void $ closePositionOnExchange pool config mcUserId updated
      _ -> pure ()

    when (riskRebalanceEnabled risk) $
      evaluateRebalance risk updated

  case (riskAutoModeEnabled risk, mConfig) of
    (True, Just config)
      | length (filter (isPositionActive . positionStatus) positions) < riskMaxOpenPositions risk ->
          tryAutoOpen pool config mcUserId risk positions
    _ -> pure ()

tryAutoOpen
  :: ConnectionPool
  -> BrokerConfig
  -> UserId
  -> RiskParameters
  -> [Position]
  -> IO ()
tryAutoOpen pool config uid risk openPos = do
  result <- fetchRealTimeStrategies defaultFilters
  case result of
    Left err -> putStrLn $ "PositionManager: scan failed: " ++ err
    Right strategies ->
      case pickBest strategies of
        Nothing -> pure ()
        Just spec -> do
          eres <- openMultiLegPosition pool config uid spec
          case eres of
            Left err -> putStrLn $ "PositionManager: auto-open failed: " ++ Text.unpack err
            Right pos -> putStrLn $ "PositionManager: auto-opened " ++ show (positionId pos)
  where
    occupied = [posLegInstrumentId l | p <- openPos, l <- positionLegs p]
    pickBest ss =
      let scored = mapMaybe (scoreOccupied occupied risk) ss
      in if null scored then Nothing else Just (snd $ maximumBy (comparing fst) scored)

scoreOccupied :: [InstrumentId] -> RiskParameters -> StrategyResponse -> Maybe (Scientific, OpenPositionSpec)
scoreOccupied occupied risk s = do
  let m = strategyMetrics s
  p <- metricsProbabilityOfProfit m
  maxP <- metricsMaxProfit m
  maxL <- metricsMaxLoss m
  let insts = map sliInstrumentId (strategyOpenLegs s)
      f = kellyFraction p (if maxL > 0 then maxP / maxL else 0)
      legs = map (\StrategyLegInfo{..} -> mkOpenLeg sliInstrumentId sliSide sliQuantity sliLimitPrice) (strategyOpenLegs s)
  spec <- sizeLegs risk p maxP maxL OpenPositionSpec
    { opsStrategyId = strategyId s
    , opsUnderlying = strategyUnderlying s
    , opsLegs = legs
    , opsMaxProfit = Just maxP
    , opsMaxLoss = Just maxL
    , opsEntryPremium = Just (strategyNetPremium s)
    , opsEntryPop = Just p
    , opsMargin = strategyMarginRequired s
    }
  if f > 0 && not (null legs) && not (any (`elem` occupied) insts)
    then Just (f, spec)
    else Nothing

fetchMarks :: Maybe BrokerConfig -> [InstrumentId] -> IO [(InstrumentId, Scientific)]
fetchMarks (Just config@BybitConfig{}) insts = do
  let bases = ["BTC", "SOL", "XAUT", "XRP", "MNT", "DOGE"]
  tickerLists <- mapM (\b -> Bybit.fetchOptionTickers config b) bases
  let allTickers = concat [ts | Right ts <- tickerLists]
      wanted = map unInstrumentId insts
      marks = mapMaybe (\t -> do
        guard (Bybit.btSymbol t `elem` wanted)
        priceTxt <- Bybit.btMarkPrice t `orElse` Bybit.btBid1Price t
        p <- readSci priceTxt
        pure (InstrumentId (Bybit.btSymbol t), p)) allTickers
  pure marks
  where
    guard True = Just ()
    guard False = Nothing
    orElse (Just x) _ = Just x
    orElse Nothing y = y
    readSci t = case reads (Text.unpack t) of
      [(n, _)] -> Just n
      _ -> Nothing
fetchMarks _ _ = pure []

evaluateRebalance :: RiskParameters -> Position -> IO ()
evaluateRebalance risk pos =
  case (positionUnrealizedPL pos, positionMaxProfit pos) of
    (Just upl, Just maxP)
      | maxP > 0
      , upl < 0
      , abs upl > maxP * riskMinRebalanceImprovement risk ->
          putStrLn $ "PositionManager: rebalance candidate " ++ show (positionId pos)
            ++ " upl=" ++ show upl
    _ -> pure ()
