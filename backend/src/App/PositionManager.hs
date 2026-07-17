{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App.PositionManager
  ( startPositionManager
  , ManagerConfig (..)
  , defaultManagerConfig
  ) where

import App.PositionOrchestrator (closePositionOnExchange)
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forever, when, forM_, void)
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.Persist.Sql (ConnectionPool)
import Domain.Broker (BrokerConfig (..))
import Domain.Position (Position (..), PositionLeg (..), calculateUnrealizedPL)
import Domain.Settings
  ( RiskParameters (..)
  , Settings (..)
  , settingsToBrokerConfig
  )
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

    case (mUpl, positionMaxProfit pos, mConfig) of
      (Just upl, Just maxProfit, Just config)
        | maxProfit > 0
        , upl >= maxProfit * (riskTakeProfitPercent risk / 100) -> do
            putStrLn $ "PositionManager: take-profit hit for " ++ show (positionId pos)
            void $ closePositionOnExchange pool config mcUserId updated
      _ -> pure ()

    when (riskRebalanceEnabled risk) $
      evaluateRebalance risk updated

  when (riskAutoModeEnabled risk) $
    putStrLn "PositionManager: auto-open enabled"

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
