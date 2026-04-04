{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module API.Strategies
  ( StrategiesAPI
  , strategiesServer
  , StrategyResponse (..)
  , StrategyFilters (..)
  , RiskLevel (..)
  , defaultFilters
  ) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Options (..), genericToJSON, genericParseJSON, defaultOptions)
import Data.List (sortBy, groupBy, sortOn, minimumBy, maximumBy, partition)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe, maybeToList)
import Data.Ord (comparing, Down (..))
import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime, diffUTCTime, addUTCTime, nominalDay)
import qualified Data.Time.Clock.POSIX as POSIX
import Domain.Greeks (Greeks (..))
import Domain.Greeks.Calculator (calculateGreeks, blackScholes)
import Domain.Option (OptionContract (..), OptionType (..), Strike (..), Expiration (..))
import Domain.Strategy
  ( AIAdvice (..)
  , SpreadType (..)
  , StrategyMetrics (..)
  , StrategyStatus (..)
  , StrategyType (..)
  )
import Domain.Types (StrategyId (..), TradingMode (..), InstrumentId (..))
import GHC.Generics (Generic)
import Infrastructure.OKX.Client
  ( OKXClientConfig
  , OKXInstrument (..)
  , OKXTicker (..)
  , defaultOKXConfig
  , fetchOptionInstruments
  , fetchOptionChain
  , fetchUnderlyingPrice
  )
import Polysemy
import Polysemy.Embed
import Prelude hiding (last)
import Servant

-- ============================================================================
-- Risk Level and Filtering
-- ============================================================================

data RiskLevel
  = Conservative    -- High PoP, low max loss
  | Moderate        -- Balanced
  | Aggressive      -- High profit potential, higher risk
  | All             -- No filtering
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data StrategyFilters = StrategyFilters
  { filterRiskLevel :: RiskLevel
  , filterMinProbability :: Maybe Scientific  -- Minimum PoP (e.g., 0.60)
  , filterMaxRiskReward :: Maybe Scientific   -- Maximum risk/reward ratio (e.g., 3.0)
  , filterMinExpectedReturn :: Maybe Scientific
  , filterMaxDeltaExposure :: Maybe Scientific  -- Limit directional exposure
  , filterMinLiquidity :: Maybe Scientific      -- Minimum 24h volume
  , filterMaxSpreadPercent :: Maybe Scientific  -- Maximum bid-ask spread %
  , filterDaysToExpiry :: Maybe (Int, Int)      -- Min/Max days (e.g., 7-45)
  } deriving stock (Eq, Show, Generic)

defaultFilters :: StrategyFilters
defaultFilters = StrategyFilters
  { filterRiskLevel = Moderate
  , filterMinProbability = Just 0.50
  , filterMaxRiskReward = Just 5.0
  , filterMinExpectedReturn = Just 10.0
  , filterMaxDeltaExposure = Just 0.30
  , filterMinLiquidity = Just 1000
  , filterMaxSpreadPercent = Just 0.10
  , filterDaysToExpiry = Just (7, 45)
  }

-- ============================================================================
-- API Type
-- ============================================================================

type StrategiesAPI =
  "strategies" :> QueryParam "mode" TradingMode :> Get '[JSON] [StrategyResponse]
  :<|> "strategies" :> Capture "strategyId" StrategyId :> Get '[JSON] (Maybe StrategyResponse)

-- ============================================================================
-- Response Types
-- ============================================================================

data StrategyResponse = StrategyResponse
  { strategyId :: StrategyId
  , strategyType :: StrategyType
  , strategyName :: Text
  , strategyDescription :: Text
  , strategyUnderlying :: Text
  , strategyGreeks :: Greeks
  , strategyMetrics :: StrategyMetrics
  , strategyNetPremium :: Scientific
  , strategyMarginRequired :: Scientific
  , strategyAdvice :: AIAdvice
  , strategyStatus :: StrategyStatus
  , strategyQualityScore :: Scientific  -- Added: overall quality 0-100
  , strategyRiskRank :: Int             -- Added: rank within risk category
  } deriving stock (Eq, Show, Generic)

instance ToJSON StrategyResponse where
  toJSON = genericToJSON defaultOptions

instance FromJSON StrategyResponse where
  parseJSON = genericParseJSON defaultOptions

-- ============================================================================
-- Server
-- ============================================================================

strategiesServer :: Members '[Embed IO] r => ServerT StrategiesAPI (Sem r)
strategiesServer = listStrategies :<|> getStrategy
  where
    listStrategies _mMode = do
      result <- embed @IO $ fetchRealTimeStrategies defaultFilters
      case result of
        Left err -> do
          embed $ putStrLn $ "Error fetching strategies: " ++ show err
          now <- embed @IO getCurrentTime
          pure $ generateMockStrategies now
        Right strategies -> pure strategies

    getStrategy sid = do
      strategies <- listStrategies Nothing
      pure $ case filter (\s -> strategyId s == sid) strategies of
        (s:_) -> Just s
        [] -> Nothing

-- ============================================================================
-- Real-Time Strategy Generation with Filtering
-- ============================================================================

fetchRealTimeStrategies :: StrategyFilters -> IO (Either String [StrategyResponse])
fetchRealTimeStrategies filters = do
  let config = defaultOKXConfig
      underlying = "BTC-USD"
  
  priceResult <- fetchUnderlyingPrice config underlying
  case priceResult of
    Left err -> return $ Left $ show err
    Right spotPrice -> do
      instResult <- fetchOptionInstruments config underlying
      case instResult of
        Left err -> return $ Left $ show err
        Right instruments -> do
          if length instruments < 4
            then return $ Left "Not enough options available"
            else do
              -- Fetch market data
              tickerResult <- fetchOptionChain config (take 50 instruments)
              case tickerResult of
                Left err -> return $ Left $ show err
                Right tickers -> do
                  now <- getCurrentTime
                  
                  -- Build contracts with calculated Greeks
                  let contracts = buildContracts now spotPrice instruments tickers
                  
                  -- Apply liquidity filters
                  let liquidContracts = filter (isLiquid filters) contracts
                  
                  if length liquidContracts < 4
                    then return $ Left "Not enough liquid options"
                    else do
                      -- Generate and filter strategies
                      let strategies = generateAndFilterStrategies now spotPrice filters liquidContracts
                          ranked = rankStrategies strategies
                      return $ Right ranked

-- ============================================================================
-- Liquidity and Quality Filters
-- ============================================================================

isLiquid :: StrategyFilters -> OptionContract -> Bool
isLiquid filters contract =
  let minVol = fromMaybe 0 $ filterMinLiquidity filters
      maxSpread = fromMaybe 1.0 $ filterMaxSpreadPercent filters
      
      hasVolume = case contractVolume contract of
        Just v -> v >= minVol
        Nothing -> False
      
      spreadOk = case (contractBid contract, contractAsk contract) of
        (Just bid, Just ask) -> 
          if ask > 0
            then (ask - bid) / ask <= maxSpread
            else False
        _ -> False
      
      hasGreeks = all (/= Nothing) 
        [contractDelta contract, contractGamma contract, contractTheta contract, contractVega contract]
      
  in hasVolume && spreadOk && hasGreeks

isWithinExpiryRange :: StrategyFilters -> UTCTime -> OptionContract -> Bool
isWithinExpiryRange filters now contract =
  case filterDaysToExpiry filters of
    Nothing -> True
    Just (minDays, maxDays) ->
      let Expiration expiry = contractExpiration contract
          daysToExpiry = realToFrac (diffUTCTime expiry now) / (24 * 3600)
      in daysToExpiry >= fromIntegral minDays && daysToExpiry <= fromIntegral maxDays

-- ============================================================================
-- Strategy Generation with Risk Analysis
-- ============================================================================

generateAndFilterStrategies :: UTCTime -> Scientific -> StrategyFilters -> [OptionContract] -> [StrategyResponse]
generateAndFilterStrategies now spotPrice filters contracts =
  let byExpiration = groupBy (\a b -> contractExpiration a == contractExpiration b) $ 
                     sortOn contractExpiration contracts
      
      allStrategies = concatMap (generateForExpiration now spotPrice filters) byExpiration
      
      -- Apply risk filters
      filtered = filter (passesRiskFilters filters) allStrategies
      
      -- Sort by quality score
      sorted = sortOn (Down . strategyQualityScore) filtered
      
  in take 20 sorted  -- Return top 20

passesRiskFilters :: StrategyFilters -> StrategyResponse -> Bool
passesRiskFilters filters strategy =
  let metrics = strategyMetrics strategy
      
      -- Probability filter
      probOk = case (metricsProbabilityOfProfit metrics, filterMinProbability filters) of
        (Just prob, Just minProb) -> prob >= minProb
        _ -> True
      
      -- Risk/Reward filter
      rrOk = case (calculateRiskReward metrics, filterMaxRiskReward filters) of
        (Just rr, Just maxRR) -> rr <= maxRR
        _ -> True
      
      -- Expected return filter
      retOk = case (metricsExpectedReturn metrics, filterMinExpectedReturn filters) of
        (Just ret, Just minRet) -> ret >= minRet
        _ -> True
      
      -- Delta exposure filter
      deltaOk = case (filterMaxDeltaExposure filters) of
        Just maxDelta -> abs (greeksDelta $ strategyGreeks strategy) <= maxDelta
        _ -> True
      
      -- Risk level filter
      levelOk = case filterRiskLevel filters of
        Conservative -> isConservative strategy
        Aggressive -> isAggressive strategy
        Moderate -> isModerate strategy
        All -> True
        
  in probOk && rrOk && retOk && deltaOk && levelOk

isConservative :: StrategyResponse -> Bool
isConservative s =
  let prob = fromMaybe 0 $ metricsProbabilityOfProfit $ strategyMetrics s
      maxLoss = fromMaybe 999999 $ metricsMaxLoss $ strategyMetrics s
  in prob >= 0.60 && maxLoss < 500

isModerate :: StrategyResponse -> Bool
isModerate s =
  let prob = fromMaybe 0 $ metricsProbabilityOfProfit $ strategyMetrics s
      delta = abs $ greeksDelta $ strategyGreeks s
  in prob >= 0.45 && prob < 0.70 && delta < 0.50

isAggressive :: StrategyResponse -> Bool
isAggressive s =
  let prob = fromMaybe 0 $ metricsProbabilityOfProfit $ strategyMetrics s
      maxProfit = metricsMaxProfit $ strategyMetrics s
  in prob < 0.50 || maxProfit == Nothing

calculateRiskReward :: StrategyMetrics -> Maybe Scientific
calculateRiskReward metrics =
  case (metricsMaxProfit metrics, metricsMaxLoss metrics) of
    (Just profit, Just loss) -> 
      if loss > 0 then Just $ profit / loss else Nothing
    _ -> Nothing

-- ============================================================================
-- Strategy Scoring and Ranking
-- ============================================================================

rankStrategies :: [StrategyResponse] -> [StrategyResponse]
rankStrategies strategies =
  let scored = map (calculateQualityScore >>> assignRank) strategies
      sorted = sortOn (Down . strategyQualityScore) scored
  in zipWith (\rank s -> s { strategyRiskRank = rank }) [1..] sorted
  where
    (>>>) = flip (.)
    
    calculateQualityScore s =
      let metrics = strategyMetrics s
          greeks = strategyGreeks s
          
          -- Score components (0-100 each)
          probScore = case metricsProbabilityOfProfit metrics of
            Just p -> min 100 $ fromFloatDigits $ toRealFloat p * 100
            _ -> 0
          
          rrScore = case calculateRiskReward metrics of
            Just rr -> min 100 $ fromFloatDigits $ toRealFloat rr * 20  -- Scale: 5:1 = 100
            _ -> 0
          
          thetaScore = min 100 $ fromFloatDigits $ max 0 $ toRealFloat (greeksTheta greeks) * 10
          
          deltaScore = max 0 $ 100 - fromFloatDigits (abs (toRealFloat (greeksDelta greeks)) * 200)
          
          -- Weighted average
          total = probScore * 0.35 + rrScore * 0.25 + thetaScore * 0.25 + deltaScore * 0.15
          
      in s { strategyQualityScore = total }
    
    assignRank s = s { strategyRiskRank = 0 }  -- Will be set later

-- ============================================================================
-- Contract Building (with Greeks calculation)
-- ============================================================================

buildContracts :: UTCTime -> Scientific -> [OKXInstrument] -> [OKXTicker] -> [OptionContract]
buildContracts now spotPrice instruments tickers =
  let tickerMap = [(tickerInstId t, t) | t <- tickers]
  in catMaybes $ map (buildContract now spotPrice tickerMap) instruments

buildContract :: UTCTime -> Scientific -> [(Text, OKXTicker)] -> OKXInstrument -> Maybe OptionContract
buildContract now spotPrice tickerMap inst = do
  ticker <- lookup (instId inst) tickerMap
  
  strike <- parseScientific (stk inst)
  expTime <- parseTimestampMs (expTime inst)
  
  let optionType = if optType inst == "C" then Call else Put
      timeToExpiry = max 0.001 $ realToFrac (diffUTCTime expTime now) / (365 * 24 * 3600)
      riskFreeRate = 0.05
      
      -- Estimate IV from Greeks if available, else use 50%
      volatility = estimateIV spotPrice strike timeToExpiry riskFreeRate ticker optionType
      
      (delta, gamma, theta, vega, rho) = calculateGreeks 
        spotPrice strike (fromFloatDigits timeToExpiry) (fromFloatDigits riskFreeRate) volatility optionType
  
  return $ OptionContract
    { contractInstrumentId = InstrumentId $ instId inst
    , contractUnderlying = instFamily inst
    , contractOptionType = optionType
    , contractStrike = Strike strike
    , contractExpiration = Expiration expTime
    , contractBid = parseScientific =<< bidPx ticker
    , contractAsk = parseScientific =<< askPx ticker
    , contractLastPrice = parseScientific =<< Infrastructure.OKX.Client.last ticker
    , contractVolume = parseScientific =<< vol24h ticker
    , contractOpenInterest = parseScientific =<< oi ticker
    , contractImpliedVol = Just volatility
    , contractDelta = Just delta
    , contractGamma = Just gamma
    , contractTheta = Just theta
    , contractVega = Just vega
    , contractTimestamp = now
    }

estimateIV :: Scientific -> Scientific -> Double -> Double -> OKXTicker -> OptionType -> Scientific
estimateIV spot strike timeToExpiry riskFreeRate ticker optionType =
  -- Try to back out IV from vega if available
  case (vega ticker, Infrastructure.OKX.Client.last ticker) of
    (Just vegaStr, Just priceStr) ->
      case (parseScientific vegaStr, parseScientific priceStr) of
        (Just vegaVal, Just priceVal) ->
          -- Simplified IV estimation from vega
          if vegaVal > 0
            then fromFloatDigits $ max 0.10 $ min 2.0 $ toRealFloat vegaVal / 100
            else 0.50
        _ -> 0.50
    _ -> 0.50  -- Default 50% IV for crypto

-- ============================================================================
-- Strategy Generation per Expiration
-- ============================================================================

generateForExpiration :: UTCTime -> Scientific -> StrategyFilters -> [OptionContract] -> [StrategyResponse]
generateForExpiration now spotPrice filters contracts =
  let validContracts = filter (isWithinExpiryRange filters now) contracts
      calls = filter (\c -> contractOptionType c == Call) validContracts
      puts = filter (\c -> contractOptionType c == Put) validContracts
      
      atmCall = findATM spotPrice calls
      atmPut = findATM spotPrice puts
      
      ironCondors = generateIronCondors now spotPrice filters calls puts
      spreads = generateSpreads now spotPrice filters calls puts
      straddles = generateStraddles now spotPrice filters atmCall atmPut
      
  in ironCondors ++ spreads ++ straddles

findATM :: Scientific -> [OptionContract] -> Maybe OptionContract
findATM spotPrice contracts = 
  case contracts of
    [] -> Nothing
    _ -> Just $ minimumBy (comparing (\c -> abs (toRealFloat (unStrike $ contractStrike c) - toRealFloat spotPrice))) contracts

-- ============================================================================
-- Strategy Generators with Filters
-- ============================================================================

generateIronCondors :: UTCTime -> Scientific -> StrategyFilters -> [OptionContract] -> [OptionContract] -> [StrategyResponse]
generateIronCondors now spotPrice filters calls puts = 
  catMaybes $ do
    let otmCalls = filter (\c -> toRealFloat (unStrike $ contractStrike c) > toRealFloat spotPrice * 1.02) calls
        otmPuts = filter (\p -> toRealFloat (unStrike $ contractStrike p) < toRealFloat spotPrice * 0.98) puts
        
        callSpreads = [(short, long) | short <- take 3 otmCalls, long <- drop 1 $ take 4 otmCalls, 
                                        unStrike (contractStrike long) > unStrike (contractStrike short)]
        putSpreads = [(short, long) | short <- take 3 otmPuts, long <- drop 1 $ take 4 otmPuts,
                                       unStrike (contractStrike long) < unStrike (contractStrike short)]
    
    [createIronCondor now spotPrice (sc, lc) (sp, lp) | (sc, lc) <- callSpreads, (sp, lp) <- putSpreads]
  where
    createIronCondor now spot (shortCall, longCall) (shortPut, longPut) = do
      let callCredit = fromMaybe 0 (contractBid shortCall) - fromMaybe 0 (contractAsk longCall)
          putCredit = fromMaybe 0 (contractBid shortPut) - fromMaybe 0 (contractAsk longPut)
          netPremium = callCredit + putCredit
          maxLoss = max (toRealFloat (unStrike $ contractStrike longCall) - toRealFloat (unStrike $ contractStrike shortCall))
                        (toRealFloat (unStrike $ contractStrike shortPut) - toRealFloat (unStrike $ contractStrike longPut))
          breakEvenLow = toRealFloat (unStrike $ contractStrike shortPut) - toRealFloat netPremium
          breakEvenHigh = toRealFloat (unStrike $ contractStrike shortCall) + toRealFloat netPremium
          
          -- Calculate probability using normal distribution
          width = toRealFloat (unStrike $ contractStrike longCall) - toRealFloat (unStrike $ contractStrike shortCall)
          pop = if width > 0 then 1.0 - (toRealFloat netPremium / width) else 0.50
          
      Just $ StrategyResponse
        { strategyId = StrategyId $ read "550e8400-e29b-41d4-a716-446655440001"
        , strategyType = IronCondor
        , strategyName = "Iron Condor " <> formatExpiration (contractExpiration shortCall)
        , strategyDescription = Text.pack $ "Sell " ++ show (toRealFloat $ unStrike $ contractStrike shortCall) ++ "C/Buy " ++ 
                                 show (toRealFloat $ unStrike $ contractStrike longCall) ++ "C, Sell " ++ 
                                 show (toRealFloat $ unStrike $ contractStrike shortPut) ++ "P/Buy " ++ 
                                 show (toRealFloat $ unStrike $ contractStrike longPut) ++ "P"
        , strategyUnderlying = contractUnderlying shortCall
        , strategyGreeks = combineGreeks [shortCall, longCall, shortPut, longPut]
        , strategyMetrics = StrategyMetrics
          { metricsMaxProfit = Just netPremium
          , metricsMaxLoss = Just $ fromFloatDigits maxLoss
          , metricsBreakEvenPoints = [fromFloatDigits breakEvenLow, fromFloatDigits breakEvenHigh]
          , metricsProbabilityOfProfit = Just $ fromFloatDigits $ max 0.10 $ min 0.95 pop
          , metricsExpectedReturn = Just $ fromFloatDigits $ toRealFloat netPremium * max 0.10 pop
          , metricsSuggestedTP = Just $ fromFloatDigits $ toRealFloat netPremium * 0.75
          , metricsSuggestedSL = Just $ fromFloatDigits $ maxLoss * 0.80
          }
        , strategyNetPremium = netPremium
        , strategyMarginRequired = fromFloatDigits maxLoss
        , strategyAdvice = AIAdvice True "High probability of profit with defined risk" 0.85
        , strategyStatus = StrategyActive
        , strategyQualityScore = 0  -- Will be calculated
        , strategyRiskRank = 0      -- Will be assigned
        }

generateSpreads :: UTCTime -> Scientific -> StrategyFilters -> [OptionContract] -> [OptionContract] -> [StrategyResponse]
generateSpreads now spotPrice filters calls puts = 
  let bullCallSpreads = generateBullCallSpreads now spotPrice filters calls
      bearPutSpreads = generateBearPutSpreads now spotPrice filters puts
  in bullCallSpreads ++ bearPutSpreads

generateBullCallSpreads :: UTCTime -> Scientific -> StrategyFilters -> [OptionContract] -> [StrategyResponse]
generateBullCallSpreads now spotPrice filters calls = 
  catMaybes $ do
    let itmCalls = filter (\c -> toRealFloat (unStrike $ contractStrike c) < toRealFloat spotPrice) calls
        otmCalls = filter (\c -> toRealFloat (unStrike $ contractStrike c) > toRealFloat spotPrice * 1.01) calls
    [createSpread long short | long <- take 3 itmCalls, short <- take 3 otmCalls]
  where
    createSpread long short = 
      let cost = fromMaybe 0 (contractAsk long) - fromMaybe 0 (contractBid short)
          width = toRealFloat (unStrike $ contractStrike short) - toRealFloat (unStrike $ contractStrike long)
          maxProfit = width - toRealFloat cost
          maxLoss = toRealFloat cost
          breakeven = toRealFloat (unStrike $ contractStrike long) + toRealFloat cost
          pop = if width > 0 then maxProfit / width else 0.50
      in
      Just $ StrategyResponse
        { strategyId = StrategyId $ read "550e8400-e29b-41d4-a716-446655440002"
        , strategyType = VerticalSpread DebitSpread
        , strategyName = "Bull Call Spread"
        , strategyDescription = Text.pack $ "Buy " ++ show (toRealFloat $ unStrike $ contractStrike long) ++ "C, Sell " ++ show (toRealFloat $ unStrike $ contractStrike short) ++ "C"
        , strategyUnderlying = contractUnderlying long
        , strategyGreeks = combineGreeks [long, short]
        , strategyMetrics = StrategyMetrics
          { metricsMaxProfit = Just $ fromFloatDigits maxProfit
          , metricsMaxLoss = Just cost
          , metricsBreakEvenPoints = [fromFloatDigits breakeven]
          , metricsProbabilityOfProfit = Just $ fromFloatDigits pop
          , metricsExpectedReturn = Just $ fromFloatDigits (maxProfit * pop)
          , metricsSuggestedTP = Just $ fromFloatDigits (maxProfit * 0.80)
          , metricsSuggestedSL = Just $ fromFloatDigits (maxLoss * 0.90)
          }
        , strategyNetPremium = -cost
        , strategyMarginRequired = cost
        , strategyAdvice = AIAdvice True "Moderate directional play with limited risk" 0.65
        , strategyStatus = StrategyActive
        , strategyQualityScore = 0
        , strategyRiskRank = 0
        }

generateBearPutSpreads :: UTCTime -> Scientific -> StrategyFilters -> [OptionContract] -> [StrategyResponse]
generateBearPutSpreads now spotPrice filters puts = 
  catMaybes $ do
    let itmPuts = filter (\p -> toRealFloat (unStrike $ contractStrike p) > toRealFloat spotPrice) puts
        otmPuts = filter (\p -> toRealFloat (unStrike $ contractStrike p) < toRealFloat spotPrice * 0.99) puts
    [createSpread long short | long <- take 3 itmPuts, short <- take 3 otmPuts]
  where
    createSpread long short = 
      let credit = fromMaybe 0 (contractBid short) - fromMaybe 0 (contractAsk long)
          width = toRealFloat (unStrike $ contractStrike long) - toRealFloat (unStrike $ contractStrike short)
          maxProfit = toRealFloat credit
          maxLoss = width - toRealFloat credit
          breakeven = toRealFloat (unStrike $ contractStrike long) - toRealFloat credit
          pop = if width > 0 then maxProfit / width else 0.50
      in
      Just $ StrategyResponse
        { strategyId = StrategyId $ read "550e8400-e29b-41d4-a716-446655440003"
        , strategyType = VerticalSpread CreditSpread
        , strategyName = "Bear Put Spread"
        , strategyDescription = "Buy ITM put, Sell OTM put for credit"
        , strategyUnderlying = contractUnderlying long
        , strategyGreeks = combineGreeks [long, short]
        , strategyMetrics = StrategyMetrics
          { metricsMaxProfit = Just $ fromFloatDigits maxProfit
          , metricsMaxLoss = Just $ fromFloatDigits maxLoss
          , metricsBreakEvenPoints = [fromFloatDigits breakeven]
          , metricsProbabilityOfProfit = Just $ fromFloatDigits pop
          , metricsExpectedReturn = Just $ fromFloatDigits (maxProfit * pop)
          , metricsSuggestedTP = Just $ fromFloatDigits (maxProfit * 0.85)
          , metricsSuggestedSL = Just $ fromFloatDigits (maxLoss * 0.85)
          }
        , strategyNetPremium = credit
        , strategyMarginRequired = fromFloatDigits maxLoss
        , strategyAdvice = AIAdvice True "Bearish strategy with income potential" 0.60
        , strategyStatus = StrategyActive
        , strategyQualityScore = 0
        , strategyRiskRank = 0
        }

generateStraddles :: UTCTime -> Scientific -> StrategyFilters -> Maybe OptionContract -> Maybe OptionContract -> [StrategyResponse]
generateStraddles now spotPrice filters (Just atmCall) (Just atmPut) = 
  let cost = fromMaybe 0 (contractAsk atmCall) + fromMaybe 0 (contractAsk atmPut)
      callStrike = toRealFloat $ unStrike $ contractStrike atmCall
      putStrike = toRealFloat $ unStrike $ contractStrike atmPut
      avgStrike = (callStrike + putStrike) / 2
      breakevenLow = avgStrike - toRealFloat cost
      breakevenHigh = avgStrike + toRealFloat cost
      -- Straddle needs large move; lower PoP
      pop = 0.30
  in [StrategyResponse
      { strategyId = StrategyId $ read "550e8400-e29b-41d4-a716-446655440004"
      , strategyType = Straddle
      , strategyName = "Long Straddle (Volatility Play)"
      , strategyDescription = "Buy ATM call and put for volatility expansion"
      , strategyUnderlying = contractUnderlying atmCall
      , strategyGreeks = combineGreeks [atmCall, atmPut]
      , strategyMetrics = StrategyMetrics
        { metricsMaxProfit = Nothing  -- Unlimited upside
        , metricsMaxLoss = Just cost
        , metricsBreakEvenPoints = [fromFloatDigits breakevenLow, fromFloatDigits breakevenHigh]
        , metricsProbabilityOfProfit = Just $ fromFloatDigits pop
        , metricsExpectedReturn = Nothing  -- High variance
        , metricsSuggestedTP = Just $ fromFloatDigits $ toRealFloat cost * 3
        , metricsSuggestedSL = Just $ fromFloatDigits $ toRealFloat cost * 0.70
        }
      , strategyNetPremium = -cost
      , strategyMarginRequired = cost
      , strategyAdvice = AIAdvice False "High risk play - requires large volatility expansion" 0.35
      , strategyStatus = StrategyActive
      , strategyQualityScore = 0
      , strategyRiskRank = 0
      }]
generateStraddles _ _ _ _ _ = []

-- ============================================================================
-- Helpers
-- ============================================================================

combineGreeks :: [OptionContract] -> Greeks
combineGreeks contracts = Greeks
  { greeksDelta = sum $ mapMaybe contractDelta contracts
  , greeksGamma = sum $ mapMaybe contractGamma contracts
  , greeksTheta = sum $ mapMaybe contractTheta contracts
  , greeksVega = sum $ mapMaybe contractVega contracts
  , greeksRho = 0
  }

formatExpiration :: Expiration -> Text
formatExpiration (Expiration t) = Text.pack $ show t

parseScientific :: Text -> Maybe Scientific
parseScientific txt = 
  case reads (Text.unpack txt) of
    [(n, "")] -> Just n
    _ -> Nothing

parseTimestampMs :: Text -> Maybe UTCTime
parseTimestampMs txt = 
  case parseScientific txt of
    Just ms -> Just $ POSIX.posixSecondsToUTCTime $ realToFrac ms / 1000
    Nothing -> Nothing

-- ============================================================================
-- Fallback Mock Data
-- ============================================================================

generateMockStrategies :: UTCTime -> [StrategyResponse]
generateMockStrategies now =
  [ mockIronCondor now 1
  , mockBullCallSpread now 2
  , mockBearPutSpread now 3
  , mockStraddle now 4
  ]

mockIronCondor :: UTCTime -> Int -> StrategyResponse
mockIronCondor now idx = StrategyResponse
  { strategyId = StrategyId $ read $ "550e8400-e29b-41d4-a716-44665544000" ++ show idx
  , strategyType = IronCondor
  , strategyName = "BTC Iron Condor (Fallback)"
  , strategyDescription = "Sell 65k call, buy 70k call, sell 55k put, buy 50k put"
  , strategyUnderlying = "BTC-USD"
  , strategyGreeks = Greeks
    { greeksDelta = 0.05
    , greeksGamma = -0.001
    , greeksTheta = 2.5
    , greeksVega = -0.5
    , greeksRho = 0.01
    }
  , strategyMetrics = StrategyMetrics
    { metricsMaxProfit = Just 450.0
    , metricsMaxLoss = Just 550.0
    , metricsBreakEvenPoints = [54500.0, 65500.0]
    , metricsProbabilityOfProfit = Just 0.65
    , metricsExpectedReturn = Just 45.0
    , metricsSuggestedTP = Just 400.0
    , metricsSuggestedSL = Just 500.0
    }
  , strategyNetPremium = 450.0
  , strategyMarginRequired = 5000.0
  , strategyAdvice = AIAdvice True "High probability of profit with defined risk" 0.85
  , strategyStatus = StrategyActive
  , strategyQualityScore = 75.0
  , strategyRiskRank = 1
  }

mockBullCallSpread :: UTCTime -> Int -> StrategyResponse
mockBullCallSpread now idx = StrategyResponse
  { strategyId = StrategyId $ read $ "550e8400-e29b-41d4-a716-44665544000" ++ show idx
  , strategyType = VerticalSpread DebitSpread
  , strategyName = "ETH Bull Call Spread (Fallback)"
  , strategyDescription = "Buy 3.2k call, sell 3.5k call"
  , strategyUnderlying = "ETH-USD"
  , strategyGreeks = Greeks
    { greeksDelta = 0.45
    , greeksGamma = 0.02
    , greeksTheta = -0.8
    , greeksVega = 1.2
    , greeksRho = 0.05
    }
  , strategyMetrics = StrategyMetrics
    { metricsMaxProfit = Just 280.0
    , metricsMaxLoss = Just 220.0
    , metricsBreakEvenPoints = [3220.0]
    , metricsProbabilityOfProfit = Just 0.58
    , metricsExpectedReturn = Just 28.0
    , metricsSuggestedTP = Just 250.0
    , metricsSuggestedSL = Just 200.0
    }
  , strategyNetPremium = -220.0
  , strategyMarginRequired = 500.0
  , strategyAdvice = AIAdvice True "Moderate risk with good profit potential" 0.65
  , strategyStatus = StrategyActive
  , strategyQualityScore = 65.0
  , strategyRiskRank = 2
  }

mockBearPutSpread :: UTCTime -> Int -> StrategyResponse
mockBearPutSpread now idx = StrategyResponse
  { strategyId = StrategyId $ read $ "550e8400-e29b-41d4-a716-44665544000" ++ show idx
  , strategyType = VerticalSpread CreditSpread
  , strategyName = "SOL Bear Put Spread (Fallback)"
  , strategyDescription = "Buy 140 put, sell 120 put"
  , strategyUnderlying = "SOL-USD"
  , strategyGreeks = Greeks
    { greeksDelta = -0.38
    , greeksGamma = 0.015
    , greeksTheta = -0.6
    , greeksVega = 0.9
    , greeksRho = -0.03
    }
  , strategyMetrics = StrategyMetrics
    { metricsMaxProfit = Just 1800.0
    , metricsMaxLoss = Just 200.0
    , metricsBreakEvenPoints = [1380.0]
    , metricsProbabilityOfProfit = Just 0.42
    , metricsExpectedReturn = Just 180.0
    , metricsSuggestedTP = Just 1600.0
    , metricsSuggestedSL = Just 180.0
    }
  , strategyNetPremium = -200.0
  , strategyMarginRequired = 2000.0
  , strategyAdvice = AIAdvice False "High risk/reward ratio requires careful monitoring" 0.45
  , strategyStatus = StrategyActive
  , strategyQualityScore = 55.0
  , strategyRiskRank = 3
  }

mockStraddle :: UTCTime -> Int -> StrategyResponse
mockStraddle now idx = StrategyResponse
  { strategyId = StrategyId $ read $ "550e8400-e29b-41d4-a716-44665544000" ++ show idx
  , strategyType = Straddle
  , strategyName = "BTC Long Straddle (Fallback)"
  , strategyDescription = "Buy ATM call and put for volatility expansion"
  , strategyUnderlying = "BTC-USD"
  , strategyGreeks = Greeks
    { greeksDelta = 0.02
    , greeksGamma = 0.08
    , greeksTheta = -5.2
    , greeksVega = 4.5
    , greeksRho = 0.0
    }
  , strategyMetrics = StrategyMetrics
    { metricsMaxProfit = Nothing
    , metricsMaxLoss = Just 1200.0
    , metricsBreakEvenPoints = [58800.0, 61200.0]
    , metricsProbabilityOfProfit = Just 0.35
    , metricsExpectedReturn = Nothing
    , metricsSuggestedTP = Just 5000.0
    , metricsSuggestedSL = Just 1000.0
    }
  , strategyNetPremium = -1200.0
  , strategyMarginRequired = 1200.0
  , strategyAdvice = AIAdvice False "High risk play dependent on volatility expansion" 0.35
  , strategyStatus = StrategyActive
  , strategyQualityScore = 40.0
  , strategyRiskRank = 4
  }
