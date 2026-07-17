{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Infrastructure.Broker.Bybit.MarketData
  ( fetchBybitOptionContracts
  , defaultBybitPublicConfig
  ) where

import Control.Monad (forM)
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Domain.Broker (BrokerConfig (..))
import Domain.Option (OptionContract (..), OptionType (..), Strike (..), Expiration (..))
import Domain.Settings (supportedBybitOptionCoins)
import Domain.Types (InstrumentId (..))
import Infrastructure.Broker.Bybit.Client
  ( BybitInstrument (..)
  , BybitTicker (..)
  , fetchIndexPrice
  , fetchOptionInstruments
  , fetchOptionTickers
  )

-- | Public (unsigned) Bybit config for market data
defaultBybitPublicConfig :: Bool -> BrokerConfig
defaultBybitPublicConfig testnet = BybitConfig
  { bybitApiKey = ""
  , bybitApiSecret = ""
  , bybitTestnet = testnet
  }

fetchBybitOptionContracts
  :: Bool  -- testnet
  -> [Text]  -- base coins (default: supportedBybitOptionCoins)
  -> IO (Either String (Scientific, [OptionContract]))
fetchBybitOptionContracts testnet mCoins = do
  let config = defaultBybitPublicConfig testnet
      coins = if null mCoins then supportedBybitOptionCoins else mCoins
  now <- getCurrentTime
  results <- forM coins (fetchOne config now)
  let successes = [x | Right x <- results]
      errors = [e | Left e <- results]
  if null successes
    then pure $ Left $ "Bybit option fetch failed: " ++ show errors
    else do
      let allContracts = concatMap snd successes
          -- Use BTC index as primary spot if present, else first
          spot = case lookup "BTC" [(c, s) | (c, (s, _)) <- zip coins successes] of
            Just s -> s
            Nothing -> fst (head successes)
      pure $ Right (spot, allContracts)

fetchOne
  :: BrokerConfig
  -> UTCTime
  -> Text
  -> IO (Either String (Scientific, [OptionContract]))
fetchOne config now baseCoin = do
  priceE <- fetchIndexPrice config baseCoin
  case priceE of
    Left err -> pure $ Left $ show err
    Right spot -> do
      instE <- fetchOptionInstruments config baseCoin
      case instE of
        Left err -> pure $ Left $ show err
        Right instruments -> do
          tickE <- fetchOptionTickers config baseCoin
          case tickE of
            Left err -> pure $ Left $ show err
            Right tickers ->
              pure $ Right (spot, buildContracts now baseCoin spot instruments tickers)

buildContracts
  :: UTCTime
  -> Text
  -> Scientific
  -> [BybitInstrument]
  -> [BybitTicker]
  -> [OptionContract]
buildContracts now baseCoin _spot instruments tickers =
  mapMaybe toContract instruments
  where
    tickerMap = [(btSymbol t, t) | t <- tickers]

    toContract BybitInstrument{..} = do
      ticker <- lookup biSymbol tickerMap
      optType <- parseOptType biOptionsType
      strike <- parseSci biStrike
      expiry <- parseDelivery biDeliveryTime
      pure OptionContract
        { contractInstrumentId = InstrumentId biSymbol
        , contractUnderlying = baseCoin
        , contractOptionType = optType
        , contractStrike = Strike strike
        , contractExpiration = Expiration expiry
        , contractBid = parseSciMaybe (btBid1Price ticker)
        , contractAsk = parseSciMaybe (btAsk1Price ticker)
        , contractLastPrice = parseSciMaybe (btLastPrice ticker)
        , contractVolume = parseSciMaybe (btVolume24h ticker)
        , contractOpenInterest = parseSciMaybe (btOpenInterest ticker)
        , contractImpliedVol = parseSciMaybe (btMarkIv ticker)
        , contractDelta = parseSciMaybe (btDelta ticker)
        , contractGamma = parseSciMaybe (btGamma ticker)
        , contractTheta = parseSciMaybe (btTheta ticker)
        , contractVega = parseSciMaybe (btVega ticker)
        , contractTimestamp = now
        }

    parseOptType t
      | Text.toLower t == "call" = Just Call
      | Text.toLower t == "put" = Just Put
      | otherwise = Nothing

    parseSci t = case reads (Text.unpack t) of
      [(n, _)] -> Just n
      _ -> Nothing

    parseSciMaybe Nothing = Nothing
    parseSciMaybe (Just t) = parseSci t

    parseDelivery t = case reads (Text.unpack t) of
      [(ms, _)] -> Just $ posixSecondsToUTCTime (fromInteger ms / 1000)
      _ -> Nothing
