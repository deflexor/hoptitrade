{-# LANGUAGE DerivingStrategies #-}

module Domain.Greeks.Calculator
  ( calculateGreeks
  , blackScholes
  , blackScholesDelta
  , blackScholesGamma
  , blackScholesTheta
  , blackScholesVega
  , blackScholesRho
  , annualizedVolatility
  ) where

import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Data.Time (UTCTime, diffUTCTime)
import Domain.Option (OptionType (..))
import qualified Statistics.Distribution as Stats
import qualified Statistics.Distribution.Normal as Normal

-- ============================================================================
-- Black-Scholes Model
-- ============================================================================

-- | Calculate all Greeks for an option
calculateGreeks :: Scientific    -- ^ Underlying price (S)
                -> Scientific    -- ^ Strike price (K)
                -> Scientific    -- ^ Time to expiration in years (T)
                -> Scientific    -- ^ Risk-free rate (r)
                -> Scientific    -- ^ Volatility (sigma)
                -> OptionType    -- ^ Call or Put
                -> (Scientific, Scientific, Scientific, Scientific, Scientific)  -- ^ (Delta, Gamma, Theta, Vega, Rho)
calculateGreeks s k t r sigma optionType =
  let s' = toRealFloat s
      k' = toRealFloat k
      t' = toRealFloat t
      r' = toRealFloat r
      sigma' = toRealFloat sigma
      
      d1 = (log (s' / k') + (r' + sigma' * sigma' / 2) * t') / (sigma' * sqrt t')
      d2 = d1 - sigma' * sqrt t'
      
      nd1 = Normal.cumulative (Normal.standard) d1
      nd2 = Normal.cumulative (Normal.standard) d2
      
      pdf_d1 = Stats.density (Normal.standard) d1
      
      delta = case optionType of
        Call -> fromFloatDigits nd1
        Put -> fromFloatDigits (nd1 - 1)
      
      gamma = fromFloatDigits $ pdf_d1 / (s' * sigma' * sqrt t')
      
      thetaTerm1 = -(s' * pdf_d1 * sigma') / (2 * sqrt t')
      thetaTerm2 = case optionType of
        Call -> -(r' * k' * exp (-r' * t') * nd2)
        Put -> r' * k' * exp (-r' * t') * (1 - nd2)
      theta = fromFloatDigits $ (thetaTerm1 + thetaTerm2) / 365  -- Daily theta
      
      vega = fromFloatDigits $ (s' * pdf_d1 * sqrt t') / 100  -- Per 1% vol change
      
      rho = case optionType of
        Call -> fromFloatDigits $ (k' * t' * exp (-r' * t') * nd2) / 100
        Put -> fromFloatDigits $ (-(k' * t' * exp (-r' * t') * (1 - nd2))) / 100
      
  in (delta, gamma, theta, vega, rho)

-- | Calculate option price using Black-Scholes
blackScholes :: Scientific    -- ^ Underlying price (S)
             -> Scientific    -- ^ Strike price (K)
             -> Scientific    -- ^ Time to expiration in years (T)
             -> Scientific    -- ^ Risk-free rate (r)
             -> Scientific    -- ^ Volatility (sigma)
             -> OptionType    -- ^ Call or Put
             -> Scientific    -- ^ Option price
blackScholes s k t r sigma optionType =
  let s' = toRealFloat s
      k' = toRealFloat k
      t' = toRealFloat t
      r' = toRealFloat r
      sigma' = toRealFloat sigma
      
      d1 = (log (s' / k') + (r' + sigma' * sigma' / 2) * t') / (sigma' * sqrt t')
      d2 = d1 - sigma' * sqrt t'
      
      nd1 = Normal.cumulative (Normal.standard) d1
      nd2 = Normal.cumulative (Normal.standard) d2
      
      price = case optionType of
        Call -> s' * nd1 - k' * exp (-r' * t') * nd2
        Put -> k' * exp (-r' * t') * (1 - nd2) - s' * (1 - nd1)
      
  in fromFloatDigits price

-- | Calculate only Delta
blackScholesDelta :: Scientific -> Scientific -> Scientific -> Scientific -> Scientific -> OptionType -> Scientific
blackScholesDelta s k t r sigma optionType =
  let (delta, _, _, _, _) = calculateGreeks s k t r sigma optionType
  in delta

-- | Calculate only Gamma
blackScholesGamma :: Scientific -> Scientific -> Scientific -> Scientific -> Scientific
blackScholesGamma s k t r sigma =
  let (_, gamma, _, _, _) = calculateGreeks s k t r sigma Call
  in gamma

-- | Calculate only Theta
blackScholesTheta :: Scientific -> Scientific -> Scientific -> Scientific -> Scientific -> OptionType -> Scientific
blackScholesTheta s k t r sigma optionType =
  let (_, _, theta, _, _) = calculateGreeks s k t r sigma optionType
  in theta

-- | Calculate only Vega
blackScholesVega :: Scientific -> Scientific -> Scientific -> Scientific -> Scientific
blackScholesVega s k t r sigma =
  let (_, _, _, vega, _) = calculateGreeks s k t r sigma Call
  in vega

-- | Calculate only Rho
blackScholesRho :: Scientific -> Scientific -> Scientific -> Scientific -> Scientific -> OptionType -> Scientific
blackScholesRho s k t r sigma optionType =
  let (_, _, _, _, rho) = calculateGreeks s k t r sigma optionType
  in rho

-- | Estimate annualized volatility from historical data
-- For now, returns a default value. In production, calculate from historical prices
annualizedVolatility :: [Scientific] -> Scientific
annualizedVolatility prices =
  if length prices < 2
    then 0.5  -- Default 50% volatility for crypto options
    else fromFloatDigits 0.5  -- TODO: Calculate from price history
