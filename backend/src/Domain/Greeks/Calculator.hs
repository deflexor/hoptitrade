{-# LANGUAGE DerivingStrategies #-}

module Domain.Greeks.Calculator
  ( calculateGreeks
  , blackScholes
  , normalCDF
  , normalPDF
  ) where

import Data.Scientific (Scientific, toRealFloat, fromFloatDigits)
import Domain.Option (OptionType (..))

-- ============================================================================
-- Normal Distribution Approximation (Abramowitz and Stegun)
-- ============================================================================

-- | Approximation of the standard normal CDF
-- Uses the approximation from Abramowitz and Stegun formula 26.2.17
normalCDF :: Double -> Double
normalCDF x
  | x < 0     = 1 - normalCDF (-x)
  | otherwise = 1 - normalPDF x * t * (polynomial t)
  where
    t = 1 / (1 + 0.2316419 * x)
    polynomial t' = 0.319381530 * t' 
                  - 0.356563782 * t'^2 
                  + 1.781477937 * t'^3 
                  - 1.821255978 * t'^4 
                  + 1.330274429 * t'^5

-- | Standard normal PDF
normalPDF :: Double -> Double
normalPDF x = (1 / sqrt (2 * pi)) * exp (-0.5 * x^2)

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
      t' = max 0.0001 $ toRealFloat t  -- Prevent division by zero
      r' = toRealFloat r
      sigma' = toRealFloat sigma
      
      d1 = (log (s' / k') + (r' + sigma' * sigma' / 2) * t') / (sigma' * sqrt t')
      d2 = d1 - sigma' * sqrt t'
      
      nd1 = normalCDF d1
      nd2 = normalCDF d2
      
      pdf_d1 = normalPDF d1
      
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
      t' = max 0.0001 $ toRealFloat t
      r' = toRealFloat r
      sigma' = toRealFloat sigma
      
      d1 = (log (s' / k') + (r' + sigma' * sigma' / 2) * t') / (sigma' * sqrt t')
      d2 = d1 - sigma' * sqrt t'
      
      nd1 = normalCDF d1
      nd2 = normalCDF d2
      
      price = case optionType of
        Call -> s' * nd1 - k' * exp (-r' * t') * nd2
        Put -> k' * exp (-r' * t') * (1 - nd2) - s' * (1 - nd1)
      
  in fromFloatDigits price
