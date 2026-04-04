{-# LANGUAGE DerivingStrategies #-}

module Domain.Option
  ( OptionLeg (..)
  , OptionContract (..)
  , OptionChain (..)
  , Strike (..)
  , Expiration (..)
  , isCall
  , isPut
  , optionMoneyness
  ) where

import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Types
  ( InstrumentId (..)
  , LegId (..)
  , OptionType (..)
  , Price
  , Quantity
  , Side (..)
  )
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- ============================================================================
-- Option Leg (part of a multi-leg strategy)
-- ============================================================================

data OptionLeg = OptionLeg
  { legId :: LegId
  , instrumentId :: InstrumentId
  , side :: Side
  , optionType :: OptionType
  , strike :: Strike
  , expiration :: Expiration
  , quantity :: Quantity
  , entryPrice :: Maybe Price
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Full Option Contract (market data)
-- ============================================================================

data OptionContract = OptionContract
  { contractInstrumentId :: InstrumentId
  , contractUnderlying :: Text
  , contractOptionType :: OptionType
  , contractStrike :: Strike
  , contractExpiration :: Expiration
  , contractBid :: Maybe Price
  , contractAsk :: Maybe Price
  , contractLastPrice :: Maybe Price
  , contractVolume :: Maybe Quantity
  , contractOpenInterest :: Maybe Quantity
  , contractImpliedVol :: Maybe Scientific
  , contractDelta :: Maybe Scientific
  , contractGamma :: Maybe Scientific
  , contractTheta :: Maybe Scientific
  , contractVega :: Maybe Scientific
  , contractTimestamp :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Option Chain (all contracts for an underlying)
-- ============================================================================

data OptionChain = OptionChain
  { chainUnderlying :: Text
  , chainExpiration :: Expiration
  , chainContracts :: [OptionContract]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Value Types
-- ============================================================================

newtype Strike = Strike { unStrike :: Scientific }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Ord, Num, Fractional)

newtype Expiration = Expiration { unExpiration :: UTCTime }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, Ord)

-- ============================================================================
-- Utility Functions
-- ============================================================================

isCall :: OptionType -> Bool
isCall Call = True
isCall Put = False

isPut :: OptionType -> Bool
isPut = not . isCall

optionMoneyness :: Scientific -> Strike -> OptionType -> Scientific
optionMoneyness spotPrice (Strike strike) optionType =
  case optionType of
    Call -> spotPrice - strike
    Put -> strike - spotPrice
