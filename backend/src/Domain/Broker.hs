{-# LANGUAGE DerivingStrategies #-}

module Domain.Broker
  ( -- * Price Level
    PriceLevel (..)
    -- * Broker Types
  , Broker (..)
  , BrokerMode (..)
  , BrokerConfig (..)
  , ExchangeType (..)
  , BrokerConnectionStatus (..)
    -- * Trading Hours (MOEX-specific)
  , TradingHours (..)
  , TradingSession (..)
  , TradingStatus (..)
  , DaySession (..)
  , SessionSegment (..)
  , isMarketOpen
  , isTradingAvailable
    -- * Liquidity Assessment
  , LiquidityAssessment (..)
  , SlippageEstimate (..)
  , SlippageConfidence (..)
  , defaultLiquidityThreshold
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (TimeOfDay, UTCTime, Day)
import Domain.Types (InstrumentId, Percentage, Quantity)
import GHC.Generics (Generic)

-- ============================================================================
-- Price Level (Order Book)
-- ============================================================================

data PriceLevel = PriceLevel
  { priceLevelPrice :: Scientific
  , priceLevelSize :: Scientific
  } deriving stock (Eq, Show)

-- ============================================================================
-- Broker Types
-- ============================================================================

data Broker
  = OKX
  | TBank
  | Bybit
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BrokerMode
  = Sandbox
  | Real
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data BrokerConnectionStatus
  = Connected
  | Disconnected
  | Authenticating
  | Error Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Exchange classification for behavior differences
data ExchangeType
  = CryptoExchange      -- ^ 24/7 trading (OKX)
  | MOEXExchange        -- ^ Scheduled trading hours with breaks
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Broker-specific configuration
-- Contains credentials and mode settings
-- Note: Different API keys for Sandbox vs Real modes (especially for T-Bank)
data BrokerConfig
  = OKXConfig
      { okxApiKey :: Text
      , okxSecret :: Text
      , okxPassphrase :: Text
      , okxDemoMode :: Bool
      }
  | TBankConfig
      { tbankToken :: Text
      , tbankAccountId :: Maybe Text
      , tbankMode :: BrokerMode
      }
  | BybitConfig
      { bybitApiKey :: Text
      , bybitApiSecret :: Text
      , bybitTestnet :: Bool
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Trading Hours (MOEX-specific)
-- ============================================================================

-- | Trading session information for MOEX
data TradingSession = TradingSession
  { tsExchange :: Text           -- ^ Exchange code (e.g., "MOEX", "SPB")
  , tsInstrumentType :: Text     -- ^ Instrument type (e.g., "shares", "futures")
  , tsDays :: [DaySession]       -- ^ Trading days and hours
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DaySession = DaySession
  { dsDate :: Day
  , dsIsTradingDay :: Bool
  , dsSessions :: [SessionSegment]
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A continuous trading segment within a day
data SessionSegment = SessionSegment
  { ssStart :: TimeOfDay
  , ssEnd :: TimeOfDay
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Comprehensive trading hours for an instrument
data TradingHours = TradingHours
  { thExchangeType :: ExchangeType
  , thSessions :: [TradingSession]
  , thCurrentStatus :: TradingStatus
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Real-time trading status
data TradingStatus
  = NotAvailable           -- ^ No trading scheduled
  | PreOpen               -- ^ Pre-market/Opening auction
  | OpeningAuction        -- ^ Opening auction in progress
  | Trading               -- ^ Normal trading
  | ClosingAuction        -- ^ Closing auction in progress
  | Closed                -- ^ Market closed for the day
  | Break                 -- ^ Trading break between sessions
  | Suspended             -- ^ Trading suspended
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Check if market is currently open for trading
isMarketOpen :: TradingStatus -> Bool
isMarketOpen Trading = True
isMarketOpen _ = False

-- | Check if any trading activity is available (including auctions)
isTradingAvailable :: TradingStatus -> Bool
isTradingAvailable OpeningAuction = True
isTradingAvailable ClosingAuction = True
isTradingAvailable Trading = True
isTradingAvailable _ = False

-- ============================================================================
-- Liquidity Assessment (MOEX-specific)
-- ============================================================================

-- | Pre-trade liquidity check result
-- Critical for MOEX where liquidity can be very thin
data LiquidityAssessment = LiquidityAssessment
  { laInstrumentId :: InstrumentId
  , laTimestamp :: UTCTime
  , laBidVolume :: Quantity           -- ^ Total volume at or above best bid
  , laAskVolume :: Quantity           -- ^ Total volume at or below best ask
  , laSpreadPercent :: Percentage     -- ^ Bid-ask spread as percentage
  , laSlippageEstimate :: SlippageEstimate
  , laIsLiquid :: Bool                -- ^ Safe to trade?
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Estimated slippage for a proposed trade
data SlippageEstimate = SlippageEstimate
  { seForQuantity :: Quantity         -- ^ Quantity being evaluated
  , seExpectedSlippage :: Percentage  -- ^ Estimated slippage percentage
  , seMaxSlippage :: Percentage       -- ^ Worst-case estimate
  , seConfidence :: SlippageConfidence
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data SlippageConfidence
  = HighConfidence      -- ^ Good order book depth
  | MediumConfidence    -- ^ Moderate depth
  | LowConfidence       -- ^ Thin order book
  | UnknownConfidence   -- ^ Cannot assess
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Default threshold for warning users about slippage
-- MOEX options often have wider spreads than crypto
defaultLiquidityThreshold :: Percentage
defaultLiquidityThreshold = 0.02  -- 2%
