{-# LANGUAGE DerivingStrategies #-}

module Domain.Types
  ( -- * Core Identifiers
    UserId (..)
  , StrategyId (..)
  , PositionId (..)
  , OrderId (..)
  , LegId (..)
  , InstrumentId (..)
  , ApiKeyId (..)
    -- * Common Types
  , Price
  , Quantity
  , Timestamp
  , Percentage
  , Currency (..)
  , Side (..)
  , OptionType (..)
  , OrderStatus (..)
  , PositionStatus (..)
  , TradingMode (..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData (..))

-- ============================================================================
-- Core Identifiers
-- ============================================================================

newtype UserId = UserId { unUserId :: UUID }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype StrategyId = StrategyId { unStrategyId :: UUID }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

newtype PositionId = PositionId { unPositionId :: UUID }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData)

newtype OrderId = OrderId { unOrderId :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype LegId = LegId { unLegId :: UUID }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype InstrumentId = InstrumentId { unInstrumentId :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

newtype ApiKeyId = ApiKeyId { unApiKeyId :: UUID }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- ============================================================================
-- Common Types
-- ============================================================================

type Price = Scientific
type Quantity = Scientific
type Timestamp = UTCTime
type Percentage = Scientific

data Currency
  = BTC
  | ETH
  | USDT
  | USD
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Side
  = Buy
  | Sell
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OptionType
  = Call
  | Put
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OrderStatus
  = OrderPending
  | OrderOpening
  | OrderActive
  | OrderPartialFill Quantity
  | OrderClosing
  | OrderClosed
  | OrderCancelled
  | OrderFailed Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PositionStatus
  = PositionOpening
  | PositionActive
  | PositionPartial Quantity
  | PositionClosing
  | PositionClosed
  | PositionCancelled
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance FromHttpApiData PositionStatus where
  parseUrlPiece txt = case Text.toLower txt of
    "opening" -> Right PositionOpening
    "active" -> Right PositionActive
    "closing" -> Right PositionClosing
    "closed" -> Right PositionClosed
    "cancelled" -> Right PositionCancelled
    _ -> Left "Invalid PositionStatus"

data TradingMode
  = Manual
  | Auto
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance FromHttpApiData TradingMode where
  parseUrlPiece txt = case Text.toLower txt of
    "manual" -> Right Manual
    "auto" -> Right Auto
    _ -> Left "Invalid TradingMode. Expected 'manual' or 'auto'"
