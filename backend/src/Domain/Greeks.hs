{-# LANGUAGE DerivingStrategies #-}

module Domain.Greeks
  ( Greeks (..)
  , GreeksSnapshot (..)
  , emptyGreeks
  , calculateNetGreeks
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- ============================================================================
-- Greeks Representation
-- ============================================================================

data Greeks = Greeks
  { greeksDelta :: Scientific
  , greeksGamma :: Scientific
  , greeksTheta :: Scientific
  , greeksVega :: Scientific
  , greeksRho :: Scientific
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Greeks with Timestamp (for tracking changes)
-- ============================================================================

data GreeksSnapshot = GreeksSnapshot
  { snapshotGreeks :: Greeks
  , snapshotTimestamp :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Utility Functions
-- ============================================================================

emptyGreeks :: Greeks
emptyGreeks = Greeks 0 0 0 0 0

calculateNetGreeks :: [(Scientific, Greeks)] -> Greeks
-- ^ Calculate weighted sum of Greeks
-- Each tuple is (weight, Greeks) where weight is typically quantity or position size
calculateNetGreeks weightedGreeks =
  foldr
    (\(weight, Greeks d g t v r) (Greeks d' g' t' v' r') ->
      Greeks (d * weight + d') (g * weight + g') (t * weight + t') (v * weight + v') (r * weight + r')
    )
    emptyGreeks
    weightedGreeks
