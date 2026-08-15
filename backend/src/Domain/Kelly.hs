-- | Kelly criterion (formula only). PoP is the existing premium/width heuristic.
-- ponytail: heuristic PoP; swap in BS/norm-CDF when pricing is trusted
module Domain.Kelly
  ( kellyFraction
  , kellyQuantity
  , kellyEdgeGone
  ) where

import Data.Scientific (Scientific, toRealFloat)

-- | f* = max(0, p - (1-p)/b). 0 if b<=0 or p not in (0,1).
kellyFraction :: Scientific -> Scientific -> Scientific
kellyFraction p b
  | b <= 0 || p <= 0 || p >= 1 = 0
  | otherwise = max 0 (p - (1 - p) / b)

-- | Contracts: floor(stake / maxLoss), 0 if that is < 1.
kellyQuantity
  :: Scientific -- ^ p
  -> Scientific -- ^ maxProfit
  -> Scientific -- ^ maxLoss
  -> Scientific -- ^ riskKellyFraction
  -> Scientific -- ^ bankroll (riskMaxPositionSize)
  -> Scientific -- ^ riskMaxLossPercent
  -> Integer
kellyQuantity p maxProfit maxLoss kellyFrac bankroll maxLossPercent
  | maxLoss <= 0 = 0
  | otherwise =
      let b = maxProfit / maxLoss
          f = kellyFraction p b
          stake = min (f * kellyFrac * bankroll) (bankroll * maxLossPercent / 100)
      in floor (toRealFloat (stake / maxLoss) :: Double)

-- | Close when remaining edge is gone: f*(entryPop, remainingProfit/remainingLoss) <= 0.
kellyEdgeGone :: Scientific -> Scientific -> Scientific -> Scientific -> Bool
kellyEdgeGone entryPop maxProfit maxLoss upl =
  let remP = maxProfit - upl
      remL = maxLoss + upl
  in kellyFraction entryPop (if remL > 0 then remP / remL else 0) <= 0
