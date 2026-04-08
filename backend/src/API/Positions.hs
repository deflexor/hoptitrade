{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Positions
  ( PositionsAPI
  , positionsServer
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Domain.Greeks (Greeks)
import Domain.Position (Position (..), PositionLeg (..))
import Domain.Types (OrderId (..), PositionId (..), PositionStatus (..), Side (..), UserId)
import Data.Time (UTCTime)
import Domain.User (AuthToken (..))
import Effects.Auth (AuthEffect, extractBearerToken, verifyToken)
import Effects.Position
import GHC.Generics (Generic)
import Polysemy
import Servant

-- ============================================================================
-- API Type
-- ============================================================================

type PositionsAPI =
  "positions" :> Header "Authorization" Text :> QueryParam "status" PositionStatus :> Get '[JSON] [PositionResponse]
  :<|> "positions" :> Header "Authorization" Text :> Capture "positionId" PositionId :> Get '[JSON] (Maybe PositionResponse)
  :<|> "positions" :> Header "Authorization" Text :> Capture "positionId" PositionId :> "close" :> Post '[JSON] ClosePositionResponse

-- ============================================================================
-- Response Types
-- ============================================================================

data PositionResponse = PositionResponse
  { respPositionId :: PositionId
  , respPositionStatus :: PositionStatus
  , respPositionLegs :: [PositionLegResponse]
  , respPositionGreeks :: Maybe Greeks
  , respPositionRealizedPL :: Maybe Scientific
  , respPositionUnrealizedPL :: Maybe Scientific
  , respPositionMarginUsed :: Scientific
  , respPositionOpenedAt :: Maybe UTCTime
  , respPositionClosedAt :: Maybe UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PositionLegResponse = PositionLegResponse
  { respLegOrderId :: Text
  , respLegSide :: Text
  , respLegQuantity :: Scientific
  , respLegFilledPrice :: Scientific
  , respLegFilledAt :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ClosePositionResponse = ClosePositionResponse
  { closeSuccess :: Bool
  , closePositionId :: PositionId
  , closeError :: Maybe Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Server
-- ============================================================================

positionsServer :: Members '[AuthEffect, PositionEffect, Embed IO] r => ServerT PositionsAPI (Sem r)
positionsServer = listPositionsHandler :<|> getPositionHandler :<|> closePositionHandler
  where
    resolveUser :: Member AuthEffect r => Maybe Text -> Sem r (Maybe UserId)
    resolveUser mAuthHeader = case mAuthHeader >>= extractBearerToken of
      Nothing -> pure Nothing
      Just token -> verifyToken (AuthToken token)

    listPositionsHandler mAuthHeader _mStatus = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure []
        Just _uid -> do
          -- TODO: Pass UserId to listPositions for proper filtering
          pure []

    getPositionHandler mAuthHeader positionId = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure Nothing
        Just _uid -> do
          mPosition <- getPosition positionId
          pure $ fmap toPositionResponse mPosition

    closePositionHandler mAuthHeader positionId = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ ClosePositionResponse
          { closeSuccess = False
          , closePositionId = positionId
          , closeError = Just "Unauthorized"
          }
        Just _uid -> do
          mPosition <- closePosition positionId
          case mPosition of
            Just _ ->
              pure $ ClosePositionResponse
                { closeSuccess = True
                , closePositionId = positionId
                , closeError = Nothing
                }
            Nothing ->
              pure $ ClosePositionResponse
                { closeSuccess = False
                , closePositionId = positionId
                , closeError = Just "Position not found or already closed"
                }

toPositionResponse :: Position -> PositionResponse
toPositionResponse pos = PositionResponse
  { respPositionId = positionId pos
  , respPositionStatus = positionStatus pos
  , respPositionLegs = map toPositionLegResponse $ positionLegs pos
  , respPositionGreeks = positionGreeks pos
  , respPositionRealizedPL = positionRealizedPL pos
  , respPositionUnrealizedPL = positionUnrealizedPL pos
  , respPositionMarginUsed = positionMarginUsed pos
  , respPositionOpenedAt = positionOpenedAt pos
  , respPositionClosedAt = positionClosedAt pos
  }

toPositionLegResponse :: PositionLeg -> PositionLegResponse
toPositionLegResponse leg = PositionLegResponse
  { respLegOrderId = unOrderId $ posLegOrderId leg
  , respLegSide = case posLegSide leg of
      Buy -> "buy"
      Sell -> "sell"
  , respLegQuantity = posLegQuantity leg
  , respLegFilledPrice = posLegFilledPrice leg
  , respLegFilledAt = posLegFilledAt leg
  }
