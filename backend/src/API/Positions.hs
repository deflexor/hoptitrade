{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module API.Positions
  ( PositionsAPI
  , positionsServer
  ) where

import App.PositionOrchestrator
  ( OpenLegSpec (..)
  , OpenPositionSpec (..)
  , closePositionOnExchange
  , openMultiLegPosition
  )
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Sql (ConnectionPool)
import Domain.Greeks (Greeks)
import Domain.Position (Position (..), PositionLeg (..), isPositionActive)
import qualified Domain.Position as Pos
import qualified Domain.Settings as DS
import Domain.Settings (settingsToBrokerConfig)
import Domain.Types
  ( InstrumentId (..)
  , OrderId (..)
  , PositionId (..)
  , PositionStatus (..)
  , Side (..)
  , StrategyId (..)
  , UserId
  )
import Domain.User (AuthToken (..))
import Effects.Auth (AuthEffect, extractBearerToken, verifyToken)
import Effects.Position
import Effects.Settings (SettingsEffect, getSettings)
import GHC.Generics (Generic)
import Polysemy
import Servant

type PositionsAPI =
  "positions" :> Header "Authorization" Text :> QueryParam "status" PositionStatus :> Get '[JSON] [PositionResponse]
  :<|> "positions" :> Header "Authorization" Text :> Capture "positionId" PositionId :> Get '[JSON] (Maybe PositionResponse)
  :<|> "positions" :> Header "Authorization" Text :> Capture "positionId" PositionId :> "close" :> Post '[JSON] ClosePositionResponse
  :<|> "positions" :> "open" :> Header "Authorization" Text :> ReqBody '[JSON] OpenPositionRequest :> Post '[JSON] OpenPositionResponse

data PositionResponse = PositionResponse
  { positionId :: PositionId
  , positionStatus :: PositionStatus
  , positionLegs :: [PositionLegResponse]
  , positionGreeks :: Maybe Greeks
  , positionRealizedPL :: Maybe Scientific
  , positionUnrealizedPL :: Maybe Scientific
  , positionMarginUsed :: Scientific
  , positionMaxProfit :: Maybe Scientific
  , positionMaxLoss :: Maybe Scientific
  , positionOpenedAt :: Maybe UTCTime
  , positionClosedAt :: Maybe UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PositionLegResponse = PositionLegResponse
  { posLegOrderId :: Text
  , posLegInstrumentId :: Text
  , posLegSide :: Text
  , posLegQuantity :: Scientific
  , posLegFilledPrice :: Scientific
  , posLegFilledAt :: UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ClosePositionResponse = ClosePositionResponse
  { closeSuccess :: Bool
  , closePositionId :: PositionId
  , closeError :: Maybe Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OpenLegRequest = OpenLegRequest
  { olrInstrumentId :: Text
  , olrSide :: Text
  , olrQuantity :: Scientific
  , olrLimitPrice :: Maybe Scientific
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OpenPositionRequest = OpenPositionRequest
  { oprStrategyId :: StrategyId
  , oprUnderlying :: Text
  , oprLegs :: [OpenLegRequest]
  , oprMaxProfit :: Maybe Scientific
  , oprMaxLoss :: Maybe Scientific
  , oprEntryPremium :: Maybe Scientific
  , oprMargin :: Scientific
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data OpenPositionResponse = OpenPositionResponse
  { openSuccess :: Bool
  , openPosition :: Maybe PositionResponse
  , openError :: Maybe Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

positionsServer
  :: Members '[AuthEffect, PositionEffect, SettingsEffect, Embed IO] r
  => ConnectionPool
  -> ServerT PositionsAPI (Sem r)
positionsServer pool =
  listPositionsHandler
  :<|> getPositionHandler
  :<|> closePositionHandler
  :<|> openPositionHandler
  where
    resolveUser mAuthHeader = case mAuthHeader >>= extractBearerToken of
      Nothing -> pure Nothing
      Just token -> verifyToken (AuthToken token)

    listPositionsHandler mAuthHeader mStatus = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure []
        Just uid -> map toPositionResponse <$> listPositions uid mStatus

    getPositionHandler mAuthHeader positionId = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure Nothing
        Just _ -> fmap toPositionResponse <$> getPosition positionId

    closePositionHandler mAuthHeader positionId = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ ClosePositionResponse False positionId (Just "Unauthorized")
        Just uid -> do
          mPos <- getPosition positionId
          case mPos of
            Nothing -> pure $ ClosePositionResponse False positionId (Just "Position not found")
            Just pos -> do
              settings <- getSettings uid
              case settingsToBrokerConfig settings of
                Nothing -> do
                  _ <- closePosition positionId
                  pure $ ClosePositionResponse True positionId Nothing
                Just config -> do
                  result <- embed $ closePositionOnExchange pool config uid pos
                  case result of
                    Left err -> pure $ ClosePositionResponse False positionId (Just err)
                    Right _ -> pure $ ClosePositionResponse True positionId Nothing

    openPositionHandler mAuthHeader req = do
      mUid <- resolveUser mAuthHeader
      case mUid of
        Nothing -> pure $ OpenPositionResponse False Nothing (Just "Unauthorized")
        Just uid -> do
          settings <- getSettings uid
          openPositions <- listPositions uid Nothing
          let risk = DS.settingsRiskParams settings
              activeCount = length $ filter (isPositionActive . Pos.positionStatus) openPositions
          if activeCount >= DS.riskMaxOpenPositions risk
            then pure $ OpenPositionResponse False Nothing (Just "Max open positions reached")
            else case settingsToBrokerConfig settings of
              Nothing -> pure $ OpenPositionResponse False Nothing (Just "No active broker configured")
              Just config -> do
                let legs = mapMaybe parseLeg (oprLegs req)
                    spec = OpenPositionSpec
                      { opsStrategyId = oprStrategyId req
                      , opsUnderlying = oprUnderlying req
                      , opsLegs = legs
                      , opsMaxProfit = oprMaxProfit req
                      , opsMaxLoss = oprMaxLoss req
                      , opsEntryPremium = oprEntryPremium req
                      , opsMargin = oprMargin req
                      }
                if length legs /= length (oprLegs req)
                  then pure $ OpenPositionResponse False Nothing (Just "Invalid leg side")
                  else do
                    result <- embed $ openMultiLegPosition pool config uid spec
                    case result of
                      Left err -> pure $ OpenPositionResponse False Nothing (Just err)
                      Right pos -> pure $ OpenPositionResponse True (Just $ toPositionResponse pos) Nothing

parseLeg :: OpenLegRequest -> Maybe OpenLegSpec
parseLeg OpenLegRequest{..} = do
  side <- case olrSide of
    "buy"  -> Just Buy
    "Buy"  -> Just Buy
    "sell" -> Just Sell
    "Sell" -> Just Sell
    _      -> Nothing
  pure OpenLegSpec
    { olsInstrumentId = InstrumentId olrInstrumentId
    , olsSide = side
    , olsQuantity = olrQuantity
    , olsLimitPrice = olrLimitPrice
    }

toPositionResponse :: Position -> PositionResponse
toPositionResponse pos = PositionResponse
  { positionId = Pos.positionId pos
  , positionStatus = Pos.positionStatus pos
  , positionLegs = map toLegResponse (Pos.positionLegs pos)
  , positionGreeks = Pos.positionGreeks pos
  , positionRealizedPL = Pos.positionRealizedPL pos
  , positionUnrealizedPL = Pos.positionUnrealizedPL pos
  , positionMarginUsed = Pos.positionMarginUsed pos
  , positionMaxProfit = Pos.positionMaxProfit pos
  , positionMaxLoss = Pos.positionMaxLoss pos
  , positionOpenedAt = Pos.positionOpenedAt pos
  , positionClosedAt = Pos.positionClosedAt pos
  }

toLegResponse :: PositionLeg -> PositionLegResponse
toLegResponse leg = PositionLegResponse
  { posLegOrderId = let OrderId o = Pos.posLegOrderId leg in o
  , posLegInstrumentId = let InstrumentId i = Pos.posLegInstrumentId leg in i
  , posLegSide = case Pos.posLegSide leg of Buy -> "buy"; Sell -> "sell"
  , posLegQuantity = Pos.posLegQuantity leg
  , posLegFilledPrice = Pos.posLegFilledPrice leg
  , posLegFilledAt = Pos.posLegFilledAt leg
  }
