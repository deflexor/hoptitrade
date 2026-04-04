{-# LANGUAGE DerivingStrategies #-}

module Domain.User
  ( User (..)
  , UserCredentials (..)
  , AuthToken (..)
  , defaultUser
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Types
  ( TradingMode (..)
  , UserId (..)
  )
import GHC.Generics (Generic)

-- ============================================================================
-- User Domain Model
-- ============================================================================

data User = User
  { userId :: UserId
  , userUsername :: Text
  , userEmail :: Maybe Text
  , userTradingMode :: TradingMode
  , userCreatedAt :: UTCTime
  , userLastLogin :: Maybe UTCTime
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Authentication Types
-- ============================================================================

data UserCredentials = UserCredentials
  { credUsername :: Text
  , credPassword :: Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype AuthToken = AuthToken { unAuthToken :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- ============================================================================
-- Default User (for development)
-- ============================================================================

defaultUser :: UserId -> UTCTime -> User
defaultUser uid now = User
  { userId = uid
  , userUsername = "opti"
  , userEmail = Just "opti@hoptitrade.io"
  , userTradingMode = Manual
  , userCreatedAt = now
  , userLastLogin = Nothing
  }
