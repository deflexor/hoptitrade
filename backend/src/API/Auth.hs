{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Auth
  ( AuthAPI
  , authServer
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Domain.User (AuthToken (..), User (..), UserCredentials (..))
import Effects.Auth
import GHC.Generics (Generic)
import Polysemy
import Servant

-- ============================================================================
-- API Type
-- ============================================================================

type AuthAPI =
  "auth" :> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
  :<|> "auth" :> "logout" :> Header "Authorization" Text :> Post '[JSON] LogoutResponse
  :<|> "auth" :> "verify" :> Header "Authorization" Text :> Get '[JSON] VerifyResponse

-- ============================================================================
-- Request/Response Types
-- ============================================================================

data LoginRequest = LoginRequest
  { loginUsername :: Text
  , loginPassword :: Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LoginResponse = LoginResponse
  { loginSuccess :: Bool
  , loginToken :: Maybe Text
  , loginUser :: Maybe User
  , loginError :: Maybe Text
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data LogoutResponse = LogoutResponse
  { logoutSuccess :: Bool
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VerifyResponse = VerifyResponse
  { verifyValid :: Bool
  , verifyUser :: Maybe User
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ============================================================================
-- Server
-- ============================================================================

authServer :: Members '[AuthEffect, Embed IO] r
           => ServerT AuthAPI (Sem r)
authServer = loginHandler :<|> logoutHandler :<|> verifyHandler
  where
    loginHandler req = do
      let creds = UserCredentials
            { credUsername = loginUsername req
            , credPassword = loginPassword req
            }
      mResult <- authenticateUser creds
      case mResult of
        Just (user, AuthToken token) ->
          pure $ LoginResponse
            { loginSuccess = True
            , loginToken = Just token
            , loginUser = Just user
            , loginError = Nothing
            }
        Nothing ->
          pure $ LoginResponse
            { loginSuccess = False
            , loginToken = Nothing
            , loginUser = Nothing
            , loginError = Just "Invalid credentials"
            }

    logoutHandler _mToken = do
      -- TODO: Implement token invalidation
      pure $ LogoutResponse { logoutSuccess = True }

    verifyHandler mToken = case mToken of
      Just token -> do
        mUid <- verifyToken $ AuthToken token
        case mUid of
          Just uid -> do
            mUser <- getCurrentUser uid
            pure $ VerifyResponse
              { verifyValid = True
              , verifyUser = mUser
              }
          Nothing ->
            pure $ VerifyResponse
              { verifyValid = False
              , verifyUser = Nothing
              }
      Nothing ->
        pure $ VerifyResponse
          { verifyValid = False
          , verifyUser = Nothing
          }
