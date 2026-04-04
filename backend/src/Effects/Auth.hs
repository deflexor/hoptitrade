{-# LANGUAGE TemplateHaskell #-}

module Effects.Auth
  ( AuthEffect (..)
  , authenticateUser
  , verifyToken
  , getCurrentUser
  , runAuthIO
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Types (TradingMode (..), UserId (..))
import Domain.User (AuthToken (..), User (..), UserCredentials (..))
import Polysemy
import Polysemy.Embed

-- ============================================================================
-- Auth Effect Definition
-- ============================================================================

data AuthEffect m a where
  AuthenticateUser :: UserCredentials -> AuthEffect m (Maybe (User, AuthToken))
  VerifyToken :: AuthToken -> AuthEffect m (Maybe UserId)
  GetCurrentUser :: UserId -> AuthEffect m (Maybe User)

makeSem ''AuthEffect

-- ============================================================================
-- IO Interpreter (Production)
-- ============================================================================

runAuthIO :: Members '[Embed IO] r
          => UTCTime
          -> Sem (AuthEffect ': r) a
          -> Sem r a
runAuthIO now = interpret $ \case
  AuthenticateUser creds -> embed @IO $ do
    -- TODO: Implement actual authentication with database
    -- For now, use hardcoded default user
    if credUsername creds == "opti" && credPassword creds == "opti"
      then do
        let uid = UserId $ read "550e8400-e29b-41d4-a716-446655440000"
        let user = User
              { userId = uid
              , userUsername = "opti"
              , userEmail = Just "opti@hoptitrade.io"
              , userTradingMode = Manual
              , userCreatedAt = now
              , userLastLogin = Just now
              }
        let token = AuthToken "demo-token-12345"
        pure $ Just (user, token)
      else pure Nothing

  VerifyToken (AuthToken token) -> embed @IO $ do
    -- TODO: Implement JWT verification
    if token == "demo-token-12345"
      then pure $ Just (UserId $ read "550e8400-e29b-41d4-a716-446655440000")
      else pure Nothing

  GetCurrentUser uid -> embed @IO $ do
    -- TODO: Fetch from database
    if unUserId uid == read "550e8400-e29b-41d4-a716-446655440000"
      then pure $ Just User
        { userId = uid
        , userUsername = "opti"
        , userEmail = Just "opti@hoptitrade.io"
        , userTradingMode = Manual
        , userCreatedAt = now
        , userLastLogin = Just now
        }
      else pure Nothing
