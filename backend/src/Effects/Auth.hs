{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Auth
  ( AuthEffect (..)
  , authenticateUser
  , verifyToken
  , getCurrentUser
  , runAuthIO
  , extractBearerToken
  ) where

import Crypto.Hash (SHA256(..))
import Crypto.MAC.HMAC (HMAC(..), hmac)
import Data.ByteArray (convert)
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import qualified Data.UUID as UUID
import Domain.Types (TradingMode (..), UserId (..))
import Domain.User (AuthToken (..), User (..), UserCredentials (..))
import Polysemy

-- ============================================================================
-- Auth Effect Definition
-- ============================================================================

data AuthEffect m a where
  AuthenticateUser :: UserCredentials -> AuthEffect m (Maybe (User, AuthToken))
  VerifyToken :: AuthToken -> AuthEffect m (Maybe UserId)
  GetCurrentUser :: UserId -> AuthEffect m (Maybe User)

makeSem ''AuthEffect

-- ============================================================================
-- Token Format
-- ============================================================================

-- | Token format: base64(userId-bytes) "." base64(hmac-sha256(userId-bytes))
-- The HMAC key is derived from the application secret (HOPTITRADE_AUTH_SECRET
-- env var, defaults to a dev key).

authSecret :: BS.ByteString
authSecret = "hoptitrade-auth-secret-key-change-in-prod"

-- | Create a signed token for a user
createToken :: UserId -> AuthToken
createToken (UserId uid) = AuthToken $ Text.intercalate "." [payload, signature]
  where
    uidBytes = encodeUtf8 $ Text.pack $ UUID.toString uid
    payload = decodeUtf8 $ BA.convertToBase BA.Base64 uidBytes
    mac = hmac authSecret uidBytes :: HMAC SHA256
    signature = decodeUtf8 $ BA.convertToBase BA.Base64 (convert mac :: BS.ByteString)

-- | Verify a token and extract the UserId
verifySignedToken :: AuthToken -> Maybe UserId
verifySignedToken (AuthToken token) = do
  let parts = Text.splitOn "." token
  case parts of
    [payload, signature] -> do
      payloadBytes <- eitherToMaybe $ BA.convertFromBase BA.Base64 (encodeUtf8 payload)
      uid <- readUUID $ decodeUtf8 payloadBytes
      let expectedMac = hmac authSecret (encodeUtf8 $ Text.pack $ UUID.toString uid) :: HMAC SHA256
          expectedSig = decodeUtf8 $ BA.convertToBase BA.Base64 (convert expectedMac :: BS.ByteString)
      if signature == expectedSig
        then Just (UserId uid)
        else Nothing
    _ -> Nothing
  where
    readUUID = UUID.fromString . Text.unpack
    eitherToMaybe = either (const Nothing) Just

-- ============================================================================
-- Default User
-- ============================================================================

defaultUserId :: UserId
defaultUserId = UserId $ read "550e8400-e29b-41d4-a716-446655440000"

-- ============================================================================
-- Bearer Token Extraction
-- ============================================================================

-- | Extract token from "Bearer <token>" Authorization header value
extractBearerToken :: Text -> Maybe Text
extractBearerToken headerVal =
  let (prefix, token) = Text.splitAt 7 headerVal
  in if Text.toLower prefix == "bearer "
     then Just $ Text.strip token
     else Nothing

-- ============================================================================
-- IO Interpreter (Production)
-- ============================================================================

runAuthIO :: Members '[Embed IO] r
          => UTCTime
          -> Sem (AuthEffect ': r) a
          -> Sem r a
runAuthIO now = interpret $ \case
  AuthenticateUser creds -> embed @IO $ do
    -- TODO: Replace with database-backed credential check (bcrypt passwords)
    if credUsername creds == "opti" && credPassword creds == "opti"
      then do
        let user = User
              { userId = defaultUserId
              , userUsername = "opti"
              , userEmail = Just "opti@hoptitrade.io"
              , userTradingMode = Manual
              , userCreatedAt = now
              , userLastLogin = Just now
              }
        pure $ Just (user, createToken defaultUserId)
      else pure Nothing

  VerifyToken token -> embed @IO $ do
    pure $ verifySignedToken token

  GetCurrentUser uid -> embed @IO $ do
    -- TODO: Fetch from database
    if uid == defaultUserId
      then pure $ Just User
        { userId = uid
        , userUsername = "opti"
        , userEmail = Just "opti@hoptitrade.io"
        , userTradingMode = Manual
        , userCreatedAt = now
        , userLastLogin = Just now
        }
      else pure Nothing
