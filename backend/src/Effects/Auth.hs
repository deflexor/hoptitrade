{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Auth
  ( AuthEffect (..)
  , authenticateUser
  , verifyToken
  , getCurrentUser
  , runAuthIO
  , extractBearerToken
  , defaultUserId
  ) where

import Crypto.Hash (SHA256 (..), hashWith)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.ByteArray (convert)
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime, addUTCTime, diffUTCTime, nominalDay)
import qualified Data.UUID as UUID
import Domain.Types (TradingMode (..), UserId (..))
import Domain.User (AuthToken (..), User (..), UserCredentials (..))
import Polysemy
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

-- ============================================================================
-- Auth Effect Definition
-- ============================================================================

data AuthEffect m a where
  AuthenticateUser :: UserCredentials -> AuthEffect m (Maybe (User, AuthToken))
  VerifyToken :: AuthToken -> AuthEffect m (Maybe UserId)
  GetCurrentUser :: UserId -> AuthEffect m (Maybe User)

makeSem ''AuthEffect

-- ============================================================================
-- Configuration (env)
-- ============================================================================

-- | Token format: base64(userId|expiryEpoch) "." base64(hmac-sha256(payload))
-- Secret from HOPTITRADE_AUTH_SECRET (required unless HOPTITRADE_ENV=dev).

{-# NOINLINE authSecret #-}
authSecret :: BS.ByteString
authSecret = unsafePerformIO loadAuthSecret

loadAuthSecret :: IO BS.ByteString
loadAuthSecret = do
  mSecret <- lookupEnv "HOPTITRADE_AUTH_SECRET"
  mEnv <- lookupEnv "HOPTITRADE_ENV"
  case mSecret of
    Just s | not (null s) -> pure $ encodeUtf8 $ Text.pack s
    _ -> case mEnv of
      Just "dev" -> do
        putStrLn "WARNING: Using dev auth secret. Set HOPTITRADE_AUTH_SECRET for production."
        pure "hoptitrade-dev-only-secret-do-not-use-in-prod"
      _ -> do
        putStrLn "FATAL: HOPTITRADE_AUTH_SECRET is required (or set HOPTITRADE_ENV=dev)."
        error "HOPTITRADE_AUTH_SECRET must be set unless HOPTITRADE_ENV=dev"

tokenTTLSeconds :: Integer
tokenTTLSeconds = 86400  -- 24 hours

defaultUserId :: UserId
defaultUserId = UserId $ read "550e8400-e29b-41d4-a716-446655440000"

{-# NOINLINE bootstrapUsername #-}
bootstrapUsername :: Text
bootstrapUsername = unsafePerformIO $ do
  m <- lookupEnv "HOPTITRADE_USER"
  pure $ maybe "opti" Text.pack m

{-# NOINLINE bootstrapPasswordHash #-}
bootstrapPasswordHash :: Text
bootstrapPasswordHash = unsafePerformIO $ do
  mHash <- lookupEnv "HOPTITRADE_PASSWORD_HASH"
  mPass <- lookupEnv "HOPTITRADE_PASSWORD"
  mEnv <- lookupEnv "HOPTITRADE_ENV"
  case mHash of
    Just h | not (null h) -> pure $ Text.pack h
    _ -> case mPass of
      Just p -> pure $ hashPassword (Text.pack p)
      Nothing -> case mEnv of
        Just "dev" -> do
          putStrLn "WARNING: Using default dev password. Set HOPTITRADE_PASSWORD_HASH."
          pure $ hashPassword "opti"
        _ -> do
          putStrLn "FATAL: Set HOPTITRADE_PASSWORD_HASH (or HOPTITRADE_PASSWORD / HOPTITRADE_ENV=dev)."
          error "HOPTITRADE_PASSWORD_HASH required unless HOPTITRADE_ENV=dev"

hashPassword :: Text -> Text
hashPassword password =
  let digest = convert (hashWith SHA256 (encodeUtf8 password)) :: BS.ByteString
  in decodeUtf8 $ BA.convertToBase BA.Base64 digest

-- ============================================================================
-- Token helpers
-- ============================================================================

-- Approximate POSIX seconds from UTCTime (MJD offset 40587)
utcTimeToPOSIXSecondsApprox :: UTCTime -> Integer
utcTimeToPOSIXSecondsApprox t =
  let mjdEpoch = read "1858-11-17 00:00:00 UTC" :: UTCTime
      days = floor (diffUTCTime t mjdEpoch / nominalDay) :: Integer
  in (days - 40587) * 86400

createToken :: UTCTime -> UserId -> AuthToken
createToken now (UserId uid) = AuthToken $ Text.intercalate "." [payload, signature]
  where
    expiry = addUTCTime (fromInteger tokenTTLSeconds) now
    expiryEpoch = utcTimeToPOSIXSecondsApprox expiry
    raw = Text.pack (UUID.toString uid) <> "|" <> Text.pack (show expiryEpoch)
    uidBytes = encodeUtf8 raw
    payload = decodeUtf8 $ BA.convertToBase BA.Base64 uidBytes
    mac = hmac authSecret uidBytes :: HMAC SHA256
    signature = decodeUtf8 $ BA.convertToBase BA.Base64 (convert mac :: BS.ByteString)

verifySignedToken :: UTCTime -> AuthToken -> Maybe UserId
verifySignedToken now (AuthToken token) = do
  let parts = Text.splitOn "." token
  case parts of
    [payload, signature] -> do
      payloadBytes <- eitherToMaybe $ BA.convertFromBase BA.Base64 (encodeUtf8 payload)
      let raw = decodeUtf8 payloadBytes
          segs = Text.splitOn "|" raw
      case segs of
        [uidText, expiryText] -> do
          uid <- UUID.fromString (Text.unpack uidText)
          expiryEpoch <- readMaybeInt expiryText
          let nowEpoch = utcTimeToPOSIXSecondsApprox now
          if nowEpoch > expiryEpoch
            then Nothing
            else do
              let expectedMac = hmac authSecret payloadBytes :: HMAC SHA256
                  expectedSig = decodeUtf8 $ BA.convertToBase BA.Base64 (convert expectedMac :: BS.ByteString)
              if signature == expectedSig
                then Just (UserId uid)
                else Nothing
        _ -> Nothing
    _ -> Nothing
  where
    eitherToMaybe = either (const Nothing) Just
    readMaybeInt t = case reads (Text.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing

-- ============================================================================
-- Bearer Token Extraction
-- ============================================================================

extractBearerToken :: Text -> Maybe Text
extractBearerToken headerVal =
  let (prefix, token) = Text.splitAt 7 headerVal
  in if Text.toLower prefix == "bearer "
     then Just $ Text.strip token
     else Nothing

-- ============================================================================
-- IO Interpreter
-- ============================================================================

runAuthIO :: Members '[Embed IO] r
          => UTCTime
          -> Sem (AuthEffect ': r) a
          -> Sem r a
runAuthIO now = interpret $ \case
  AuthenticateUser creds -> embed @IO $ do
    let passwordOk = hashPassword (credPassword creds) == bootstrapPasswordHash
    if credUsername creds == bootstrapUsername && passwordOk
      then do
        let user = User
              { userId = defaultUserId
              , userUsername = bootstrapUsername
              , userEmail = Just $ bootstrapUsername <> "@hoptitrade.io"
              , userTradingMode = Manual
              , userCreatedAt = now
              , userLastLogin = Just now
              }
        pure $ Just (user, createToken now defaultUserId)
      else pure Nothing

  VerifyToken token -> embed @IO $
    pure $ verifySignedToken now token

  GetCurrentUser uid -> embed @IO $
    if uid == defaultUserId
      then pure $ Just User
        { userId = uid
        , userUsername = bootstrapUsername
        , userEmail = Just $ bootstrapUsername <> "@hoptitrade.io"
        , userTradingMode = Manual
        , userCreatedAt = now
        , userLastLogin = Just now
        }
      else pure Nothing
