{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Encryption
  ( -- * Encryption Context
    EncryptionContext
  , initializeEncryption
    -- * Credential Encryption (PLACEHOLDER - implement proper encryption)
  , encryptCredential
  , decryptCredential
  , hashCredential
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Hash (SHA256(..), hashWith)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)

-- ============================================================================
-- Encryption Context (PLACEHOLDER)
-- ============================================================================

-- | Encryption context containing the master key
-- WARNING: This is a placeholder implementation!
-- In production, use proper AES-256 encryption with cryptonite
newtype EncryptionContext = EncryptionContext
  { encMasterKey :: ByteString
  }

-- | Initialize encryption context
-- For now, generates a dummy key or loads from environment
initializeEncryption :: MonadIO m => m EncryptionContext
initializeEncryption = liftIO $ do
  mKey <- lookupEnv "HOPTITRADE_ENCRYPTION_KEY"
  case mKey of
    Just key -> do
      putStrLn "Encryption: Using key from environment"
      pure $ EncryptionContext (TE.encodeUtf8 $ Text.pack key)
    Nothing -> do
      putStrLn "WARNING: Using placeholder encryption (NOT SECURE FOR PRODUCTION)"
      putStrLn "Set HOPTITRADE_ENCRYPTION_KEY environment variable for real encryption"
      pure $ EncryptionContext "placeholder-key-do-not-use-in-production"

-- ============================================================================
-- Credential Encryption (PLACEHOLDER)
-- ============================================================================

-- | PLACEHOLDER: "Encrypt" a credential
-- Currently just base64 encodes - NOT REAL ENCRYPTION!
-- TODO: Implement proper AES-256-GCM encryption
encryptCredential :: EncryptionContext -> Text -> IO Text
encryptCredential _ctx plaintext = do
  let encoded = Base64.encode (TE.encodeUtf8 plaintext)
  pure (TE.decodeUtf8 encoded)

-- | PLACEHOLDER: "Decrypt" a credential
-- Currently just base64 decodes - NOT REAL DECRYPTION!
-- TODO: Implement proper AES-256-GCM decryption
decryptCredential :: EncryptionContext -> Text -> IO (Maybe Text)
decryptCredential _ctx encrypted = do
  case Base64.decode (TE.encodeUtf8 encrypted) of
    Left _ -> pure Nothing
    Right bs -> pure (Just $ TE.decodeUtf8 bs)

-- | Hash a credential using SHA-256
-- This is one-way and cannot be reversed
hashCredential :: Text -> Text
hashCredential text =
  let textBs = TE.encodeUtf8 text
      hash = hashWith SHA256 textBs
      encoded = Base64.encode $ convert hash
  in TE.decodeUtf8 encoded

-- ============================================================================
-- Security Notes
-- ============================================================================

-- IMPORTANT: This is a PLACEHOLDER implementation!
--
-- For production, implement proper encryption:
-- 1. Use AES-256-GCM from cryptonite
-- 2. Generate random IV for each encryption
-- 3. Store IV with ciphertext
-- 4. Use authenticated encryption (GCM mode)
-- 5. Load encryption key from secure storage (KMS, HSM, etc.)
--
-- Current implementation:
-- - Only base64 encodes credentials (easily reversible!)
-- - Does NOT provide real security
-- - For development/testing only
