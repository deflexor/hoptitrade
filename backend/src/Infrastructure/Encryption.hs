{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Infrastructure.Encryption
  ( -- * Encryption Context
    EncryptionContext
  , initializeEncryption
    -- * Credential Encryption (AES-256-GCM)
  , encryptCredential
  , decryptCredential
  , hashCredential
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEADMode (..), AuthTag (..), Cipher (..), aeadInit, aeadSimpleDecrypt, aeadSimpleEncrypt)
import Crypto.Error (CryptoFailable (..), eitherCryptoError)
import Crypto.Hash (SHA256 (..), hashWith)
import Crypto.Random.Types (getRandomBytes)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)

-- ============================================================================
-- Encryption Context
-- ============================================================================

-- | Encryption context holding an initialized AES-256 cipher.
-- The master key must be exactly 32 bytes (256 bits).
newtype EncryptionContext = EncryptionContext
  { encCipher :: AES256
  }

-- | Initialize encryption context from environment variable.
-- Key must be exactly 32 bytes (base64-decoded if needed, or raw bytes).
initializeEncryption :: MonadIO m => m EncryptionContext
initializeEncryption = liftIO $ do
  mKey <- lookupEnv "HOPTITRADE_ENCRYPTION_KEY"
  keyBytes <- case mKey of
    Just key -> do
      let raw = TE.encodeUtf8 $ Text.pack key
      -- Try base64 decode first; if it fails, use raw bytes (padded/truncated to 32)
      case Base64.decode raw of
        Right bs
          | ByteString.length bs >= 32 -> pure $ ByteString.take 32 bs
          | otherwise -> pure $ padKey raw
        Left _ -> pure $ padKey raw
    Nothing -> do
      putStrLn "WARNING: Generating random encryption key (credentials will not survive restart!)"
      putStrLn "Set HOPTITRADE_ENCRYPTION_KEY (32+ bytes, raw or base64) for persistent encryption."
      getRandomBytes 32

  case cipherInit keyBytes :: CryptoFailable AES256 of
    CryptoFailed err -> do
      putStrLn $ "FATAL: Failed to initialize AES-256 cipher: " ++ show err
      putStrLn "Generating fallback key — credentials will not be recoverable!"
      fallbackKey <- getRandomBytes 32 :: IO ByteString
      case cipherInit fallbackKey of
        CryptoFailed _ -> error "Impossible: getRandomBytes 32 should always produce a valid AES-256 key"
        CryptoPassed c -> pure $ EncryptionContext c
    CryptoPassed cipher -> do
      putStrLn "Encryption: AES-256-GCM initialized"
      pure $ EncryptionContext cipher
  where
    padKey bs =
      -- Pad or truncate to exactly 32 bytes using repeated hashing
      let h1 = convert (hashWith SHA256 bs) :: ByteString
      in ByteString.take 32 h1

-- ============================================================================
-- Credential Encryption (AES-256-GCM)
-- ============================================================================

-- Wire format: IV(12 bytes) || AuthTag(16 bytes) || ciphertext
-- All base64-encoded together for storage as Text.

-- | Encrypt a credential using AES-256-GCM with a random 12-byte IV.
-- Output is base64-encoded: IV || AuthTag || ciphertext.
encryptCredential :: EncryptionContext -> Text -> IO Text
encryptCredential ctx plaintext = do
  iv <- getRandomBytes 12
  let plainBs = TE.encodeUtf8 plaintext
      cipher = encCipher ctx
  case aeadInit AEAD_GCM cipher iv of
    CryptoFailed _ -> error "Failed to initialize GCM AEAD context (should never happen)"
    CryptoPassed aeadCtx -> do
      let (authTag, ciphertext) = aeadSimpleEncrypt aeadCtx ("" :: ByteString) plainBs 16
          packed = iv <> convert authTag <> ciphertext
      pure $ TE.decodeUtf8 $ Base64.encode packed

-- | Decrypt a credential that was encrypted with 'encryptCredential'.
-- Returns Nothing if the data is corrupted or tampered with.
decryptCredential :: EncryptionContext -> Text -> IO (Maybe Text)
decryptCredential ctx encrypted =
  case Base64.decode (TE.encodeUtf8 encrypted) of
    Left _ -> pure Nothing
    Right packed
      -- Minimum size: IV(12) + AuthTag(16) = 28 bytes
      | ByteString.length packed < 28 -> pure Nothing
      | otherwise ->
          let iv = ByteString.take 12 packed
              rest = ByteString.drop 12 packed
              authTagBs = ByteString.take 16 rest
              ciphertext = ByteString.drop 16 rest
              authTag = AuthTag (convert authTagBs)
              cipher = encCipher ctx
          in case aeadInit AEAD_GCM cipher iv of
              CryptoFailed _ -> pure Nothing
              CryptoPassed aeadCtx ->
                case aeadSimpleDecrypt aeadCtx ("" :: ByteString) ciphertext authTag of
                  Nothing    -> pure Nothing
                  Just plain -> pure $ Just $ TE.decodeUtf8 plain

-- | Hash a credential using SHA-256 (one-way, cannot be reversed).
hashCredential :: Text -> Text
hashCredential text =
  let textBs = TE.encodeUtf8 text
      hash = hashWith SHA256 textBs
      encoded = Base64.encode $ convert hash
  in TE.decodeUtf8 encoded
