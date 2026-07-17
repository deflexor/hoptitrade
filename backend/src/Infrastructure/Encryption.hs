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
import Crypto.Error (CryptoFailable (..))
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

newtype EncryptionContext = EncryptionContext
  { encCipher :: AES256
  }

-- | Initialize encryption from HOPTITRADE_ENCRYPTION_KEY.
-- Required unless HOPTITRADE_ENV=dev (then a random ephemeral key is allowed with warning).
initializeEncryption :: MonadIO m => m EncryptionContext
initializeEncryption = liftIO $ do
  mKey <- lookupEnv "HOPTITRADE_ENCRYPTION_KEY"
  mEnv <- lookupEnv "HOPTITRADE_ENV"
  keyBytes <- case mKey of
    Just key -> do
      let raw = TE.encodeUtf8 $ Text.pack key
      case Base64.decode raw of
        Right bs
          | ByteString.length bs >= 32 -> pure $ ByteString.take 32 bs
          | otherwise -> pure $ padKey raw
        Left _ -> pure $ padKey raw
    Nothing -> case mEnv of
      Just "dev" -> do
        putStrLn "WARNING: Generating random encryption key (credentials will not survive restart!)"
        putStrLn "Set HOPTITRADE_ENCRYPTION_KEY (32+ bytes, raw or base64) for persistent encryption."
        getRandomBytes 32
      _ -> do
        putStrLn "FATAL: HOPTITRADE_ENCRYPTION_KEY is required (or set HOPTITRADE_ENV=dev)."
        error "HOPTITRADE_ENCRYPTION_KEY must be set unless HOPTITRADE_ENV=dev"

  case cipherInit keyBytes :: CryptoFailable AES256 of
    CryptoFailed err -> do
      putStrLn $ "FATAL: Failed to initialize AES-256 cipher: " ++ show err
      error "Failed to initialize AES-256 cipher from HOPTITRADE_ENCRYPTION_KEY"
    CryptoPassed cipher -> do
      putStrLn "Encryption: AES-256-GCM initialized"
      pure $ EncryptionContext cipher
  where
    padKey bs =
      let h1 = convert (hashWith SHA256 bs) :: ByteString
      in ByteString.take 32 h1

-- ============================================================================
-- Credential Encryption (AES-256-GCM)
-- ============================================================================

-- Wire format: IV(12 bytes) || AuthTag(16 bytes) || ciphertext — base64-encoded.

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

decryptCredential :: EncryptionContext -> Text -> IO (Maybe Text)
decryptCredential ctx encrypted =
  case Base64.decode (TE.encodeUtf8 encrypted) of
    Left _ -> pure Nothing
    Right packed
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

hashCredential :: Text -> Text
hashCredential text =
  let textBs = TE.encodeUtf8 text
      hash = hashWith SHA256 textBs
      encoded = Base64.encode $ convert hash
  in TE.decodeUtf8 encoded
