{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Broker.Bybit.Auth
  ( bybitMainnetUrl
  , bybitTestnetUrl
  , signRequest
  , recvWindow
  , currentTimestampMs
  ) where

import Crypto.Hash (SHA256 (..))
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.ByteArray (convert)
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)

bybitMainnetUrl :: Text
bybitMainnetUrl = "https://api.bybit.com"

bybitTestnetUrl :: Text
bybitTestnetUrl = "https://api-testnet.bybit.com"

recvWindow :: Text
recvWindow = "5000"

-- | Bybit V5 HMAC: timestamp + apiKey + recvWindow + (queryString | body)
signRequest :: Text -> Text -> Text -> Text -> Text
signRequest apiSecret timestamp apiKey payload =
  let raw = encodeUtf8 $ timestamp <> apiKey <> recvWindow <> payload
      mac = hmac (encodeUtf8 apiSecret) raw :: HMAC SHA256
  in decodeUtf8 $ BA.convertToBase BA.Base16 (convert mac :: BS.ByteString)

currentTimestampMs :: IO Text
currentTimestampMs = do
  now <- getPOSIXTime
  pure $ Text.pack $ show (floor (now * 1000) :: Integer)
