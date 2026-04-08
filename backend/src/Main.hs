{-# LANGUAGE TypeApplications #-}

module Main where

import App.Server (initializeApp, defaultDbPath)
import Domain.Types (UserId(..))
import Effects.Position (recoverOpenPositions, runPositionWithPool)
import Network.Wai.Handler.Warp (run)
import Polysemy (runM)
import qualified Data.Text as Text

main :: IO ()
main = do
  putStrLn "Starting HoptiTrade server..."
  putStrLn $ "Database: " ++ Text.unpack defaultDbPath
  
  -- Initialize application with database and encryption
  (app, pool, _encCtx) <- initializeApp
  
  -- RECOVER OPEN POSITIONS ON STARTUP (per TODO.md requirement!)
  putStrLn ""
  putStrLn "=== Position Recovery ==="
  let uid = UserId $ read "550e8400-e29b-41d4-a716-446655440000"  -- TODO: Get from auth
  _ <- runM @IO $ runPositionWithPool pool $ recoverOpenPositions uid
  putStrLn "========================"
  
  putStrLn ""
  putStrLn "Server running on http://localhost:8080"
  putStrLn "API Endpoints:"
  putStrLn "  GET  /health"
  putStrLn "  POST /auth/login"
  putStrLn "  GET  /strategies"
  putStrLn "  GET  /positions"
  putStrLn "  POST /orders/open"
  putStrLn "  GET  /settings"
  putStrLn "  POST /settings/credentials/okx"
  putStrLn "  POST /settings/credentials/tbank"
  putStrLn "  GET  /settings/tbank/sandbox/accounts"
  putStrLn ""
  
  run 8080 app
