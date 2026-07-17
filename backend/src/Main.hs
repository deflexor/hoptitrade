{-# LANGUAGE TypeApplications #-}

module Main where

import App.PositionManager (defaultManagerConfig, startPositionManager)
import App.Server (initializeApp, defaultDbPath)
import Domain.Types (UserId(..))
import Effects.Position (recoverOpenPositions, runPositionWithPool)
import Network.Wai.Handler.Warp (run)
import Polysemy (runM)
import qualified Data.Text as Text
import System.Environment (setEnv, lookupEnv)

main :: IO ()
main = do
  -- Default to dev env for local runs unless explicitly set
  mEnv <- lookupEnv "HOPTITRADE_ENV"
  case mEnv of
    Nothing -> setEnv "HOPTITRADE_ENV" "dev"
    _ -> pure ()

  putStrLn "Starting HoptiTrade server..."
  putStrLn $ "Database: " ++ Text.unpack defaultDbPath

  (app, pool, encCtx) <- initializeApp

  putStrLn ""
  putStrLn "=== Position Recovery ==="
  let uid = UserId $ read "550e8400-e29b-41d4-a716-446655440000"
  _ <- runM @IO $ runPositionWithPool pool $ recoverOpenPositions uid
  putStrLn "========================"

  putStrLn "Starting position manager..."
  startPositionManager pool encCtx defaultManagerConfig

  putStrLn ""
  putStrLn "Server running on http://localhost:8080"
  run 8080 app
