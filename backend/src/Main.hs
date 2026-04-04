module Main where

import App.Server (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  putStrLn "Starting HoptiTrade server on http://localhost:8080"
  putStrLn "API Endpoints:"
  putStrLn "  GET  /health"
  putStrLn "  POST /auth/login"
  putStrLn "  GET  /strategies"
  putStrLn "  GET  /positions"
  putStrLn "  POST /orders/open"
  putStrLn "  GET  /settings"
  run 8080 app
