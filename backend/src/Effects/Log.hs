{-# LANGUAGE TemplateHaskell #-}

module Effects.Log
  ( LogEffect (..)
  , logDebug
  , logInfo
  , logWarning
  , logError
  , runLogIO
  , runLogSilent
  ) where

import Data.Text (Text)
import Data.Time (getCurrentTime)
import Polysemy
import Polysemy.Embed

-- ============================================================================
-- Log Effect Definition
-- ============================================================================

data LogLevel
  = Debug
  | Info
  | Warning
  | Error
  deriving stock (Eq, Ord, Show)

data LogEffect m a where
  LogMessage :: LogLevel -> Text -> LogEffect m ()

makeSem ''LogEffect

-- ============================================================================
-- Convenience Functions
-- ============================================================================

logDebug :: Member LogEffect r => Text -> Sem r ()
logDebug = logMessage Debug

logInfo :: Member LogEffect r => Text -> Sem r ()
logInfo = logMessage Info

logWarning :: Member LogEffect r => Text -> Sem r ()
logWarning = logMessage Warning

logError :: Member LogEffect r => Text -> Sem r ()
logError = logMessage Error

-- ============================================================================
-- IO Interpreter (Production)
-- ============================================================================

runLogIO :: Members '[Embed IO] r => Sem (LogEffect ': r) a -> Sem r a
runLogIO = interpret $ \case
  LogMessage level msg -> embed $ do
    now <- getCurrentTime
    let levelStr = case level of
          Debug -> "[DEBUG]"
          Info -> "[INFO]"
          Warning -> "[WARN]"
          Error -> "[ERROR]"
    putStrLn $ show now ++ " " ++ levelStr ++ " " ++ show msg

-- ============================================================================
-- Silent Interpreter (Testing)
-- ============================================================================

runLogSilent :: Sem (LogEffect ': r) a -> Sem r a
runLogSilent = interpret $ \case
  LogMessage _ _ -> pure ()
