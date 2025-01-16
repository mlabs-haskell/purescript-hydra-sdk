-- | This module provides convenience logging functionality.
module HydraSdk.Internal.Lib.Logger
  ( log'
  , logDebug
  , logError
  , logInfo
  , logTrace
  , logWarn
  ) where

import Prelude

import Control.Monad.Logger.Class (class MonadLogger, debug, error, info, log, trace, warn)
import Data.JSDate (now)
import Data.Log.Level (LogLevel)
import Data.Log.Tag (TagSet)
import Data.Map (empty) as Map
import Effect.Class (liftEffect)

-- | Logs a message with the specified log level and tag set.
-- |
-- | Taken without modifications from Control.Monad.Logger.Class, where
-- | this function is not exported.
log' :: forall m. MonadLogger m => LogLevel -> TagSet -> String -> m Unit
log' level tags message =
  (log <<< { level, message, tags, timestamp: _ })
    =<< liftEffect now

logTrace :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logTrace = trace Map.empty

logDebug :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logDebug = debug Map.empty

logInfo :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logInfo = info Map.empty

logWarn :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logWarn = warn Map.empty

logError :: forall (m :: Type -> Type). MonadLogger m => String -> m Unit
logError = error Map.empty
