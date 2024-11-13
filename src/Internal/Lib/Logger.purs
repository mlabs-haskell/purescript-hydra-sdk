module HydraSdk.Internal.Lib.Logger
  ( log'
  ) where

import Prelude

import Control.Monad.Logger.Class (class MonadLogger, log)
import Data.JSDate (now)
import Data.Log.Level (LogLevel)
import Data.Log.Tag (TagSet)
import Effect.Class (liftEffect)

log' :: forall m. MonadLogger m => LogLevel -> TagSet -> String -> m Unit
log' level tags message =
  (log <<< { level, message, tags, timestamp: _ })
    =<< liftEffect now
