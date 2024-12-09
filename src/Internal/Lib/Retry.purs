module HydraSdk.Internal.Lib.Retry
  ( RetryParams
  , retry
  ) where

import Prelude

import Contract.Log (logWarn')
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Rec.Class (class MonadRec, Step(Loop, Done), tailRecM)
import Data.Int (toNumber)
import Data.Time.Duration (Seconds(Seconds), fromDuration)
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested ((/\))
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)

-- TODO: Consider using https://github.com/Unisay/purescript-aff-retry
-- in case a more advanced retry mechanism is required.
type RetryParams (m :: Type -> Type) (a :: Type) =
  { actionName :: String
  -- ^ name of the action for logging purposes
  , action :: m a
  -- ^ action to retry
  , delaySec :: Int
  -- ^ delay until the next retry
  , maxRetries :: Int
  -- ^ maximum number of retries before executing the failHandler
  , successPredicate :: (a -> m Boolean)
  -- ^ predicate to determine if the action was successful, retry if false
  , failHandler :: (a -> m a)
  -- ^ action to perform if maxRetries is exceeded 
  }

retry :: forall m a. MonadAff m => MonadRec m => MonadLogger m => RetryParams m a -> m a
retry p = do
  initRes <- p.action
  (p.maxRetries /\ initRes) # tailRecM \(i /\ res) -> do
    liftAff $ delay $ fromDuration $ Seconds $ toNumber p.delaySec
    success <- p.successPredicate res
    case success of
      true ->
        pure $ Done res
      false | i <= zero -> do
        logWarn' $ "maxRetries for action " <> p.actionName <>
          " exceeded, executing failHandler."
        Done <$> p.failHandler res
      false -> do
        logWarn' $ "successPredicate for action " <> p.actionName
          <> " returned false, retrying the action in "
          <> show p.delaySec
          <> " seconds."
        Loop <<< Tuple (i - 1) <$> p.action
