module HydraSdk.Internal.Lib.AVar
  ( modify
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, catchError, throwError)
import Effect.AVar (AVar)
import Effect.Aff.AVar (put, take) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)

modify
  :: forall (m :: Type -> Type) (e :: Type) (a :: Type)
   . MonadAff m
  => MonadError e m
  => (a -> m a)
  -> AVar a
  -> m a
modify f avar = do
  prev <- liftAff $ AVar.take avar
  new <- catchError (f prev) \err -> liftAff (AVar.put prev avar) *> throwError err
  liftAff $ AVar.put new avar
  pure new
