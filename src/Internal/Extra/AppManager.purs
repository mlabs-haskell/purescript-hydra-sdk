-- | This module provides an experimental and opinionated interface
-- | for managing multiple Hydra applications.
module HydraSdk.Internal.Extra.AppManager
  ( ActiveApp
  , AppManager
  , AppManagerSlot
  , HostAppError(SlotNotAvailable, IncorrectReservationCode)
  , HostAppParams
  , ReservationCode
  , ReservedSlot
  , getAvailableSlots
  , hostApp
  , removeApp
  , reserveSlot
  , withAppManager
  ) where

import Prelude

import Control.Alt (alt)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(Left, Right), note)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (delete, insert, keys, lookup, pop) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Time.Duration (Seconds, fromDuration)
import Data.Tuple (Tuple(Tuple), snd)
import Data.UUID (UUID, genUUID)
import Effect.Aff (Aff, Fiber, delay, forkAff, generalBracket)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (put, take) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)

-- | The index of the slot within the `AppManager` that can be
-- | reserved / occupied to host a Hydra application with a properly
-- | configured Hydra Head.
type AppManagerSlot = Int

-- | A secret used to authenticate application hosting requests
-- | for a reserved slot.
type ReservationCode = UUID

-- | Represents an active application within the `AppManager`, i.e.,
-- | a hosted application with an established Hydra Head.
-- |
-- | `state`: The state of the running application. Typically a record
-- | with mutable, thread-safe variables used to track the current
-- | Hydra Head status, UTxO snapshot, and other relevant
-- | information.
-- |
-- | `config`: The configuration of the running application.
-- |
-- | `occupiedSlot`: The number of the slot this application occupies.
type ActiveApp appState appConfig =
  { state :: appState
  , config :: appConfig
  , occupiedSlot :: AppManagerSlot
  }

-- | Represents a reserved slot within the `AppManager`.
-- | 
-- | `config`: App configuration corresponding to the reserved slot.
-- | This configuration is used to spin up an application instance
-- | given the correct reservation code is provided. 
-- |
-- | `reservationCode`: A secret used to authenticate hosting requests
-- | for this reservation.
-- |
-- | `reservationMonitor`: Fiber of the assigned reservation monitor,
-- | which will remove the reservation and free the slot once
-- | the configured slot reservation period has expired.
type ReservedSlot appConfig =
  { config :: appConfig
  , reservationCode :: ReservationCode
  , reservationMonitor :: Fiber Unit
  }

-- | Hydra application manager. Tracks active application instances
-- | and slots for future hosting requests.
type AppManager appId appState appConfigAvailable appConfigActive =
  { activeApps :: Map appId (ActiveApp appState appConfigActive)
  , reservedSlots :: Map AppManagerSlot (ReservedSlot appConfigAvailable)
  , availableSlots :: Map AppManagerSlot appConfigAvailable
  }

-- | Applies an effectful function to the `AppManager` stored in
-- | an asynchronous variable (AVar) in a safe manner.
-- TODO: Consider using `HydraSdk.Internal.Lib.AVar.modify` instead?
withAppManager
  :: forall m appId appState appConfigAvailable appConfigActive a
   . MonadAff m
  => AVar (AppManager appId appState appConfigAvailable appConfigActive)
  -> ( AppManager appId appState appConfigAvailable appConfigActive
       -> Aff (Tuple (AppManager appId appState appConfigAvailable appConfigActive) a)
     )
  -> m a
withAppManager appManagerAvar =
  liftAff <<< map snd <<< generalBracket (AVar.take appManagerAvar)
    { killed: handleFailure
    , failed: handleFailure
    , completed:
        \(Tuple appManager' _) _ ->
          AVar.put appManager' appManagerAvar
    }
  where
  handleFailure
    :: Error
    -> AppManager appId appState appConfigAvailable appConfigActive
    -> Aff Unit
  handleFailure = const (flip AVar.put appManagerAvar)

-- | Retrieves the set of currently available slots for reservation.
getAvailableSlots
  :: forall m appId appState appConfigAvailable appConfigActive
   . MonadAff m
  => AVar (AppManager appId appState appConfigAvailable appConfigActive)
  -> m (Set AppManagerSlot)
getAvailableSlots appManagerAvar =
  withAppManager appManagerAvar \appManager ->
    pure $ Tuple appManager $ Map.keys appManager.availableSlots

-- | Attempts to reserve the specified slot.
-- |
-- | If successful, atomically modifies the provided AppManager AVar
-- | and returns the `ReservedSlot` record, which includes the
-- | corresponding app configuration, reservation secret, and a
-- | reservation monitor fiber that should be managed by the caller.
-- | For example, the caller is responsible for cleaning up the
-- | returned fiber during the exit cleanup procedure.
-- |
-- | Returns 'Nothing' if the slot does not exist or is currently
-- | unavailable.
reserveSlot
  :: forall m appId appState appConfigAvailable appConfigActive
   . MonadAff m
  => AVar (AppManager appId appState appConfigAvailable appConfigActive)
  -> Seconds
  -> AppManagerSlot
  -> (String -> Aff Unit)
  -> m (Maybe (ReservedSlot appConfigAvailable))
reserveSlot appManagerAvar slotReservationPeriod slot logger =
  withAppManager appManagerAvar \appManager ->
    case Map.pop slot appManager.availableSlots of
      Nothing -> pure $ Tuple appManager Nothing
      Just (Tuple config availableSlots) -> do
        logger $ "Added reservation for slot " <> show slot
        reservationMonitor <- forkReservationMonitor appManagerAvar slotReservationPeriod slot
          logger
        logger $
          "Reservation for slot "
            <> show slot
            <> " will be valid for the next "
            <> show (unwrap slotReservationPeriod)
            <> " seconds"
        reservationCode <- liftEffect genUUID
        let reservedSlot = { config, reservationCode, reservationMonitor }
        pure $ Tuple
          ( appManager
              { availableSlots = availableSlots
              , reservedSlots = Map.insert slot reservedSlot appManager.reservedSlots
              }
          )
          (Just reservedSlot)

-- | Removes the active application, frees up the associated slot,
-- | and deactivates the app configuration. This function should be
-- | called during Hydra Head finalization as part of the cleanup
-- | logic, the implementation of which is left to the SDK user.
-- | Note that this function is not inherently thread-safe and should
-- | be invoked within the `withAppManager` context.
removeApp
  :: forall m appId appState appConfigAvailable appConfigActive
   . Monad m
  => Ord appId
  => appId
  -> AppManager appId appState appConfigAvailable appConfigActive
  -> { deactivateConfig :: appConfigActive -> m appConfigAvailable
     }
  -> m (Maybe (AppManager appId appState appConfigAvailable appConfigActive))
removeApp id appManager { deactivateConfig } =
  case Map.pop id appManager.activeApps of
    Nothing -> pure Nothing
    Just (Tuple { config, occupiedSlot } activeApps) -> do
      config' <- deactivateConfig config
      pure $ Just $ appManager
        { activeApps = activeApps
        , availableSlots = Map.insert occupiedSlot config' appManager.availableSlots
        }

type HostAppParams m appId appState appConfigAvailable appConfigActive =
  { slot :: AppManagerSlot
  , reservationCode :: Maybe ReservationCode
  , appManager :: AppManager appId appState appConfigAvailable appConfigActive
  , startApp ::
      appConfigAvailable -> m { id :: appId, state :: appState, config :: appConfigActive }
  }

data HostAppError
  = SlotNotAvailable
  | IncorrectReservationCode

derive instance Generic HostAppError _
derive instance Eq HostAppError

instance Show HostAppError where
  show = genericShow

-- | Attempts to host an application at the specified slot.
-- | On success, invokes the provided startup procedure with the
-- | retrieved configuration.
-- | Returns an exception if the slot is unavailable or if the
-- | reservation code is incorrect.
-- | Note that this function is not inherently thread-safe and should
-- | be invoked within the `withAppManager` context.
hostApp
  :: forall m appId appState appConfigAvailable appConfigActive
   . Monad m
  => Ord appId
  => HostAppParams m appId appState appConfigAvailable appConfigActive
  -> m
       ( Either HostAppError
           (AppManager appId appState appConfigAvailable appConfigActive)
       )
hostApp { slot, reservationCode, appManager, startApp } =
  runExceptT do
    appConfigAvailable <- except $ getAppConfigForSlot appManager slot reservationCode
    { id, state, config } <- lift $ startApp appConfigAvailable
    pure
      { availableSlots: Map.delete slot appManager.availableSlots
      , reservedSlots: Map.delete slot appManager.reservedSlots
      , activeApps: Map.insert id { state, config, occupiedSlot: slot } appManager.activeApps
      }

-- Internal ----------------------------------------------------------

forkReservationMonitor
  :: forall m appId appState appConfigAvailable appConfigActive
   . MonadAff m
  => AVar (AppManager appId appState appConfigAvailable appConfigActive)
  -> Seconds
  -> AppManagerSlot
  -> (String -> Aff Unit)
  -> m (Fiber Unit)
forkReservationMonitor appManagerAvar slotReservationPeriod slot logger =
  liftAff $ forkAff do
    delay $ fromDuration slotReservationPeriod
    withAppManager appManagerAvar \appManager ->
      case Map.pop slot appManager.reservedSlots of
        Nothing -> pure $ Tuple appManager unit
        Just (Tuple { config } reservedSlots) -> do
          logger $ "Removed reservation for slot " <> show slot
          pure $ Tuple
            ( appManager
                { reservedSlots = reservedSlots
                , availableSlots = Map.insert slot config appManager.availableSlots
                }
            )
            unit

getAppConfigForSlot
  :: forall appId appState appConfigAvailable appConfigActive
   . AppManager appId appState appConfigAvailable appConfigActive
  -> AppManagerSlot
  -> Maybe ReservationCode
  -> Either HostAppError appConfigAvailable
getAppConfigForSlot appManager slot mReservationCode =
  alt
    ( maybe (Left SlotNotAvailable)
        ( \code ->
            case Map.lookup slot appManager.reservedSlots of
              Just { config, reservationCode } | reservationCode == code ->
                Right config
              Just _ -> Left IncorrectReservationCode
              Nothing -> Left SlotNotAvailable

        )
        mReservationCode
    )
    ( note SlotNotAvailable $
        Map.lookup slot appManager.availableSlots
    )
