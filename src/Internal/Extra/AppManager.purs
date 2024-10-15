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

type AppManagerSlot = Int
type ReservationCode = UUID

type ActiveApp appState appConfig =
  { state :: appState
  , config :: appConfig
  , occupiedSlot :: AppManagerSlot
  }

type ReservedSlot appConfig =
  { config :: appConfig
  , reservationCode :: ReservationCode
  , reservationMonitor :: Fiber Unit
  }

type AppManager appId appState appConfigAvailable appConfigActive =
  { activeApps :: Map appId (ActiveApp appState appConfigActive)
  , reservedSlots :: Map AppManagerSlot (ReservedSlot appConfigAvailable)
  , availableSlots :: Map AppManagerSlot appConfigAvailable
  }

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

getAvailableSlots
  :: forall m appId appState appConfigAvailable appConfigActive
   . MonadAff m
  => AVar (AppManager appId appState appConfigAvailable appConfigActive)
  -> m (Set AppManagerSlot)
getAvailableSlots appManagerAvar =
  withAppManager appManagerAvar \appManager ->
    pure $ Tuple appManager $ Map.keys appManager.availableSlots

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
