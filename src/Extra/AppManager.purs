module HydraSdk.Extra.AppManager
  ( module ExportAppManager
  ) where

import HydraSdk.Internal.Extra.AppManager
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
  ) as ExportAppManager
