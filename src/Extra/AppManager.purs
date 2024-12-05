--| An opinionated interface for managing multiple Hydra application instances,
--| with each instance running a separate hydra-node process.
--| Please refer to README.md for more details.

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
