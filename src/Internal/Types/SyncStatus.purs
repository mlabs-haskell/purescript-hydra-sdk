module HydraSdk.Internal.Types.SyncStatus
  ( SyncStatus(InSync, CatchingUp)
  , syncStatusCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data SyncStatus = InSync | CatchingUp

derive instance Generic SyncStatus _
derive instance Eq SyncStatus

instance Show SyncStatus where
  show = genericShow

syncStatusCodec :: CA.JsonCodec SyncStatus
syncStatusCodec = CAG.nullarySum "SyncStatus"
