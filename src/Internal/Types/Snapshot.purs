module HydraSdk.Internal.Types.Snapshot
  ( HydraSnapshot(HydraSnapshot)
  , emptySnapshot
  , hydraSnapshotCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, int, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, wrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraSdk.Internal.Types.UtxoMap (HydraUtxoMap, hydraUtxoMapCodec)

newtype HydraSnapshot = HydraSnapshot
  { snapshotNumber :: Int
  , utxo :: HydraUtxoMap
  }

derive instance Generic HydraSnapshot _
derive instance Newtype HydraSnapshot _
derive instance Eq HydraSnapshot

instance Show HydraSnapshot where
  show = genericShow

emptySnapshot :: HydraSnapshot
emptySnapshot = wrap
  { snapshotNumber: zero
  , utxo: mempty
  }

hydraSnapshotCodec :: CA.JsonCodec HydraSnapshot
hydraSnapshotCodec =
  wrapIso HydraSnapshot $ CA.object "HydraSnapshot" $ CAR.record
    { snapshotNumber: CA.int
    , utxo: hydraUtxoMapCodec
    }
