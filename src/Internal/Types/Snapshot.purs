module HydraSdk.Internal.Types.Snapshot
  ( ConfirmedSnapshot(InitialSnapshot, ConfirmedSnapshot)
  , HydraSnapshot(HydraSnapshot)
  , confirmedSnapshotCodec
  , emptySnapshot
  , hydraSnapshotCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, array, int, object, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CACompat
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (class Newtype, wrap)
import Data.Profunctor (wrapIso)
import Data.Show.Generic (genericShow)
import HydraSdk.Internal.Types.Tx (HydraTx, hydraTxCodec)
import HydraSdk.Internal.Types.UtxoMap (HydraUtxoMap, hydraUtxoMapCodec)

data ConfirmedSnapshot
  = InitialSnapshot
      { headId :: String
      , initialUTxO :: HydraUtxoMap
      }
  | ConfirmedSnapshot
      { snapshot :: HydraSnapshot
      , signatures :: { multiSignature :: Array String }
      }

derive instance Generic ConfirmedSnapshot _
derive instance Eq ConfirmedSnapshot

instance Show ConfirmedSnapshot where
  show = genericShow

confirmedSnapshotCodec :: CA.JsonCodec ConfirmedSnapshot
confirmedSnapshotCodec =
  CAS.sumFlat "ConfirmedSnapshot"
    { "InitialSnapshot":
        CAR.record
          { headId: CA.string
          , initialUTxO: hydraUtxoMapCodec
          }
    , "ConfirmedSnapshot":
        CAR.record
          { snapshot: hydraSnapshotCodec
          , signatures:
              CA.object "ConfirmedSnapshot:signatures" $ CAR.record
                { multiSignature: CA.array CA.string
                }
          }
    }

newtype HydraSnapshot = HydraSnapshot
  { number :: Int
  , utxo :: HydraUtxoMap
  , confirmed :: Array HydraTx
  , utxoToCommit :: Maybe HydraUtxoMap
  , utxoToDecommit :: Maybe HydraUtxoMap
  }

derive instance Generic HydraSnapshot _
derive instance Newtype HydraSnapshot _
derive instance Eq HydraSnapshot

instance Show HydraSnapshot where
  show = genericShow

{-
instance DecodeAeson HydraSnapshot where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    snapshotNumber <- getField obj "snapshotNumber" <|> getField obj "number"
    utxo <- (lmap fromCaJsonDecodeError <<< CA.decode hydraUtxoMapCodec) =<< getField obj
      "utxo"
    confirmed <-
      (lmap fromCaJsonDecodeError <<< CA.decode (CA.array hydraTxCodec))
        =<< getField obj "confirmed"
    pure $ wrap
      { snapshotNumber
      , utxo
      , confirmed
      }

instance EncodeAeson HydraSnapshot where
  encodeAeson (HydraSnapshot snapshot) = do
    encodeAeson
      { snapshotNumber: encodeAeson snapshot.snapshotNumber
      , utxo: CA.encode hydraUtxoMapCodec snapshot.utxo
      , confirmed: CA.encode (CA.array hydraTxCodec) snapshot.confirmed
      }
-}

hydraSnapshotCodec :: CA.JsonCodec HydraSnapshot
hydraSnapshotCodec = --aesonCodec "HydraSnapshot"

  wrapIso HydraSnapshot $ CA.object "HydraSnapshot" $ CAR.record
    { number: CA.int
    , utxo: hydraUtxoMapCodec
    , confirmed: CA.array hydraTxCodec
    , utxoToCommit: CACompat.maybe hydraUtxoMapCodec
    , utxoToDecommit: CACompat.maybe hydraUtxoMapCodec
    }

emptySnapshot :: HydraSnapshot
emptySnapshot = wrap
  { number: zero
  , utxo: mempty
  , confirmed: mempty
  , utxoToCommit: Nothing
  , utxoToDecommit: Nothing
  }

