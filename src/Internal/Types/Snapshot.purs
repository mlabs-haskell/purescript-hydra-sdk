module HydraSdk.Internal.Types.Snapshot
  ( ConfirmedSnapshot(InitialSnapshot, ConfirmedSnapshot)
  , HydraSnapshot(HydraSnapshot)
  , confirmedSnapshotCodec
  , emptySnapshot
  , hydraSnapshotCodec
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson, getField)
import Cardano.Types (TransactionHash)
import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, array, decode, encode, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import HydraSdk.Internal.Lib.Codec (aesonCodec, fromCaJsonDecodeError)
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
  { snapshotNumber :: Int
  , utxo :: HydraUtxoMap
  , confirmedTransactions :: Array TransactionHash
  }

derive instance Generic HydraSnapshot _
derive instance Newtype HydraSnapshot _
derive instance Eq HydraSnapshot

instance Show HydraSnapshot where
  show = genericShow

instance DecodeAeson HydraSnapshot where
  decodeAeson aeson = do
    obj <- decodeAeson aeson
    snapshotNumber <- getField obj "snapshotNumber" <|> getField obj "number"
    utxo <- (lmap fromCaJsonDecodeError <<< CA.decode hydraUtxoMapCodec) =<< getField obj
      "utxo"
    confirmedTransactions <- getField obj "confirmedTransactions" <|> getField obj "confirmed"
    pure $ wrap
      { snapshotNumber
      , utxo
      , confirmedTransactions
      }

instance EncodeAeson HydraSnapshot where
  encodeAeson (HydraSnapshot snapshot) = do
    encodeAeson
      { snapshotNumber: encodeAeson snapshot.snapshotNumber
      , utxo: CA.encode hydraUtxoMapCodec snapshot.utxo
      , confirmedTransactions: encodeAeson snapshot.confirmedTransactions
      }

hydraSnapshotCodec :: CA.JsonCodec HydraSnapshot
hydraSnapshotCodec = aesonCodec "HydraSnapshot"

emptySnapshot :: HydraSnapshot
emptySnapshot = wrap
  { snapshotNumber: zero
  , utxo: mempty
  , confirmedTransactions: mempty
  }

