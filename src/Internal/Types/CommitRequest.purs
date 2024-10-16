module HydraSdk.Internal.Types.CommitRequest
  ( HydraCommitRequest(SimpleCommitRequest, FullCommitRequest)
  , HydraFullCommitRequest
  , hydraFullCommitRequestCodec
  , mkFullCommitRequest
  , mkSimpleCommitRequest
  ) where

import Prelude

import Contract.Transaction (Transaction)
import Contract.Utxos (UtxoMap)
import Data.Argonaut (class EncodeJson)
import Data.Codec.Argonaut (JsonCodec, encode, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import HydraSdk.Internal.Types.Tx (HydraTx, hydraTxCodec, mkHydraTx)
import HydraSdk.Internal.Types.UtxoMap (HydraUtxoMap, fromUtxoMap, hydraUtxoMapCodec)

data HydraCommitRequest
  = SimpleCommitRequest HydraUtxoMap
  | FullCommitRequest HydraFullCommitRequest

derive instance Generic HydraCommitRequest _
derive instance Eq HydraCommitRequest

instance Show HydraCommitRequest where
  show = genericShow

instance EncodeJson HydraCommitRequest where
  encodeJson =
    case _ of
      SimpleCommitRequest utxos ->
        CA.encode hydraUtxoMapCodec utxos
      FullCommitRequest rec ->
        CA.encode hydraFullCommitRequestCodec rec

mkSimpleCommitRequest :: UtxoMap -> HydraCommitRequest
mkSimpleCommitRequest = SimpleCommitRequest <<< fromUtxoMap

mkFullCommitRequest :: Transaction -> UtxoMap -> HydraCommitRequest
mkFullCommitRequest tx utxos =
  FullCommitRequest
    { blueprintTx: mkHydraTx tx
    , utxo: fromUtxoMap utxos
    }

type HydraFullCommitRequest =
  { blueprintTx :: HydraTx
  , utxo :: HydraUtxoMap
  }

hydraFullCommitRequestCodec :: CA.JsonCodec HydraFullCommitRequest
hydraFullCommitRequestCodec =
  CA.object "HydraFullCommitRequest" $ CAR.record
    { blueprintTx: hydraTxCodec
    , utxo: hydraUtxoMapCodec
    }
