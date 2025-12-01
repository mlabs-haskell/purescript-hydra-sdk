module HydraSdk.Internal.Types.Tx
  ( HydraTx
  , hydraTxCodec
  , mkHydraTx
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Types (CborBytes, Transaction, TransactionHash)
import Cardano.Types.Transaction (hash) as Transaction
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import HydraSdk.Internal.Lib.Codec (cborBytesCodec, txHashCodec)

type HydraTx =
  { cborHex :: CborBytes
  , txId :: TransactionHash
  , description :: String
  , "type" :: String
  }

hydraTxCodec :: CA.JsonCodec HydraTx
hydraTxCodec =
  CA.object "HydraTx" $ CAR.record
    { cborHex: cborBytesCodec
    , txId: txHashCodec
    , description: CA.string
    , "type": CA.string
    }

mkHydraTx :: Transaction -> HydraTx
mkHydraTx tx =
  { cborHex: encodeCbor tx
  , txId: Transaction.hash tx
  , description: ""
  -- In hydra-node, the "type" field is not used to determine content
  -- and any transaction is tried to decode as a "ConwayEra"
  -- transaction, which mostly is backward compatible to
  -- previous eras.
  , "type": "Tx ConwayEra"
  }
