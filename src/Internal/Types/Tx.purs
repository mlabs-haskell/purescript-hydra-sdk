module HydraSdk.Internal.Types.Tx
  ( HydraTx
  , hydraTxCodec
  , mkHydraTx
  ) where

import Prelude

import Contract.Transaction (Transaction)
import Data.Codec.Argonaut (JsonCodec, object, string) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import HydraSdk.Internal.Lib.Codec (txCodec)

type HydraTx =
  { cborHex :: Transaction
  , description :: String
  , "type" :: String
  }

hydraTxCodec :: CA.JsonCodec HydraTx
hydraTxCodec =
  CA.object "HydraTx" $ CAR.record
    { cborHex: txCodec
    , description: CA.string
    , "type": CA.string
    }

mkHydraTx :: Transaction -> HydraTx
mkHydraTx tx =
  { cborHex: tx
  , description: ""
  -- TODO: add parameter for tx type
  , "type": "Tx ConwayEra"
  }
