module HydraSdk.Internal.Types.QueryLayer
  ( QueryLayer(CardanoNode, Blockfrost)
  , queryLayerCodec
  ) where

import Prelude

import Data.Codec.Argonaut (JsonCodec, int, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CAC
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import HydraSdk.Internal.Types.Network (Network, networkCodec)
import Node.Path (FilePath)

data QueryLayer
  = CardanoNode
      { network :: Network
      , nodeSocket :: FilePath
      }
  | Blockfrost
      { apiKeyFile :: FilePath
      , queryTimeoutSec :: Maybe Int
      , retryTimeoutSec :: Maybe Int
      }

derive instance Generic QueryLayer _

instance Show QueryLayer where
  show = genericShow

queryLayerCodec :: CA.JsonCodec QueryLayer
queryLayerCodec =
  CAS.sumFlat "QueryLayer"
    { "CardanoNode": CAR.record
        { network: networkCodec
        , nodeSocket: CA.string
        }
    , "Blockfrost": CAR.record
        { apiKeyFile: CA.string
        , queryTimeoutSec: CAC.maybe CA.int
        , retryTimeoutSec: CAC.maybe CA.int
        }
    }
