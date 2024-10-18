module HydraSdk.Internal.Types.Network
  ( Network(Testnet, Mainnet)
  , networkCodec
  , networkToNetworkId
  ) where

import Prelude

import Cardano.Types (NetworkId(TestnetId, MainnetId))
import Data.Codec.Argonaut (JsonCodec, int, object) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Variant (variantMatch) as CAV
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Profunctor (dimap)
import Data.Show.Generic (genericShow)
import Data.Variant (inj, match) as Variant
import HydraSdk.Internal.Lib.Codec (fixTaggedSumCodec)
import Type.Proxy (Proxy(Proxy))

data Network = Testnet { magic :: Int } | Mainnet

derive instance Generic Network _

instance Show Network where
  show = genericShow

networkCodec :: CA.JsonCodec Network
networkCodec =
  fixTaggedSumCodec $
    dimap toVariant fromVariant
      ( CAV.variantMatch
          { "testnet":
              Right $ CA.object "Testnet" $ CAR.record
                { magic: CA.int
                }
          , "mainnet": Left unit
          }
      )
  where
  toVariant = case _ of
    Testnet rec -> Variant.inj (Proxy :: _ "testnet") rec
    Mainnet -> Variant.inj (Proxy :: _ "mainnet") unit

  fromVariant = Variant.match
    { "testnet": Testnet
    , "mainnet": const Mainnet
    }

networkToNetworkId :: Network -> NetworkId
networkToNetworkId = case _ of
  Testnet _ -> TestnetId
  Mainnet -> MainnetId
