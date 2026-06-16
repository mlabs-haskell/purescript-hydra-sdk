module HydraSdk.Example.Minimal.Config
  ( DelegateServerConfig
  , DelegateServerQueryBackend(Blockfrost, Kupmios)
  , configFromArgv
  ) where

import Prelude

import Cardano.Provider (ServerConfig)
import Cardano.Types (NetworkId, TransactionInput)
import Data.Codec.Argonaut
  ( JsonCodec
  , boolean
  , int
  , object
  , printJsonDecodeError
  , prismaticCodec
  , record
  , string
  ) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Log.Level (LogLevel)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.UInt (fromInt', toInt) as UInt
import Effect (Effect)
import Effect.Exception (throw)
import HydraSdk.Lib (caDecodeFile, logLevelCodec, orefCodec)
import HydraSdk.Process (HydraNodeStartupParams, hydraNodeStartupParamsCodec)
import Node.Path (FilePath)
import Node.Process (argv)

type DelegateServerConfig =
  { hydraNodeStartupParams :: HydraNodeStartupParams ()
  , queryBackend :: DelegateServerQueryBackend
  , logLevel :: LogLevel
  , ctlLogLevel :: LogLevel
  , commitOutRef :: Maybe TransactionInput
  , isLeader :: Boolean
  }

delegateServerConfigCodec :: CA.JsonCodec DelegateServerConfig
delegateServerConfigCodec =
  CA.object "DelegateServerConfig" $ CAR.record
    { hydraNodeStartupParams: hydraNodeStartupParamsCodec CA.record
    , queryBackend: delegateServerQueryBackendCodec
    , logLevel: logLevelCodec
    , ctlLogLevel: logLevelCodec
    , commitOutRef: CA.maybe orefCodec
    , isLeader: CA.boolean
    }

data DelegateServerQueryBackend
  = Blockfrost { apiKeyFile :: FilePath }
  | Kupmios
      { network :: NetworkId
      , kupoConfig :: ServerConfig
      , ogmiosConfig :: ServerConfig
      }

derive instance Generic DelegateServerQueryBackend _
derive instance Eq DelegateServerQueryBackend

instance Show DelegateServerQueryBackend where
  show = genericShow

delegateServerQueryBackendCodec :: CA.JsonCodec DelegateServerQueryBackend
delegateServerQueryBackendCodec =
  CAS.sumFlat "DelegateServerQueryBackend"
    { "Blockfrost":
        CAR.record
          { apiKeyFile: CA.string
          }
    , "Kupmios":
        CAR.record
          { network: networkIdCodec
          , kupoConfig: serverConfigCodec
          , ogmiosConfig: serverConfigCodec
          }
    }

networkIdCodec :: CA.JsonCodec NetworkId
networkIdCodec = CAG.nullarySum "NetworkId"

serverConfigCodec :: CA.JsonCodec ServerConfig
serverConfigCodec =
  CA.object "ServerConfig" $ CAR.record
    { port: CA.prismaticCodec "UInt" UInt.fromInt' UInt.toInt CA.int
    , host: CA.string
    , secure: CA.boolean
    , path: CA.maybe CA.string
    }

configFromArgv :: Effect DelegateServerConfig
configFromArgv =
  argv >>= case _ of
    [ _, _, configFp ] ->
      either (throw <<< append "configFromArgv: " <<< CA.printJsonDecodeError) pure
        =<< caDecodeFile delegateServerConfigCodec configFp
    _ ->
      throw "configFromArgv: unexpected number of command-line arguments"
