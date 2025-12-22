module HydraSdk.Example.Minimal.Config
  ( DelegateServerConfig
  , configFromArgv
  ) where

import Prelude

import Cardano.Types (TransactionInput)
import Data.Codec.Argonaut (JsonCodec, object, printJsonDecodeError, record, string) as CA
import Data.Codec.Argonaut.Compat (maybe) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (either)
import Data.Log.Level (LogLevel)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (throw)
import HydraSdk.Lib (caDecodeFile, logLevelCodec, orefCodec)
import HydraSdk.Process (HydraNodeStartupParams, hydraNodeStartupParamsCodec)
import Node.Path (FilePath)
import Node.Process (argv)

type DelegateServerConfig =
  { hydraNodeStartupParams :: HydraNodeStartupParams ()
  , blockfrostApiKeyFile :: FilePath
  , logLevel :: LogLevel
  , ctlLogLevel :: LogLevel
  , commitOutRef :: Maybe TransactionInput
  }

delegateServerConfigCodec :: CA.JsonCodec DelegateServerConfig
delegateServerConfigCodec =
  CA.object "DelegateServerConfig" $ CAR.record
    { hydraNodeStartupParams: hydraNodeStartupParamsCodec CA.record
    , blockfrostApiKeyFile: CA.string
    , logLevel: logLevelCodec
    , ctlLogLevel: logLevelCodec
    , commitOutRef: CA.maybe orefCodec
    }

configFromArgv :: Effect DelegateServerConfig
configFromArgv =
  argv >>= case _ of
    [ _, _, configFp ] ->
      either (throw <<< append "configFromArgv: " <<< CA.printJsonDecodeError) pure
        =<< caDecodeFile delegateServerConfigCodec configFp
    _ ->
      throw "configFromArgv: unexpected number of command-line arguments"
