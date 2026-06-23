module Test.HydraSdk.Main
  ( main
  ) where

import Prelude

import Contract.Config (emptyHooks)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Testnet (TestnetConfig, testTestnetContracts)
import Contract.Test.Utils (interruptOnSignal)
import Data.Log.Level (LogLevel(Trace))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Data.Time.Duration (Minutes(Minutes), Seconds(Seconds), fromDuration)
import Data.UInt (fromInt) as UInt
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Ref (new, write) as Ref
import Mote (group)
import Test.HydraSdk.CardanoTestnet (suite) as CardanoTestnet
import Test.Spec.Runner (Config, defaultConfig)

main :: Effect Unit
main = do
  fiber <- launchAff $ interpretWithConfig runnerConfig suite
  interruptOnSignal SIGINT fiber
  interruptOnSignal SIGTERM fiber
  where
  runnerConfig :: Config
  runnerConfig =
    defaultConfig
      { timeout = Just (fromDuration $ Minutes 5.0)
      }

  suite :: TestPlanM (Aff Unit) Unit
  suite = do
    clusterParamsRef <- liftEffect $ Ref.new mempty
    let
      config =
        localnetConfig
          { hooks = localnetConfig.hooks
              { onClusterStartup = Just
                  ( flip Ref.write clusterParamsRef
                      <<< _.nodeSocketPath
                  )
              }
          }
    group "hydra-sdk" do
      testTestnetContracts config do
        CardanoTestnet.suite clusterParamsRef config

localnetConfig :: TestnetConfig
localnetConfig =
  { logLevel: Trace
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , suppressLogs: false
  , customLogger: Nothing
  , hooks: emptyHooks
  , clusterConfig:
      { testnetMagic: 2
      , slotLength: Seconds 0.1
      , epochSize: Just $ UInt.fromInt 4320000
      }
  }
