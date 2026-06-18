module Test.HydraSdk.CardanoTestnet
  ( suite
  ) where

import Prelude

import Cardano.Types (NetworkId(TestnetId))
import Cardano.Types.BigNum (fromInt) as BigNum
import Contract.Log (logInfo')
import Contract.Monad (runContractInEnv)
import Contract.Test (ContractTest, InitialUTxOs, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Testnet (TestnetConfig)
import Control.Monad.Reader (ask)
import Data.Array (replicate) as Array
import Data.Array.NonEmpty (head) as NEArray
import Data.Log.Level (LogLevel(Trace, Warn))
import Data.Maybe (Maybe(Nothing))
import Data.Time.Duration (Seconds(Seconds))
import Effect.Aff.Class (liftAff)
import Effect.Aff.Retry (constantDelay, retrying)
import Effect.Class (liftEffect)
import Effect.Ref (Ref)
import Effect.Ref (read) as Ref
import HydraSdk.Example.Minimal.App (appLogger, initApp, readHeadStatus, runApp)
import HydraSdk.Example.Minimal.Config (DelegateServerQueryBackend(Kupmios))
import HydraSdk.Example.Minimal.Main (startDelegateServer)
import HydraSdk.Internal.Test.Cluster (withHydraCluster)
import HydraSdk.Test (defaultHydraClusterTimeParamsForCardanoTestnet)
import HydraSdk.Types (HydraHeadStatus(HeadStatus_Final))
import Mote (group)
import Mote.Monad (test)
import Node.Path (FilePath)

numHydraNodes :: Int
numHydraNodes = 2

defaultUtxoDistribution :: InitialUTxOs
defaultUtxoDistribution =
  [ BigNum.fromInt 2_000_000_000
  , BigNum.fromInt 2_000_000_000
  ]

suite :: Ref FilePath -> TestnetConfig -> TestPlanM ContractTest Unit
suite nodeSocketPathRef testnetConfig =
  group "cardano-testnet" do
    -- FIXME: this test is flaky because sometimes deposits expire before being approved
    -- we should try to adjust deposit and contestation periods to address this
    test "smoke" do
      withWallets (Array.replicate numHydraNodes defaultUtxoDistribution) \wallets -> do
        nodeSocketPath <- liftEffect $ Ref.read nodeSocketPathRef
        contractEnv <- ask
        liftAff $ withHydraCluster wallets
          { mkClusterSpec: \workdir ->
              { workdir
              , nodeSocketPath
              , testnetMagic: testnetConfig.clusterConfig.testnetMagic
              , protocolParametersPath: "protocol-parameters.json"
              , hydraNodeFirstPort: 7060
              , hydraNodeApiFirstPort: 7070
              , timeParams: defaultHydraClusterTimeParamsForCardanoTestnet
              }
          , mkPeerExtra: \_ _ -> {}
          , startHydraApp: \hydraNodeStartupParams idx -> do
              appState <- initApp
                { hydraNodeStartupParams
                , queryBackend:
                    Kupmios
                      { network: TestnetId
                      , kupoConfig: testnetConfig.kupoConfig
                      , ogmiosConfig: testnetConfig.ogmiosConfig
                      }
                , logLevel: Trace
                , ctlLogLevel: Warn
                , commitOutRef: Nothing
                , isLeader: idx == 0
                }
              startDelegateServer appState appLogger
          , runCleanupForHydraApp: liftEffect <<< _.cleanupHandler
          , action: \appHandles -> do
              -- TODO: select random app
              let
                app = NEArray.head appHandles
                action' app' = do
                  liftAff $ void $
                    retrying
                      (constantDelay $ Seconds 5.0)
                      (\_ done -> pure $ not done)
                      (\_ -> eq HeadStatus_Final <$> app'.getHeadStatus)
                  logInfo' "Success: Head finalized"
              runContractInEnv contractEnv $
                action'
                  { getHeadStatus: runApp app.appState app.appLogger readHeadStatus
                  }
          }
