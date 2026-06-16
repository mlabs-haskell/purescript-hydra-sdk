module Test.HydraSdk.CardanoTestnet
  ( suite
  ) where

import Prelude

import Cardano.Types (NetworkId(TestnetId))
import Cardano.Types.BigNum (fromInt) as BigNum
import Cardano.Wallet.Key (KeyWallet, getPrivatePaymentKey)
import Contract.Log (logInfo')
import Contract.Monad (Contract, runContractInEnv)
import Contract.Test (ContractTest, InitialUTxOs, withWallets)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Testnet (TestnetConfig)
import Control.Monad.Error.Class (catchError, liftMaybe, throwError)
import Control.Monad.Reader (ask)
import Control.Parallel (parTraverse, parTraverse_)
import Ctl.Internal.Helpers (concatPaths)
import Ctl.Internal.Testnet.Utils (tmpdir)
import Data.Array (length, range, replicate, zip) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray, head) as NEArray
import Data.Log.Level (LogLevel(Trace, Warn))
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap)
import Data.Time.Duration (Seconds(Seconds))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.UUID (genUUID, toString) as UUID
import Effect (Effect)
import Effect.Aff (Aff, bracket)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Retry (constantDelay, retrying)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error)
import Effect.Ref (Ref)
import Effect.Ref (read) as Ref
import HydraSdk.Example.Minimal.App (appLogger, initApp, readHeadStatus, runApp)
import HydraSdk.Example.Minimal.Config (DelegateServerQueryBackend(Kupmios))
import HydraSdk.Example.Minimal.Main (AppHandle, startDelegateServer)
import HydraSdk.Test
  ( defaultHydraClusterTimeParamsForCardanoTestnet
  , genHydraNodeConfigurations
  )
import HydraSdk.Types (HydraHeadStatus(HeadStatus_Final))
import Mote (group)
import Mote.Monad (test)
import Node.FS.Aff (mkdir)
import Node.FS.Sync (rm') as FSSync
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
      withWallets (Array.replicate numHydraNodes defaultUtxoDistribution) \wallets ->
        withHydraCluster nodeSocketPathRef testnetConfig wallets \app -> do
          liftAff $ void $
            retrying
              (constantDelay $ Seconds 5.0)
              (\_ done -> pure $ not done)
              (\_ -> eq HeadStatus_Final <$> app.getHeadStatus)
          logInfo' "Success: Head finalized"

type AppInterface =
  { getHeadStatus :: Aff HydraHeadStatus
  }

withHydraCluster
  :: Ref FilePath
  -> TestnetConfig
  -> Array KeyWallet
  -> (AppInterface -> Contract Unit)
  -> Contract Unit
withHydraCluster nodeSocketPathRef testnetConfig kws action = do
  contractEnv <- ask
  liftAff $ bracket
    ( startHydraCluster nodeSocketPathRef testnetConfig kws `catchError` \err -> do
        liftEffect $ log $ "withHydraCluster: uncaught error: " <> show err
        throwError err
    )
    ( \{ appHandles, workdirCleanupHandler } -> do
        parTraverse_ (liftEffect <<< _.cleanupHandler) appHandles
        liftEffect workdirCleanupHandler
    )
    ( \{ appHandles } -> do
        -- TODO: select random app
        let app = NEArray.head appHandles
        runContractInEnv contractEnv $
          action
            { getHeadStatus: runApp app.appState app.appLogger readHeadStatus
            }
    )

startHydraCluster
  :: Ref FilePath
  -> TestnetConfig
  -> Array KeyWallet
  -> Aff
       { appHandles :: NonEmptyArray AppHandle
       , workdirCleanupHandler :: Effect Unit
       }
startHydraCluster nodeSocketPathRef testnetConfig kws = do
  nodeSocketPath <- liftEffect $ Ref.read nodeSocketPathRef
  privateKeys <- map unwrap <$> traverse getPrivatePaymentKey kws
  clusterId <- liftEffect $ UUID.toString <$> UUID.genUUID
  workdir <- liftEffect $ tmpdir <#> flip concatPaths clusterId
  mkdir workdir
  let
    clusterSpec =
      { workdir
      , nodeSocketPath
      , testnetMagic: testnetConfig.clusterConfig.testnetMagic
      , protocolParametersPath: "protocol-parameters.json"
      , hydraNodeFirstPort: 7060
      , hydraNodeApiFirstPort: 7070
      , timeParams: defaultHydraClusterTimeParamsForCardanoTestnet
      }
  peerSpecs <-
    liftMaybe (error "startHydraCluster: no wallets provided")
      ( NEArray.fromArray $ privateKeys <#> \cardanoSk ->
          { cardanoSk
          , extra: {}
          }
      )
  startupParamsList <- liftEffect $ genHydraNodeConfigurations clusterSpec peerSpecs
  appHandles' <- parTraverse
    ( \(hydraNodeStartupParams /\ idx) -> do
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
    )
    (Array.zip startupParamsList $ Array.range 0 (Array.length startupParamsList - 1))
  appHandles <-
    liftMaybe (error "startHydraCluster: appHandles array is empty") $
      NEArray.fromArray appHandles'
  let
    workdirCleanupHandler = do
      log $ "Cleaning up Hydra cluster workdir: " <> workdir
      FSSync.rm' workdir
        { force: true
        , maxRetries: zero
        , recursive: true
        , retryDelay: zero
        }
  pure
    { appHandles
    , workdirCleanupHandler
    }
