module HydraSdk.Example.Minimal.App
  ( AppLogger
  , AppM
  , AppState
  , appLogger
  , initApp
  , readHeadStatus
  , runApp
  , runAppEff
  , runContractInApp
  , setHeadStatus
  , setUtxoSnapshot
  ) where

import Prelude

import Cardano.Types (TransactionInput, TransactionOutput, UtxoMap)
import Contract.Config
  ( ContractParams
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , QueryBackendParams
  , WalletSpec(UseKeys)
  , blockfrostPublicMainnetServerConfig
  , blockfrostPublicPreprodServerConfig
  , blockfrostPublicPreviewServerConfig
  , defaultConfirmTxDelay
  , defaultTimeParams
  , disabledSynchronizationParams
  , emptyHooks
  , mkBlockfrostBackendParams
  )
import Contract.Monad (Contract, ContractEnv, mkContractEnv, runContractInEnv)
import Contract.Utxos (getUtxo)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Logger.Trans (LoggerT, runLoggerT)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Ctl.Internal.ServerConfig (blockfrostPublicSanchonetServerConfig)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Message (Message)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.AVar (new, read) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throw)
import HydraSdk.Example.Minimal.Config (DelegateServerConfig)
import HydraSdk.Example.Minimal.Contract.PrepareUtxo (prepareUtxoContract)
import HydraSdk.Lib (modify) as AVar
import HydraSdk.Types
  ( HydraHeadStatus(HeadStatus_Unknown)
  , HydraSnapshot
  , Network(Mainnet, Testnet)
  , emptySnapshot
  , networkToNetworkId
  )

type AppM (a :: Type) = LoggerT (ReaderT AppState Aff) a

runApp :: forall a. AppState -> AppLogger -> AppM a -> Aff a
runApp appState logger =
  flip runReaderT appState
    <<< flip runLoggerT logger

runAppEff :: forall a. AppState -> AppLogger -> AppM a -> Effect Unit
runAppEff appState logger =
  void
    <<< launchAff
    <<< runApp appState logger

runContractInApp :: forall a. Contract a -> AppM a
runContractInApp contract =
  (liftAff <<< flip runContractInEnv contract)
    =<< asks _.contractEnv

type AppLogger = Message -> ReaderT AppState Aff Unit

appLogger :: AppLogger
appLogger message = do
  { config: { logLevel } } <- ask
  when (message.level >= logLevel) do
    messageFormatted <- prettyFormatter message
    liftEffect $ log messageFormatted

type AppState =
  { config :: DelegateServerConfig
  , contractEnv :: ContractEnv
  , commitUtxo :: Tuple TransactionInput TransactionOutput
  , headStatus :: AVar HydraHeadStatus
  , utxoSnapshot :: AVar HydraSnapshot
  }

readHeadStatus :: AppM HydraHeadStatus
readHeadStatus = (liftAff <<< AVar.read) =<< asks _.headStatus

setHeadStatus :: HydraHeadStatus -> AppM Unit
setHeadStatus status = (void <<< AVar.modify (const (pure status))) =<< asks _.headStatus

setUtxoSnapshot :: HydraSnapshot -> AppM Unit
setUtxoSnapshot snapshot =
  (void <<< AVar.modify (const (pure snapshot)))
    =<< asks _.utxoSnapshot

initApp :: DelegateServerConfig -> Aff AppState
initApp config@{ hydraNodeStartupParams: { network, cardanoSigningKey }, commitOutRef } = do
  backendParams <- liftEffect mkBackendParams
  contractEnv <- mkContractEnv $ contractParams backendParams
  commitUtxo <- runContractInEnv contractEnv do
    oref <- maybe prepareUtxoContract pure commitOutRef
    resolveCommitOutRef oref
  headStatus <- AVar.new HeadStatus_Unknown
  utxoSnapshot <- AVar.new emptySnapshot
  pure
    { config
    , contractEnv
    , commitUtxo
    , headStatus
    , utxoSnapshot
    }
  where
  resolveCommitOutRef
    :: TransactionInput
    -> Contract (Tuple TransactionInput TransactionOutput)
  resolveCommitOutRef oref =
    Tuple oref <$>
      ( liftMaybe (error "resolveCommitOutRef: could not resolve provided commit output ref")
          =<< getUtxo oref
      )

  mkBackendParams :: Effect QueryBackendParams
  mkBackendParams = do
    blockfrostConfig <-
      case network of
        Mainnet -> pure blockfrostPublicMainnetServerConfig
        Testnet { magic } ->
          case magic of
            1 -> pure blockfrostPublicPreprodServerConfig
            2 -> pure blockfrostPublicPreviewServerConfig
            4 -> pure blockfrostPublicSanchonetServerConfig
            _ ->
              throw $ "mkBackendParams: unsupported testnet network with magic: "
                <> show magic
    pure $ mkBlockfrostBackendParams
      { blockfrostConfig
      , blockfrostApiKey: config.blockfrostApiKey
      , confirmTxDelay: defaultConfirmTxDelay
      }

  contractParams :: QueryBackendParams -> ContractParams
  contractParams backendParams =
    { backendParams
    , networkId: networkToNetworkId network
    , logLevel: config.logLevel
    , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile cardanoSigningKey) Nothing Nothing
    , customLogger: Nothing
    , suppressLogs: false
    , hooks: emptyHooks
    , timeParams: defaultTimeParams
    , synchronizationParams: disabledSynchronizationParams
    }
