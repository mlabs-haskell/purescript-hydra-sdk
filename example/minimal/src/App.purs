module HydraSdk.Example.Minimal.App
  ( AppLogger
  , AppM
  , AppState
  , appLogger
  , initApp
  , readHeadStatus
  , readUtxoSnapshot
  , runApp
  , runAppEff
  , runContractInApp
  , runL2ContractInApp
  , setHeadStatus
  , setUtxoSnapshot
  ) where

import Prelude

import Cardano.Types
  ( NetworkId(MainnetId, TestnetId)
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  )
import Cardano.Types.BigNum (one, zero) as BigNum
import Cardano.Types.Coin (zero) as Coin
import Contract.Config
  ( ContractParams
  , PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , ProviderBackendParams
  , WalletSpec(UseKeys)
  , blockfrostPublicMainnetServerConfig
  , blockfrostPublicPreprodServerConfig
  , blockfrostPublicPreviewServerConfig
  , defaultConfirmTxDelay
  , defaultTimeParams
  , disabledSynchronizationParams
  , emptyHooks
  , mkBlockfrostBackendParams
  , mkCtlBackendParams
  )
import Contract.Monad (Contract, ContractEnv, mkContractEnv, runContractInEnv)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Utxos (getUtxo)
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Logger.Trans (LoggerT, runLoggerT)
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Message (Message)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (modify, wrap)
import Data.String (take, trim) as String
import Data.Tuple (Tuple(Tuple))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (UInt)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (Aff, launchAff)
import Effect.Aff.AVar (new, read) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (error, throw)
import Effect.Ref (Ref)
import Effect.Ref (new) as Ref
import HydraSdk.Example.Minimal.Config
  ( DelegateServerConfig
  , DelegateServerQueryBackend(Blockfrost, Kupmios)
  )
import HydraSdk.Example.Minimal.Contract.Collateral (getCollateral)
import HydraSdk.Lib (modify) as AVar
import HydraSdk.Types (HydraHeadStatus(HeadStatus_Unknown), HydraSnapshot, emptySnapshot)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Sync (readTextFile)

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

runL2ContractInApp :: forall a. Contract a -> AppM a
runL2ContractInApp contract = do
  { contractEnv } <- ask
  liftAff $ runContractInEnv contractEnv do
    pparams <- getProtocolParameters <#> modify \rec ->
      rec
        { txFeeFixed = Coin.zero
        , txFeePerByte = (zero :: UInt)
        , prices = wrap
            { memPrice: wrap { numerator: BigNum.zero, denominator: BigNum.one }
            , stepPrice: wrap { numerator: BigNum.zero, denominator: BigNum.one }
            }
        , coinsPerUtxoByte = Coin.zero
        }
    contract # local _
      { ledgerConstants =
          contractEnv.ledgerConstants
            { pparams = pparams
            }
      }

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
  , depositTxId :: Ref (Maybe TransactionHash)
  }

readHeadStatus :: AppM HydraHeadStatus
readHeadStatus = (liftAff <<< AVar.read) =<< asks _.headStatus

setHeadStatus :: HydraHeadStatus -> AppM Unit
setHeadStatus status = (void <<< AVar.modify (const (pure status))) =<< asks _.headStatus

readUtxoSnapshot :: AppM HydraSnapshot
readUtxoSnapshot = (liftAff <<< AVar.read) =<< asks _.utxoSnapshot

setUtxoSnapshot :: HydraSnapshot -> AppM Unit
setUtxoSnapshot snapshot =
  (void <<< AVar.modify (const (pure snapshot)))
    =<< asks _.utxoSnapshot

initApp :: DelegateServerConfig -> Aff AppState
initApp config@{ hydraNodeStartupParams: { cardanoSigningKey }, commitOutRef } = do
  networkId /\ backendParams <- liftEffect mkBackendParams
  contractEnv <- mkContractEnv $ contractParams backendParams networkId
  commitUtxo <- runContractInEnv contractEnv do
    oref <- maybe getCollateral pure commitOutRef
    resolveCommitOutRef oref
  headStatus <- AVar.new HeadStatus_Unknown
  utxoSnapshot <- AVar.new emptySnapshot
  depositTxId <- liftEffect $ Ref.new Nothing
  pure
    { config
    , contractEnv
    , commitUtxo
    , headStatus
    , utxoSnapshot
    , depositTxId
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

  mkBackendParams :: Effect (NetworkId /\ ProviderBackendParams)
  mkBackendParams =
    case config.queryBackend of
      Blockfrost { apiKeyFile } -> do
        blockfrostApiKey <- String.trim <$> readTextFile UTF8 apiKeyFile
        let networkPrefix = String.take 7 blockfrostApiKey
        networkId /\ blockfrostConfig <-
          case networkPrefix of
            "mainnet" ->
              pure $ MainnetId /\ blockfrostPublicMainnetServerConfig
            "preprod" ->
              pure $ TestnetId /\ blockfrostPublicPreprodServerConfig
            "preview" ->
              pure $ TestnetId /\ blockfrostPublicPreviewServerConfig
            _ ->
              throw $ "mkBackendParams: unsupported network. Blockfrost API key prefix: "
                <> networkPrefix
        pure $ networkId /\ mkBlockfrostBackendParams
          { blockfrostConfig
          , blockfrostApiKey: Just blockfrostApiKey
          , confirmTxDelay: defaultConfirmTxDelay
          }
      Kupmios { network, kupoConfig, ogmiosConfig } ->
        pure $ network /\ mkCtlBackendParams
          { ogmiosConfig
          , kupoConfig
          }

  contractParams :: ProviderBackendParams -> NetworkId -> ContractParams
  contractParams backendParams networkId =
    { backendParams
    , networkId
    , logLevel: config.ctlLogLevel
    , walletSpec: Just $ UseKeys (PrivatePaymentKeyFile cardanoSigningKey) Nothing Nothing
    , customLogger: Nothing
    , suppressLogs: false
    , hooks: emptyHooks
    , timeParams: defaultTimeParams
    , synchronizationParams: disabledSynchronizationParams
    }
