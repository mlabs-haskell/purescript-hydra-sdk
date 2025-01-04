module HydraSdk.Example.Minimal.Main
  ( main
  ) where

import Prelude

import Aeson (stringifyAeson)
import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Types (Language(PlutusV2), Transaction)
import Cardano.Types.AuxiliaryData (hashAuxiliaryData)
import Cardano.Types.Transaction (_body, _witnessSet)
import Cardano.Types.TransactionBody (_auxiliaryDataHash)
import Cardano.Types.TransactionWitnessSet (_vkeys)
import Contract.CborBytes (cborBytesToHex)
import Contract.Log (logError', logInfo', logTrace', logWarn')
import Contract.Monad (Contract, ContractEnv, stopContractEnv)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.Transaction (signTransaction, submit)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Ctl.Internal.Transaction (setScriptDataHash)
import Data.Array (length) as Array
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either(Left, Right))
import Data.Lens ((.~))
import Data.Log.Level (LogLevel(Info, Error))
import Data.Map (empty, filterKeys, fromFoldable) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap)
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, message, name, stack) as Error
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import HydraSdk.Example.Minimal.App
  ( AppLogger
  , AppM
  , AppState
  , appLogger
  , initApp
  , readHeadStatus
  , readUtxoSnapshot
  , runAppEff
  , runContractInApp
  )
import HydraSdk.Example.Minimal.App (setHeadStatus, setUtxoSnapshot) as App
import HydraSdk.Example.Minimal.Config (configFromArgv)
import HydraSdk.Example.Minimal.Contract.L2 (placeArbitraryDatumL2)
import HydraSdk.Lib (log')
import HydraSdk.NodeApi
  ( HydraNodeApiWebSocket
  , HydraTxRetryStrategy(RetryTxWithParams, DontRetryTx)
  , commitRequest
  , mkHydraNodeApiWebSocket
  )
import HydraSdk.Process (spawnHydraNode)
import HydraSdk.Types
  ( HydraHeadStatus(HeadStatus_Idle, HeadStatus_Closed)
  , HydraNodeApi_InMessage
      ( Greetings
      , HeadIsInitializing
      , HeadIsOpen
      , SnapshotConfirmed
      , HeadIsClosed
      , HeadIsContested
      , ReadyToFanout
      , HeadIsFinalized
      )
  , HydraSnapshot(HydraSnapshot)
  , hydraSnapshotCodec
  , mkSimpleCommitRequest
  , printHostPort
  , toUtxoMap
  )
import Node.ChildProcess (ChildProcess, kill)
import Node.Process (onSignal, onUncaughtException)

type AppHandle =
  { cleanupHandler :: Effect Unit
  , hydraNodeProcess :: ChildProcess
  }

main :: Effect Unit
main =
  launchAff_ do
    config <- liftEffect configFromArgv
    appState <- initApp config
    let logger = appLogger
    appHandle <- startDelegateServer appState logger
    liftEffect do
      onUncaughtException \err -> do
        runAppEff appState logger $ logError' $
          "UNCAUGHT "
            <> Error.name err
            <> ": "
            <> Error.message err
            <> maybe mempty (append ", STACK: ") (Error.stack err)
        appHandle.cleanupHandler
      onSignal SIGINT appHandle.cleanupHandler
      onSignal SIGTERM appHandle.cleanupHandler

startDelegateServer :: AppState -> AppLogger -> Aff AppHandle
startDelegateServer state logger = do
  hydraNodeApiWsRef <- liftEffect $ Ref.new Nothing
  hydraNodeProcess <- spawnHydraNode state.config.hydraNodeStartupParams
    { apiServerStartedHandler:
        Just $ appEff do
          let
            wsUrl = "ws://" <> printHostPort
              state.config.hydraNodeStartupParams.hydraNodeApiAddress
          hydraNodeApiWs <- mkHydraNodeApiWebSocket
            { url: wsUrl
            , runM: appEff
            , handlers:
                { connectHandler: const (pure unit)
                , messageHandler: \ws -> messageHandler ws
                , headStatusHandler: Just App.setHeadStatus
                , errorHandler: \_ws err ->
                    logError' $ "hydra-node API WebSocket error: " <> show err
                }
            , txRetryStrategies:
                { close:
                    RetryTxWithParams
                      { delaySec: 90
                      , maxRetries: top
                      , successPredicate: (_ >= HeadStatus_Closed) <$> readHeadStatus
                      , failHandler: pure unit
                      }
                , contest: DontRetryTx
                }
            }
          liftEffect $ Ref.write (Just hydraNodeApiWs) hydraNodeApiWsRef
    , stdoutHandler:
        Just (appEff <<< logTrace' <<< append "[hydra-node:stdout] ")
    , stderrHandler:
        Just (appEff <<< logWarn' <<< append "[hydra-node:stderr] ")
    }
  pure
    { cleanupHandler: cleanupHandler (\logLevel -> appEff <<< log' logLevel Map.empty)
        { hydraNodeProcess
        , hydraNodeApiWsRef
        , contractEnv: state.contractEnv
        }
    , hydraNodeProcess
    }
  where
  appEff :: forall a. AppM a -> Effect Unit
  appEff = runAppEff state logger

messageHandler
  :: HydraNodeApiWebSocket AppM
  -> Either String HydraNodeApi_InMessage
  -> AppM Unit
messageHandler ws =
  case _ of
    Left _rawMessage -> pure unit
    Right message ->
      case message of
        Greetings { headStatus } ->
          when (headStatus == HeadStatus_Idle) $
            liftEffect ws.initHead
        HeadIsInitializing _ -> do
          { commitUtxo, config: { hydraNodeStartupParams: { hydraNodeApiAddress } } } <- ask
          let
            payload = mkSimpleCommitRequest $ Map.fromFoldable [ commitUtxo ]
            hydraNodeHttpUrl = "http://" <> printHostPort hydraNodeApiAddress
          liftAff (commitRequest hydraNodeHttpUrl payload) >>= case _ of
            Left httpErr ->
              throwError $ Error.error $ "Commit request failed with error: "
                <> show httpErr
            Right { cborHex } -> do
              case decodeCbor cborHex of
                Just commitTx -> do
                  txHash <- runContractInApp $ submit =<< fixCommitTx commitTx
                  logInfo' $ "Submitted Commit transaction: " <> cborBytesToHex
                    (encodeCbor txHash)
                Nothing ->
                  throwError $ Error.error "Could not decode CommitTx CBOR"
        HeadIsOpen { headId, utxo } -> do
          logInfo' $ "Head ID: " <> cborBytesToHex (encodeCbor headId)
          setUtxoSnapshot $ HydraSnapshot
            { snapshotNumber: zero
            , utxo
            , confirmedTransactions: mempty
            }
          tx <- runContractInApp $ placeArbitraryDatumL2 $ toUtxoMap utxo
          liftEffect $ ws.submitTxL2 tx
        SnapshotConfirmed { snapshot } -> do
          setUtxoSnapshot snapshot
          { config: { hydraNodeStartupParams: { peers } } } <- ask
          when ((unwrap snapshot).snapshotNumber > Array.length peers) do
            logInfo' "All Head participants must have advanced the L2 state. Closing Head..."
            liftEffect ws.closeHead
        HeadIsClosed { snapshotNumber } ->
          contestClosureIfNeeded ws snapshotNumber
        HeadIsContested { snapshotNumber } ->
          contestClosureIfNeeded ws snapshotNumber
        ReadyToFanout _ ->
          liftEffect ws.fanout
        HeadIsFinalized _ ->
          -- TODO: output fanout tx hash
          throwError $ Error.error
            "SUCCESS: Head finalized, Funds transfered to L1 - Exiting..."
        _ -> pure unit

fixCommitTx :: Transaction -> Contract Transaction
fixCommitTx = reSignTransaction <=< fixScriptIntegrityHash <<< setAuxDataHash
  where
  -- | Computes and sets the transaction auxiliary data hash.
  setAuxDataHash :: Transaction -> Transaction
  setAuxDataHash tx =
    tx # _body <<< _auxiliaryDataHash .~
      (hashAuxiliaryData <$> (unwrap tx).auxiliaryData)

  -- | Removes existing vkey witnesses and signs the transaction.
  reSignTransaction :: Transaction -> Contract Transaction
  reSignTransaction tx = signTransaction (tx # _witnessSet <<< _vkeys .~ mempty)

  fixScriptIntegrityHash :: Transaction -> Contract Transaction
  fixScriptIntegrityHash tx = do
    pparams <- unwrap <$> getProtocolParameters
    let
      costModels = Map.filterKeys (eq PlutusV2) pparams.costModels
      ws = unwrap (unwrap tx).witnessSet
    liftEffect $ setScriptDataHash costModels ws.redeemers ws.plutusData tx

contestClosureIfNeeded :: HydraNodeApiWebSocket AppM -> Int -> AppM Unit
contestClosureIfNeeded ws closeSnapshot = do
  HydraSnapshot { snapshotNumber: localSnapshot } <- readUtxoSnapshot
  when (closeSnapshot < localSnapshot) do
    logInfo' $
      "Detected attempt to close the Head with older snapshot. Close snapshot: "
        <> show closeSnapshot
        <> ", local snapshot: "
        <> show localSnapshot
        <> ". Contesting Head closure..."
    liftEffect ws.challengeSnapshot

setUtxoSnapshot :: HydraSnapshot -> AppM Unit
setUtxoSnapshot snapshot = do
  App.setUtxoSnapshot snapshot
  let snapshotFormatted = stringifyAeson $ CA.encode hydraSnapshotCodec snapshot
  logInfo' $ "New confirmed snapshot: " <> snapshotFormatted

cleanupHandler
  :: forall (m :: Type -> Type)
   . (LogLevel -> String -> Effect Unit)
  -> { hydraNodeProcess :: ChildProcess
     , hydraNodeApiWsRef :: Ref (Maybe (HydraNodeApiWebSocket m))
     , contractEnv :: ContractEnv
     }
  -> Effect Unit
cleanupHandler logger { hydraNodeProcess, hydraNodeApiWsRef, contractEnv } = do
  logger Info "Killing hydra-node."
  kill SIGINT hydraNodeProcess
  logger Info "Closing hydra-node API WebSocket connection."
  Ref.read hydraNodeApiWsRef >>= traverse_ _.baseWs.close
  logger Info "Finalizing CTL Contract environment."
  runAff_
    ( case _ of
        Left err ->
          logger Error $ "stopContractEnv failed with error: "
            <> Error.message err
        Right _ ->
          logger Info "Successfully completed all cleanup actions -> exiting."
    )
    (stopContractEnv contractEnv)
