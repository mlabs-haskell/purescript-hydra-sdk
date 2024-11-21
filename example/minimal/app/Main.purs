module HydraSdk.Example.Minimal.Main
  ( main
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Contract.CborBytes (cborBytesToHex)
import Contract.Log (logError', logInfo', logTrace', logWarn')
import Contract.Monad (ContractEnv, stopContractEnv)
import Contract.Transaction (submit)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (ask)
import Data.Argonaut (stringifyWithIndent)
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either(Left, Right))
import Data.Log.Level (LogLevel(Info, Error))
import Data.Map (empty, fromFoldable) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Data.Traversable (traverse_)
import Data.UInt (fromInt) as UInt
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, runAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, message)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import HydraSdk.Example.Minimal.App
  ( AppLogger
  , AppM
  , AppState
  , appLogger
  , initApp
  , readHeadStatus
  , runAppEff
  , runContractInApp
  )
import HydraSdk.Example.Minimal.App (setHeadStatus, setUtxoSnapshot) as App
import HydraSdk.Example.Minimal.Config (configFromArgv)
import HydraSdk.Lib (log')
import HydraSdk.NodeApi
  ( HydraNodeApiWebSocket
  , HydraTxRetryStrategy(RetryTxWithParams, DontRetryTx)
  , commitRequest
  , mkHydraNodeApiWebSocket
  )
import HydraSdk.Process (spawnHydraNode)
import HydraSdk.Types
  ( HydraHeadStatus
      ( HeadStatus_Idle
      , HeadStatus_Initializing
      , HeadStatus_Open
      , HeadStatus_Closed
      )
  , HydraNodeApi_InMessage(Greetings, HeadIsInitializing, HeadIsOpen)
  , HydraSnapshot(HydraSnapshot)
  , hydraSnapshotCodec
  , mkSimpleCommitRequest
  , printHeadStatus
  , printHost
  , printHostPort
  , toUtxoMap
  )
import Node.ChildProcess (ChildProcess, kill)
import Node.Process (onSignal, onUncaughtException)
import URI.Port (toInt) as Port

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
        runAppEff appState logger $ logError' $ "UNCAUGHT EXCEPTION: " <> message err
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
        Greetings { headStatus } -> do
          setHeadStatus headStatus
          when (headStatus == HeadStatus_Idle) $ liftEffect ws.initHead
        HeadIsInitializing _ -> do
          setHeadStatus HeadStatus_Initializing
          { commitUtxo, config: { hydraNodeStartupParams: { hydraNodeApiAddress } } } <- ask
          let
            payload = mkSimpleCommitRequest $ Map.fromFoldable [ commitUtxo ]
            serverConfig =
              { port: UInt.fromInt $ Port.toInt hydraNodeApiAddress.port
              , host: printHost hydraNodeApiAddress
              , secure: false
              , path: Nothing
              }
          liftAff (commitRequest serverConfig payload) >>= case _ of
            Left httpErr ->
              throwError $ error $ "Commit request failed with error: "
                <> show httpErr
            Right { cborHex: commitTx } -> do
              txHash <- runContractInApp $ submit commitTx
              logInfo' $ "Submitted Commit transaction: " <> cborBytesToHex
                (encodeCbor txHash)
        HeadIsOpen { headId, utxo } -> do
          setHeadStatus HeadStatus_Open
          logInfo' $ "Head ID: " <> cborBytesToHex (encodeCbor headId)
          setUtxoSnapshot $ HydraSnapshot
            { snapshotNumber: zero
            , utxo
            }
        _ -> pure unit

setHeadStatus :: HydraHeadStatus -> AppM Unit
setHeadStatus status = do
  App.setHeadStatus status
  logInfo' $ "New Head status: " <> printHeadStatus status

setUtxoSnapshot :: HydraSnapshot -> AppM Unit
setUtxoSnapshot snapshot = do
  App.setUtxoSnapshot snapshot
  let snapshotFormatted = stringifyWithIndent 2 $ CA.encode hydraSnapshotCodec snapshot
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
            <> message err
        Right _ ->
          logger Info "Successfully completed all cleanup actions -> exiting."
    )
    (stopContractEnv contractEnv)
