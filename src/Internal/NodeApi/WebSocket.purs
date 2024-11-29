-- | This module provides an interface for establishing a connection
-- | and interacting with the hydra-node WebSocket API.
module HydraSdk.Internal.NodeApi.WebSocket
  ( HydraNodeApiHandlers
  , HydraNodeApiWebSocket
  , HydraNodeApiWebSocketBuilder
  , HydraTxRetryStrategy(RetryTxWithParams, DontRetryTx)
  , defaultCloseHeadSuccessPredicate
  , mkHydraNodeApiWebSocket
  ) where

import Prelude

import Cardano.Types (Transaction)
import Contract.Log (logInfo', logTrace')
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import HydraSdk.Internal.Lib.Retry (retry)
import HydraSdk.Internal.Lib.WebSocket (Url, WebSocket, mkWebSocket)
import HydraSdk.Internal.Types.HeadStatus (HydraHeadStatus(HeadStatus_Closed))
import HydraSdk.Internal.Types.NodeApiMessage
  ( HydraNodeApi_InMessage
  , HydraNodeApi_OutMessage(Init, Abort, NewTx, Close, Contest, Fanout)
  , hydraNodeApiInMessageCodec
  , hydraNodeApiOutMessageCodec
  )
import HydraSdk.Internal.Types.Tx (mkHydraTx)

-- | A record with operations that can be executed by the client to interact
-- | with the hydra-node WebSocket API.
type HydraNodeApiWebSocket (m :: Type -> Type) =
  { baseWs :: WebSocket m HydraNodeApi_InMessage HydraNodeApi_OutMessage
  , initHead :: Effect Unit
  , abortHead :: Effect Unit
  , submitTxL2 :: Transaction -> Effect Unit
  , closeHead :: Effect Unit
  , challengeSnapshot :: Effect Unit
  , fanout :: Effect Unit
  }

-- | Handlers to attach to the hydra-node API WebSocket connection.
type HydraNodeApiHandlers (m :: Type -> Type) =
  { connectHandler :: HydraNodeApiWebSocket m -> m Unit
  , errorHandler :: HydraNodeApiWebSocket m -> String -> m Unit
  -- | Attempts to decode incoming messages to `HydraNodeApi_InMessage`.
  -- | On decoding failure, logs the error and passes the raw message instead.
  , messageHandler :: HydraNodeApiWebSocket m -> Either String HydraNodeApi_InMessage -> m Unit
  }

-- | Configuration parameters for the hydra-node API WebSocket. 
-- |
-- | `url`: Address of the hydra-node API WebSocket.
-- |
-- | `runM`: Since the handlers of the underlying raw WebSocket are executed in
-- | the `Effect` monad, this function allows running client monad
-- | computations within that context.
-- |
-- | `handlers`: Handlers to attach to the established WebSocket connection.
-- |
-- | `txRetryStrategies`: Retry strategies for transactions that may be silently
-- |  dropped by the cardano-node due to limitations of the hydra-node.
type HydraNodeApiWebSocketBuilder (m :: Type -> Type) =
  { url :: Url
  , runM :: m Unit -> Effect Unit
  , handlers :: HydraNodeApiHandlers m
  , txRetryStrategies ::
      { close :: HydraTxRetryStrategy m
      , contest :: HydraTxRetryStrategy m
      }
  }

-- | Retry strategy to apply when submitting a Hydra transaction.
-- |
-- | Based on the Hydra documentation, Close and Contest transactions may be
-- | silently dropped by cardano-node. Since hydra-node does not handle this
-- | scenario itself, it is the responsibility of the client application to
-- | implement some retry mechanism.
-- | https://github.com/input-output-hk/hydra/blob/d12addeeec0a08d879b567556cb0686bef618936/docs/docs/getting-started/quickstart.md?plain=1#L196-L212
data HydraTxRetryStrategy (m :: Type -> Type)
  = RetryTxWithParams
      { delaySec :: Int
      , maxRetries :: Int
      , successPredicate :: m Boolean
      , failHandler :: m Unit
      }
  | DontRetryTx

-- | A default success predicate for the retry strategy for Close transaction.
-- | Checks whether the Hydra Head has been successfully closed by inspecting
-- | the current Head status.
defaultCloseHeadSuccessPredicate
  :: forall (m :: Type -> Type)
   . Functor m
  => { queryHeadStatus :: m HydraHeadStatus }
  -> m Boolean
defaultCloseHeadSuccessPredicate { queryHeadStatus } =
  (_ >= HeadStatus_Closed) <$> queryHeadStatus

-- | Establishes a WebSocket connection to the hydra-node, attaches the provided
-- | handlers, and returns a `HydraNodeApiWebSocket` handle with type-safe
-- | actions for interacting with the hydra-node API.
mkHydraNodeApiWebSocket
  :: forall (m :: Type -> Type)
   . MonadAff m
  => MonadLogger m
  => MonadRec m
  => HydraNodeApiWebSocketBuilder m
  -> m (HydraNodeApiWebSocket m)
mkHydraNodeApiWebSocket { url, handlers, runM, txRetryStrategies } = liftEffect do
  ws <- mkWebSocket
    { url
    , inMsgCodec: hydraNodeApiInMessageCodec
    , outMsgCodec: hydraNodeApiOutMessageCodec
    , runM
    }
  let
    hydraNodeApiWs :: HydraNodeApiWebSocket m
    hydraNodeApiWs =
      { baseWs: ws
      , initHead: ws.send Init
      , abortHead: ws.send Abort
      , submitTxL2: ws.send <<< NewTx <<< { transaction: _ } <<< mkHydraTx
      , closeHead:
          case txRetryStrategies.close of
            DontRetryTx ->
              ws.send Close
            RetryTxWithParams retryParams ->
              runM $ retry
                { actionName: "CloseHead"
                , action: liftEffect $ ws.send Close
                , delaySec: retryParams.delaySec
                , maxRetries: retryParams.maxRetries
                , successPredicate: const retryParams.successPredicate
                , failHandler: const retryParams.failHandler
                }
      , challengeSnapshot:
          case txRetryStrategies.contest of
            DontRetryTx ->
              ws.send Contest
            RetryTxWithParams retryParams ->
              runM $ retry
                { actionName: "ChallengeSnapshot"
                , action: liftEffect $ ws.send Contest
                , delaySec: retryParams.delaySec
                , maxRetries: retryParams.maxRetries
                , successPredicate: const retryParams.successPredicate
                , failHandler: const retryParams.failHandler
                }
      , fanout: ws.send Fanout
      }
  ws.onConnect do
    logInfo' $ "Connected to hydra-node WebSocket server (" <> url <> ")."
    handlers.connectHandler hydraNodeApiWs
  ws.onError (handlers.errorHandler hydraNodeApiWs)
  ws.onMessage \message -> do
    logTrace' $ "Received typed message from hydra-node WebSocket: " <> show message
    handlers.messageHandler hydraNodeApiWs message
  pure hydraNodeApiWs
