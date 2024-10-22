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
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import HydraSdk.Internal.Lib.Retry (retry)
import HydraSdk.Internal.Lib.WebSocket (Url, WebSocket, mkWebSocket)
import HydraSdk.Internal.Types.HeadStatus (HydraHeadStatus(HeadStatus_Closed))
import HydraSdk.Internal.Types.NodeApiMessage
  ( HydraNodeApi_InMessage
  , HydraNodeApi_OutMessage(Out_Init, Out_Abort, Out_NewTx, Out_Close, Out_Contest, Out_Fanout)
  , hydraNodeApiInMessageCodec
  , hydraNodeApiOutMessageCodec
  )
import HydraSdk.Internal.Types.Tx (mkHydraTx)

type HydraNodeApiWebSocket (m :: Type -> Type) =
  { baseWs :: WebSocket m HydraNodeApi_InMessage HydraNodeApi_OutMessage
  , initHead :: Effect Unit
  , abortHead :: Effect Unit
  , submitTxL2 :: Transaction -> Effect Unit
  , closeHead :: Effect Unit
  , challengeSnapshot :: Effect Unit
  , fanout :: Effect Unit
  }

type HydraNodeApiHandlers (m :: Type -> Type) =
  { connectHandler :: HydraNodeApiWebSocket m -> m Unit
  , errorHandler :: HydraNodeApiWebSocket m -> String -> m Unit
  , messageHandler :: HydraNodeApiWebSocket m -> HydraNodeApi_InMessage -> m Unit
  }

type HydraNodeApiWebSocketBuilder (m :: Type -> Type) =
  { url :: Url
  -- ^ Address of the hydra-node API WebSocket.
  , runM :: m Unit -> Effect Unit
  -- ^ Since the handlers of the underlying raw WebSocket are executed in the
  -- `Effect` monad, this function allows running client monad computations
  -- within that context.
  , handlers :: HydraNodeApiHandlers m
  -- ^ Handlers to attach to the established WebSocket connection. 
  , txRetryStrategies ::
      { close :: HydraTxRetryStrategy m
      , contest :: HydraTxRetryStrategy m
      }
  -- ^ Retry strategies for transactions that may be silently dropped by the
  -- cardano-node due to limitations of the hydra-node.
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
      , initHead: ws.send Out_Init
      , abortHead: ws.send Out_Abort
      , submitTxL2: ws.send <<< Out_NewTx <<< { transaction: _ } <<< mkHydraTx
      , closeHead:
          case txRetryStrategies.close of
            DontRetryTx ->
              ws.send Out_Close
            RetryTxWithParams retryParams ->
              runM $ retry
                { actionName: "CloseHead"
                , action: liftEffect $ ws.send Out_Close
                , delaySec: retryParams.delaySec
                , maxRetries: retryParams.maxRetries
                , successPredicate: const retryParams.successPredicate
                , failHandler: const retryParams.failHandler
                }
      , challengeSnapshot:
          case txRetryStrategies.contest of
            DontRetryTx ->
              ws.send Out_Contest
            RetryTxWithParams retryParams ->
              runM $ retry
                { actionName: "ChallengeSnapshot"
                , action: liftEffect $ ws.send Out_Contest
                , delaySec: retryParams.delaySec
                , maxRetries: retryParams.maxRetries
                , successPredicate: const retryParams.successPredicate
                , failHandler: const retryParams.failHandler
                }
      , fanout: ws.send Out_Fanout
      }
  ws.onConnect do
    logInfo' $ "Connected to hydra-node WebSocket server (" <> url <> ")."
    handlers.connectHandler hydraNodeApiWs
  ws.onError (handlers.errorHandler hydraNodeApiWs)
  ws.onMessage \message -> do
    logTrace' $ "Received typed message from hydra-node WebSocket: " <> show message
    handlers.messageHandler hydraNodeApiWs message
  pure hydraNodeApiWs
