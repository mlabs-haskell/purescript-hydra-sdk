module HydraSdk.Internal.Lib.WebSocket
  ( module ExportUrl
  , WebSocket
  , WebSocketBuilder
  , mkWebSocket
  ) where

import Prelude

import Contract.Log (logTrace')
import Control.Monad.Logger.Class (class MonadLogger)
import Ctl.Internal.JsWebSocket
  ( JsWebSocket
  , Url
  , _mkWebSocket
  , _onWsConnect
  , _onWsError
  , _onWsMessage
  , _wsClose
  , _wsFinalize
  , _wsSend
  )
import Ctl.Internal.JsWebSocket (Url) as ExportUrl
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError) as CA
import Data.Either (either)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import HydraSdk.Internal.Lib.Codec (caDecodeString, caEncodeString)

foreign import _onWsClose :: JsWebSocket -> (Int -> String -> Effect Unit) -> Effect Unit

type WebSocket (m :: Type -> Type) (in_ :: Type) (out :: Type) =
  { onConnect :: m Unit -> Effect Unit
  , onMessage :: (in_ -> m Unit) -> Effect Unit
  , onError :: (String -> m Unit) -> Effect Unit
  , onClose :: (Int -> String -> m Unit) -> Effect Unit
  , send :: out -> Effect Unit
  , close :: Effect Unit
  }

type WebSocketBuilder (m :: Type -> Type) (in_ :: Type) (out :: Type) =
  { url :: Url
  , inMsgCodec :: CA.JsonCodec in_
  , outMsgCodec :: CA.JsonCodec out
  , runM :: m Unit -> Effect Unit
  }

mkWebSocket
  :: forall (m :: Type -> Type) (in_ :: Type) (out :: Type)
   . MonadEffect m
  => MonadLogger m
  => WebSocketBuilder m in_ out
  -> Effect (WebSocket m in_ out)
mkWebSocket builder =
  _mkWebSocket wsLogger builder.url <#> \ws ->
    { onConnect:
        \callback ->
          _onWsConnect ws (builder.runM callback)

    , onMessage:
        \callback ->
          _onWsMessage ws wsLogger \msgRaw ->
            builder.runM do
              logTrace' $ "onMessage raw: " <> msgRaw
              either
                (logTrace' <<< append "onMessage decode error: " <<< CA.printJsonDecodeError)
                callback
                (caDecodeString builder.inMsgCodec msgRaw)

    , onError:
        \callback ->
          void $ _onWsError ws (builder.runM <<< callback)

    , onClose:
        \callback ->
          _onWsClose ws (\code -> builder.runM <<< callback code)

    , send:
        \msg ->
          _wsSend ws wsLogger (caEncodeString builder.outMsgCodec msg)

    , close:
        _wsFinalize ws *> _wsClose ws
    }
  where
  wsLogger :: String -> Effect Unit
  wsLogger _ = pure unit
