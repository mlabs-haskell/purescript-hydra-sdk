module HydraSdk.Internal.Lib.WebSocket
  ( WebSocket
  , WebSocketBuilder
  , WebSocketUrl
  , mkWebSocket
  ) where

import Prelude

import Control.Monad.Logger.Class (class MonadLogger)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError) as CA
import Data.Either (Either(Left, Right))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import HydraSdk.Internal.Lib.Codec (caDecodeString, caEncodeString)
import HydraSdk.Internal.Lib.Logger (logError, logTrace)

foreign import data JsWebSocket :: Type

foreign import _initWs :: WebSocketUrl -> Effect JsWebSocket

foreign import _sendWs :: JsWebSocket -> String -> Effect Unit

foreign import _closeWs :: JsWebSocket -> Effect Unit

foreign import _onWsConnect :: JsWebSocket -> (Effect Unit) -> Effect Unit

foreign import _onWsMessage :: JsWebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _onWsError :: JsWebSocket -> (String -> Effect Unit) -> Effect Unit

foreign import _onWsClose :: JsWebSocket -> (Int -> String -> Effect Unit) -> Effect Unit

type WebSocketUrl = String

type WebSocket (m :: Type -> Type) (in_ :: Type) (out :: Type) =
  { onConnect :: m Unit -> Effect Unit
  , onMessage :: (Either String in_ -> m Unit) -> Effect Unit
  , onError :: (String -> m Unit) -> Effect Unit
  , onClose :: (Int -> String -> m Unit) -> Effect Unit
  , send :: out -> Effect Unit
  , close :: Effect Unit
  }

type WebSocketBuilder (m :: Type -> Type) (in_ :: Type) (out :: Type) =
  { url :: WebSocketUrl
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
  _initWs builder.url <#> \ws ->
    { onConnect:
        \callback ->
          _onWsConnect ws (builder.runM callback)

    , onMessage:
        \callback ->
          _onWsMessage ws \msgRaw ->
            builder.runM do
              logTrace $ "mkWebSocket:onMessage: Raw message: " <> msgRaw
              case caDecodeString builder.inMsgCodec msgRaw of
                Left decodeErr -> do
                  logError $ "mkWebSocket:onMessage: Decode error: " <>
                    CA.printJsonDecodeError decodeErr
                  callback $ Left msgRaw
                Right msg ->
                  callback $ Right msg

    , onError:
        \callback ->
          void $ _onWsError ws (builder.runM <<< callback)

    , onClose:
        \callback ->
          _onWsClose ws (\code -> builder.runM <<< callback code)

    , send:
        \msg ->
          _sendWs ws $ caEncodeString builder.outMsgCodec msg

    , close:
        _closeWs ws
    }
