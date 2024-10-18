module HydraSdk.Internal.NodeApi.Http
  ( commitRequest
  ) where

import Prelude

import Contract.Config (ServerConfig)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.ServerConfig (mkHttpUrl)
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import Effect.Aff (Aff)
import HydraSdk.Internal.Http.Error (HttpError)
import HydraSdk.Internal.Http.Utils (handleResponse, postRequest)
import HydraSdk.Internal.Types.CommitRequest
  ( HydraCommitRequest(SimpleCommitRequest, FullCommitRequest)
  , hydraFullCommitRequestCodec
  )
import HydraSdk.Internal.Types.Tx (HydraTx, hydraTxCodec)
import HydraSdk.Internal.Types.UtxoMap (hydraUtxoMapCodec)

commitRequest :: ServerConfig -> HydraCommitRequest -> Aff (Either HttpError HydraTx)
commitRequest serverConfig req =
  handleResponse hydraTxCodec <$>
    postRequest
      { url: mkHttpUrl serverConfig <</>> "commit"
      , content:
          Just $ case req of
            SimpleCommitRequest utxoMap ->
              CA.encode hydraUtxoMapCodec utxoMap
            FullCommitRequest req' ->
              CA.encode hydraFullCommitRequestCodec req'
      , headers: mempty
      }
