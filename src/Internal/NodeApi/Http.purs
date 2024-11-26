module HydraSdk.Internal.NodeApi.Http
  ( commitRequest
  , commitRequestAff
  ) where

import Prelude

import Contract.Config (ServerConfig)
import Contract.Monad (Contract)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.ServerConfig (mkHttpUrl)
import Data.Codec.Argonaut (encode) as CA
import Data.Either (Either)
import Data.Maybe (Maybe(Just))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import HydraSdk.Internal.Http.Error (HttpError)
import HydraSdk.Internal.Http.Utils (handleResponse, postRequest)
import HydraSdk.Internal.Lib.Transaction (fixCommitTx)
import HydraSdk.Internal.Types.CommitRequest
  ( HydraCommitRequest(SimpleCommitRequest, FullCommitRequest)
  , hydraFullCommitRequestCodec
  )
import HydraSdk.Internal.Types.Tx (HydraTx, hydraTxCodec)
import HydraSdk.Internal.Types.UtxoMap (hydraUtxoMapCodec)

commitRequest :: ServerConfig -> HydraCommitRequest -> Contract (Either HttpError HydraTx)
commitRequest serverConfig req =
  liftAff (commitRequestAff serverConfig req) >>=
    traverse
      ( \hydraTx ->
          fixCommitTx hydraTx.cborHex <#> \fixedTx ->
            hydraTx { cborHex = fixedTx }
      )

commitRequestAff :: ServerConfig -> HydraCommitRequest -> Aff (Either HttpError HydraTx)
commitRequestAff serverConfig req =
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
