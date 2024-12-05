-- | This module provides functions for making requests
-- | to hydra-node HTTP endpoints.
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

-- | Builds a Hydra Commit transaction ready for submission
-- | to the L1 network.
-- | 
-- | Sends a request to the hydra-node API HTTP server as specified
-- | by the first argument. The second argument is used to pass
-- | the request body, which can either correspond to a simple request
-- | consisting of plain utxos or a full commit request with
-- | a blueprint transaction to be complemented by the hydra-node.
-- | The later type allows building Commit transactions of arbitrary
-- | complexity.
-- |
-- | For details on the motivation behind blueprint transactions, see this discussion: 
-- | https://github.com/cardano-scaling/hydra/discussions/1337
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
