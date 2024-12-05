-- | This module provides convenience functions for working with
-- | `Cardano.Types.Transaction`.
module HydraSdk.Internal.Lib.Transaction
  ( reSignTransaction
  , setAuxDataHash
  ) where

import Prelude

import Cardano.Types (Transaction)
import Cardano.Types.AuxiliaryData (hashAuxiliaryData)
import Cardano.Types.Transaction (_body, _witnessSet)
import Cardano.Types.TransactionBody (_auxiliaryDataHash)
import Cardano.Types.TransactionWitnessSet (_vkeys)
import Contract.Monad (Contract)
import Contract.Transaction (signTransaction)
import Data.Lens ((.~))
import Data.Newtype (unwrap)

-- | Computes and sets the transaction auxiliary data hash.
setAuxDataHash :: Transaction -> Transaction
setAuxDataHash tx =
  tx # _body <<< _auxiliaryDataHash .~
    (hashAuxiliaryData <$> (unwrap tx).auxiliaryData)

-- | Removes existing vkey witnesses and signs the transaction.
reSignTransaction :: Transaction -> Contract Transaction
reSignTransaction tx = signTransaction (tx # _witnessSet <<< _vkeys .~ mempty)
