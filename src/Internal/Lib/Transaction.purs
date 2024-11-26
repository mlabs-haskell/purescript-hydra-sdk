module HydraSdk.Internal.Lib.Transaction
  ( fixCommitTx
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

fixCommitTx :: Transaction -> Contract Transaction
fixCommitTx = reSignTransaction <<< setAuxDataHash

setAuxDataHash :: Transaction -> Transaction
setAuxDataHash tx =
  tx # _body <<< _auxiliaryDataHash .~
    (hashAuxiliaryData <$> (unwrap tx).auxiliaryData)

reSignTransaction :: Transaction -> Contract Transaction
reSignTransaction tx = signTransaction (tx # _witnessSet <<< _vkeys .~ mempty)
