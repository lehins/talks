{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}

module Byron where

import Base hiding (TxIn)
import Data.Set (Set)
import Lens.Micro


data TxOut = TxOut
  { txOutAddress :: Address
  , txOutValue :: Coin
  }
  deriving (Eq, Show)

data TxIn = TxIn
  { txInId :: TxId
  , txInIx :: Int
  }
  deriving (Eq, Show)

data TxBody = TxBody
  { txBodyInputs :: Set TxIn
  , txBodyOutputs :: [TxOut]
  }
  deriving (Eq, Show)

data Tx = Tx
  { txBody :: TxBody
  , txWitnesses :: Witnesses
  }
  deriving (Eq, Show)

isTxBodyBalanced :: TxBody -> (TxIn -> TxOut) -> Bool
isTxBodyBalanced TxBody{txBodyInputs, txBodyOutputs} txOutLookup =
  let consumed = foldMap (txOutValue . txOutLookup) txBodyInputs
      produced = foldMap txOutValue txBodyOutputs
   in consumed == produced

txOutAddressL :: Lens' TxOut Address
txOutAddressL = lens txOutAddress $ \txOut address -> txOut {txOutAddress = address}

-- > let txOut' = txOut & txOutAddressL .~ "0xDEADBEEF"
-- > txOut' .^ txOutAddressL
-- "0xDEADBEEF"
