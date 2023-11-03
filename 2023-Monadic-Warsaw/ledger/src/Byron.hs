{-# LANGUAGE NamedFieldPuns #-}

module Byron where

import RIO
import Base

data TxOut = TxOut
  { txOutAddress :: Address
  , txOutValue :: Coin
  }
  deriving (Eq, Show)

data TxBody = TxBody
  { txBodyInputs :: Set TxIn
  , txBodyOutputs :: [TxOut]
  , txBodyWitnesses :: [Witness]
  }
  deriving (Eq, Show)

data Tx = Tx
  { txBody :: TxBody
  , txWitnesses :: [Witness]
  }
  deriving (Eq, Show)

-- Need to redefine most of the types
-- Cannot reuse all of the validations rules

isTxBodyBalanced :: TxBody -> (TxIn -> TxOut) -> Bool
isTxBodyBalanced TxBody{txBodyInputs, txBodyOutputs} txOutLookup = consumed == produced
  where
    consumed = foldMap (txOutValue . txOutLookup) txBodyInputs
    produced = foldMap txOutValue txBodyOutputs

