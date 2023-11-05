{-# LANGUAGE NamedFieldPuns #-}

module Byron where

import RIO
import Base

data TxOut = TxOut
  { txOutAddress :: Address
  , txOutValue :: Coin
  }
  deriving (Eq, Show)

type Hash = ByteString
type Address = Hash

data TxIn = TxIn
  { txInId :: TxId
  , txInIx :: Int
  }
  deriving (Eq, Show)

type TxId = Hash

data TxBody = TxBody
  { txBodyInputs :: Set TxIn
  , txBodyOutputs :: [TxOut]
  }
  deriving (Eq, Show)

type PublicKey = ByteString
type Signature = ByteString
type Witnesses = [(PublicKey, Signature)]

data Tx = Tx
  { txBody :: TxBody
  , txWitnesses :: Witnesses
  }
  deriving (Eq, Show)

isTxBodyBalanced :: TxBody -> (TxIn -> TxOut) -> Bool
isTxBodyBalanced TxBody{txBodyInputs, txBodyOutputs} txOutLookup = consumed == produced
  where
    consumed = foldMap (txOutValue . txOutLookup) txBodyInputs
    produced = foldMap txOutValue txBodyOutputs


-- Need to redefine most of the types
-- Cannot reuse all of the validations rules
