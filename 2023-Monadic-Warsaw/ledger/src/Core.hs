{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Core where

import Data.Kind
import Hash
import KeyHash
import RIO

newtype TxId = TxId (Hash Blake2b256)
  deriving (Eq, Ord, Show)

data TxIn = TxIn
  { txInId :: TxId
  , txInIx :: Int
  }
  deriving (Eq, Show)

class (Eq (Value era), Monoid (Value era)) => EraTxOut era where
  type TxOut era = (r :: Type) | r -> era
  type Value era :: Type
  addressTxOutL :: Lens' (TxOut era) Address
  valueTxOutL :: Lens' (TxOut era) (Value era)

class EraTxOut era => EraTxBody era where
  type TxBody era = (r :: Type) | r -> era
  inputsTxBodyL :: Lens' (TxBody era) (Set TxIn)
  outputsTxBodyL :: Lens' (TxBody era) [TxOut era]

txBodyBalance
  :: EraTxBody era
  => TxBody era
  -> (TxIn -> TxOut era)
  -> (Value era, Value era)
txBodyBalance body txOutLookup = (consumed, produced)
  where
    consumed = foldMap (view valueTxOutL . txOutLookup) (body ^. inputsTxBodyL)
    produced = foldMap (view valueTxOutL) (body ^. outputsTxBodyL)

type PublicKey = ByteString
type Signature = ByteString
type Witnesses = [(PublicKey, Signature)]

class EraTxBody era => EraTx era where
  type Tx era = (r :: Type) | r -> era
  bodyTxL :: Lens' (Tx era) (TxBody era)
  witnessesTxL :: Lens' (Tx era) Witnesses
