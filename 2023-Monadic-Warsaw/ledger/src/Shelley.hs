{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley where


import RIO
import Core
import Base


data ShelleyTxOut era = ShelleyTxOut
  { txOutAddress :: Address
  , txOutValue :: Value era
  }
deriving instance Eq (Value era) => Eq (ShelleyTxOut era)
deriving instance Show (Value era) => Show (ShelleyTxOut era)

data ShelleyTxBody era = ShelleyTxBody
  { txBodyInputs :: Set TxIn
  , txBodyOutputs :: [TxOut era]
  }
deriving instance Eq (TxOut era) => Eq (ShelleyTxBody era)
deriving instance Show (TxOut era) => Show (ShelleyTxBody era)

data ShelleyTx era = ShelleyTx
  { txBody :: TxBody era
  , txWitnesses :: [Witness]
  }
deriving instance Eq (TxBody era) => Eq (ShelleyTx era)
deriving instance Show (TxBody era) => Show (ShelleyTx era)





data Shelley

instance EraTxOut Shelley where
  type TxOut Shelley = ShelleyTxOut Shelley
  type Value Shelley = Coin
  addressTxOutL = lens txOutAddress $ \out addr -> out{txOutAddress = addr}
  valueTxOutL = lens txOutValue $ \out val -> out{txOutValue = val}

instance EraTxBody Shelley where
  type TxBody Shelley = ShelleyTxBody Shelley
  inputsTxBodyL = lens txBodyInputs $ \body ins -> body{txBodyInputs = ins}
  outputsTxBodyL = lens txBodyOutputs $ \body outs -> body{txBodyOutputs = outs}

instance EraTx Shelley where
  type Tx Shelley = ShelleyTx Shelley
  bodyTxL = lens txBody $ \tx body -> tx{txBody = body}
  witnessesTxL = lens txWitnesses $ \tx outs -> tx{txWitnesses = outs}

isShelleyTxBodyBalanced
  :: EraTxBody era
  => TxBody era
  -> (TxIn -> TxOut era)
  -> Bool
isShelleyTxBodyBalanced body txOutLookup = consumed == produced
  where
    (consumed, produced) = shelleyTxBodyBalance body txOutLookup

shelleyTxBodyBalance
  :: EraTxBody era
  => TxBody era
  -> (TxIn -> TxOut era)
  -> (Value era, Value era)
shelleyTxBodyBalance body txOutLookup = (consumed, produced)
  where
    consumed = foldMap (view valueTxOutL . txOutLookup) (body ^. inputsTxBodyL)
    produced = foldMap (view valueTxOutL) (body ^. outputsTxBodyL)
