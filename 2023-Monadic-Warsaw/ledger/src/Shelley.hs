{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Shelley where

import Coin
import Core
import KeyHash
import RIO

data ShelleyTxOut era = ShelleyTxOut
  { txOutAddress :: Address
  , txOutValue :: Value era
  }

data ShelleyTxBody era = ShelleyTxBody
  { txBodyInputs :: Set TxIn
  , txBodyOutputs :: [TxOut era]
  }

data ShelleyTx era = ShelleyTx
  { txBody :: TxBody era
  , txWitnesses :: Witnesses
  }

deriving instance Eq (Value era) => Eq (ShelleyTxOut era)
deriving instance Show (Value era) => Show (ShelleyTxOut era)
deriving instance Eq (TxOut era) => Eq (ShelleyTxBody era)
deriving instance Show (TxOut era) => Show (ShelleyTxBody era)
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
isShelleyTxBodyBalanced body txOutLookup =
  uncurry (==) $ txBodyBalance body txOutLookup
