{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Core where

import Base
import Data.Kind
import Data.Set (Set)
import Lens.Micro (Lens', lens)


data ShelleyTxOut = ShelleyTxOut
  { txOutAddress :: Address
  , txOutValue :: Coin
  }
  deriving (Eq, Show)

class EraTxOut era where
  type TxOut era = (r :: Type) | r -> era
  addressTxOutL :: Lens' (TxOut era) Address
  valueTxOutL :: Lens' (TxOut era) Coin


data ShelleyTxBody era = ShelleyTxBody
  { txBodyInputs :: Set TxIn
  , txBodyOutputs :: [TxOut era]
  }
  --deriving (Eq, Show)

class EraTxOut era => EraTxBody era where
  type TxBody era = (r :: Type) | r -> era
  inputsTxBodyL :: Lens' (TxBody era) (Set TxIn)
  outputsTxBodyL :: Lens' (TxBody era) [TxOut era]

data Shelley

instance EraTxOut Shelley where
  type TxOut Shelley = ShelleyTxOut
  addressTxOutL = lens txOutAddress $ \out addr -> out{txOutAddress = addr}
  valueTxOutL = lens txOutValue $ \out val -> out{txOutValue = val}

-- data Allegra

-- instance EraTxOut Allegra where
--   type TxOut Allegra = ShelleyTxOut Allegra
--   addressTxOutL = lens txOutAddress $ \out addr -> out{txOutAddress = addr}
--   valueTxOutL = lens txOutValue $ \out val -> out{txOutValue = val}

-- class EraTxBody era => EraTx era where
--   type Tx era = (r :: Type) | r -> era
--   bodyTxL :: Lens' (Tx era) (TxBody era)
--   witnessesTxL :: Lens' (Tx era) Witnesses

-- txBodyBalance
--   :: EraTxBody era
--   => TxBody era
--   -> (TxIn -> TxOut era)
--   -> (Value era, Value era)
-- txBodyBalance body txOutLookup = (consumed, produced)
--   where
--     consumed = foldMap (view valueTxOutL . txOutLookup) (body ^. inputsTxBodyL)
--     produced = foldMap (view valueTxOutL) (body ^. outputsTxBodyL)
