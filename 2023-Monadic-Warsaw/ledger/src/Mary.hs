{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Mary where

import Base
import Core
import RIO
import qualified RIO.Map as Map
import Shelley hiding (ShelleyTxBody (..))
import KeyHash

newtype MultiAsset = MultiAsset (Map (KeyHash 'Minting) Integer)
  deriving (Show, Eq)

instance Semigroup MultiAsset where
  MultiAsset m1 <> MultiAsset m2 = MultiAsset $ Map.unionWith (+) m1 m2

instance Monoid MultiAsset where
  mempty = MultiAsset mempty

data MaryValue = MaryValue Coin MultiAsset
  deriving (Show, Eq)

instance Semigroup MaryValue where
  MaryValue c1 m1 <> MaryValue c2 m2 = MaryValue (c1 <> c2) (m1 <> m2)

instance Monoid MaryValue where
  mempty = MaryValue mempty mempty



data MaryTxBody era = MaryTxBody
  { txBodyInputs :: Set TxIn
  , txBodyOutputs :: [TxOut era]
  , txBodyMint :: MultiAsset
  }
deriving instance Eq (TxOut era) => Eq (MaryTxBody era)
deriving instance Show (TxOut era) => Show (MaryTxBody era)

class EraTxBody era => MaryEraTxBody era where
  mintTxBodyL :: Lens' (TxBody era) MultiAsset


data Mary

instance EraTxOut Mary where
  type TxOut Mary = ShelleyTxOut Mary
  type Value Mary = MaryValue
  addressTxOutL = lens txOutAddress $ \out addr -> out{txOutAddress = addr}
  valueTxOutL = lens txOutValue $ \out val -> out{txOutValue = val}

instance EraTxBody Mary where
  type TxBody Mary = MaryTxBody Mary
  inputsTxBodyL = lens txBodyInputs $ \body ins -> body{txBodyInputs = ins}
  outputsTxBodyL = lens txBodyOutputs $ \body outs -> body{txBodyOutputs = outs}

instance EraTx Mary where
  type Tx Mary = ShelleyTx Mary
  bodyTxL = lens txBody $ \tx body -> tx{txBody = body}
  witnessesTxL = lens txWitnesses $ \tx outs -> tx{txWitnesses = outs}

isMaryTxBodyBalanced
  :: (MaryEraTxBody era, Value era ~ MaryValue)
  => TxBody era
  -> (TxIn -> TxOut era)
  -> Bool
isMaryTxBodyBalanced body txOutLookup =
  consumed == produced <> MaryValue (Coin 0) (body ^. mintTxBodyL)
  where
    (consumed, produced) = txBodyBalance body txOutLookup
