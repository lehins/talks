{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module TxCerts where

import Data.Kind
import KeyHash
import RIO
import RIO.Map as Map
import Shelley
import Mary

data ShelleyTxCert -- era
  = RegTxCert (KeyHash 'Staking)
  | UnRegTxCert (KeyHash 'Staking)
  | DelegTxCert (KeyHash 'Staking) (KeyHash 'StakePool)

class EraTxCert era where
  -- type TxCert era :: Type
  type TxCert era = (r :: Type) | r -> era
  getRegTxCert :: TxCert era -> Maybe (KeyHash 'Staking)
  getUnRegTxCert :: TxCert era -> Maybe (KeyHash 'Staking)
  getDelegTxCert :: TxCert era -> Maybe (KeyHash 'Staking, KeyHash 'StakePool)

applyTxCert
  :: EraTxCert era
  => TxCert era
  -> Map (KeyHash 'Staking) (KeyHash 'StakePool)
  -> Map (KeyHash 'Staking) (KeyHash 'StakePool)
applyTxCert txCert m
  | Just keyHash <- getUnRegTxCert txCert =
      Map.delete keyHash m
  | Just (keyHash, poolId) <- getDelegTxCert txCert =
      Map.insert keyHash poolId m
  | otherwise = m


instance EraTxCert Shelley where
  type TxCert Shelley = ShelleyTxCert
-- instance EraTxCert Mary where
--   type TxCert Mary = ShelleyTxCert
