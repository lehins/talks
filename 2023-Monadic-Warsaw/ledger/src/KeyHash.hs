{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module KeyHash where

import Hash
import RIO
import RIO.Map as Map

data KeyRole
  = Payment
  | Staking
  | StakePool
  | Minting
  | Witness

newtype KeyHash (r :: KeyRole) = KeyHash (Hash Blake2b224)
  deriving (Eq, Ord, Show)

data Address = Address
  { paymentKeyHash :: KeyHash 'Payment
  , stakingKeyHash :: KeyHash 'Staking
  }
  deriving (Eq, Ord, Show)

delegate
  :: Address
  -> KeyHash 'StakePool
  -> Map (KeyHash 'Staking) (KeyHash 'StakePool)
  -> Map (KeyHash 'Staking) (KeyHash 'StakePool)
delegate (Address _ staking) pool = Map.insert staking pool
