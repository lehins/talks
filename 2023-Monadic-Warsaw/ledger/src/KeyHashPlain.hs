module KeyHashPlain where

import Hash
import RIO
import RIO.Map as Map

newtype KeyHash = KeyHash (Hash Blake2b224)
  deriving (Eq, Ord, Show)

data Address = Address
  { paymentKeyHash :: KeyHash
  , stakingKeyHash :: KeyHash
  }

delegate :: Address -> KeyHash -> Map KeyHash KeyHash -> Map KeyHash KeyHash
delegate (Address _ staking) pool = Map.insert staking pool
-- Incorrect possibilities:
--delegate (Address _ staking) pool = Map.insert pool staking
--delegate (Address staking _) pool = Map.insert staking pool
--delegate (Address staking _) pool = Map.insert pool staking
