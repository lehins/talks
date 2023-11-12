module KeyHashPlain where

import Hash
import RIO
import RIO.Map as Map

newtype KeyHash = KeyHash (Hash Blake2b224)
  deriving (Eq, Ord, Show)

delegate
  :: KeyHash -- ^ Delegation KeyHash
  -> KeyHash -- ^ StakePool KeyHash
  -> Map KeyHash KeyHash
  -> Map KeyHash KeyHash
delegate staking pool = Map.insert staking pool

-- Incorrect possibilities:
-- delegate staking pool = Map.insert pool staking
