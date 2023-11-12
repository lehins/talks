{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}

module KeyHash where

--import Data.Coerce (coerce)
import Hash
import Data.Map as Map (Map, insert)

data KeyRole
  = Payment
  | Staking
  | StakePool
  | Minting
  | Witness

newtype KeyHash (r :: KeyRole) = KeyHash (Hash Blake2b224)
  deriving (Eq, Ord, Show)

delegate
  :: KeyHash 'Staking
  -> KeyHash 'StakePool
  -> Map (KeyHash 'Staking) (KeyHash 'StakePool)
  -> Map (KeyHash 'Staking) (KeyHash 'StakePool)
delegate staking pool = Map.insert staking pool

-- h28 :: Hash Blake2b224
-- h28 = hash "Cardano" :: Hash Blake2b224

-- h32 = coerce h28 :: Hash Blake2b256
