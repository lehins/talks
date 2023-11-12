{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Base where

import Data.ByteString
import Data.Semigroup
import Hash
import KeyHash

type Address = KeyHash 'Payment

type TxId = Hash Blake2b256

data TxIn = TxIn
  { txInId :: TxId
  , txInIx :: Int
  }
  deriving (Eq, Show)

type PublicKey = ByteString
type Signature = ByteString
type Witnesses = [(PublicKey, Signature)]

newtype Coin = Coin Integer
  deriving (Show, Eq)
  deriving (Semigroup, Monoid) via Sum Integer
