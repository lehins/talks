{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Base where

import Data.ByteString
import Data.Semigroup
import Hash
import KeyHash

newtype Coin = Coin Integer
  deriving (Show, Eq)
  deriving (Semigroup, Monoid) via Sum Integer

type Address = KeyHash 'Payment

type TxId = Hash Blake2b256

type PublicKey = ByteString
type Signature = ByteString
type Witnesses = [(PublicKey, Signature)]

data TxIn = TxIn
  { txInId :: TxId
  , txInIx :: Int
  }
  deriving (Eq, Show)
