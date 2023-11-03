{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Base where

import Data.Semigroup
import RIO

type PublicKey = ByteString
type Signature = ByteString
type Witness = (PublicKey, Signature)

type Hash = ByteString

newtype Coin = Coin Integer
  deriving (Show, Eq)
  deriving (Semigroup, Monoid) via Sum Integer

-- | Hash of a serialized transaction
newtype TxHash = TxHash Hash
  deriving (Eq, Show)

-- | Hash of a PublicKey that can be used to verify the signature.
newtype Address = Address Hash
  deriving (Eq, Show)

data TxIn = TxIn
  { txInId :: TxHash
  , txInIx :: Int
  }
  deriving (Eq, Show)
