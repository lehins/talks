{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}

module Coin where

import Data.Semigroup

newtype Coin = Coin Integer
  deriving (Show, Eq)
  deriving (Semigroup, Monoid) via Sum Integer
