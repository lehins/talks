{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Lib
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Lib where

import Control.Concurrent.Async (replicateConcurrently)
import Control.Monad
import System.Random.Stateful
import Control.Monad.State
import Control.Monad.Reader
import Text.Printf
import Control.Monad.ST
import Data.STRef
import Data.Word

import qualified Data.ByteString as B
import qualified Data.Vector as VU
import qualified System.Random.MWC as MWC (Seed, fromSeed, toSeed)
import Codec.Serialise (Serialise(..), readFileDeserialise, writeFileSerialise, decode)

newtype AreaCode = AreaCode { unAreaCode :: Int }
  deriving (Eq, Show, Num)

-- | The North American Numbering Plan (NANP) phone
data Phone = Phone { phoneAreaCode :: AreaCode
                   , phoneLocalNumber :: Int
                   }

instance Show Phone where
  show Phone {phoneAreaCode, phoneLocalNumber} =
    let areaCode = unAreaCode phoneAreaCode
        (phoneSuffix, phonePostfix) = phoneLocalNumber `quotRem` 10000
     in printf "+1-%03d-%03d-%04d" areaCode phoneSuffix phonePostfix

randomPhone :: RandomGen g => [AreaCode] -> g -> (Phone, g)
randomPhone areaCodes g =
  let (i, g') = randomR (0, length areaCodes - 1) g
      (phoneLocalNumber, g'') = randomR (0, 9999999) g'
   in (Phone {phoneAreaCode = areaCodes !! i, phoneLocalNumber}, g'')

uniformPhone :: RandomGen g => [AreaCode] -> g -> (Phone, g)
uniformPhone areaCodes g =
  let (i, g') = uniformR (0, length areaCodes - 1) g
      (phoneLocalNumber, g'') = uniformR (0, 9999999) g'
   in (Phone {phoneAreaCode = areaCodes !! i, phoneLocalNumber}, g'')

uniformPhoneState :: RandomGen g => [AreaCode] -> g -> (Phone, g)
uniformPhoneState areaCodes =
  runState $ do
    i <- state $ uniformR (0, length areaCodes - 1)
    phoneLocalNumber <- state $ uniformR (0, 9999999)
    pure Phone {phoneAreaCode = areaCodes !! i, phoneLocalNumber}

uniformPhoneStateT :: (Monad m, RandomGen g) => [AreaCode] -> StateT g m Phone
uniformPhoneStateT areaCodes = do
  i <- state $ uniformR (0, length areaCodes - 1)
  phoneLocalNumber <- state $ uniformR (0, 9999999)
  pure Phone {phoneAreaCode = areaCodes !! i, phoneLocalNumber}


uniformPhoneM :: StatefulGen g m => [AreaCode] -> g -> m Phone
uniformPhoneM areaCodes gen = do
  i <- uniformRM (0, length areaCodes - 1) gen
  phoneLocalNumber <- uniformRM (0, 9999999) gen
  pure Phone {phoneAreaCode = areaCodes !! i, phoneLocalNumber}

uniformPhoneStateGen :: RandomGen g => [AreaCode] -> g -> (Phone, g)
uniformPhoneStateGen areaCodes g = runStateGen g (uniformPhoneM areaCodes)

data RGB a = RGB { red   :: a
                 , green :: a
                 , blue  :: a
                 } deriving (Eq, Show)

instance Uniform a => Uniform (RGB a) where
  uniformM g = RGB <$> uniformM g <*> uniformM g <*> uniformM g


uniformPhoneST :: RandomGen g => STRef s [AreaCode] -> STGenM g s -> ST s Phone
uniformPhoneST areaCodesRef stGen = do
  areaCodes <- readSTRef areaCodesRef
  uniformPhoneM areaCodes stGen

uniformPhone'' :: RandomGen g => [AreaCode] -> g -> (Phone, g)
uniformPhone'' areaCodes g = runSTGen g $ \stGen -> do
  areaCodesRef <- newSTRef areaCodes
  uniformPhoneST areaCodesRef stGen



uniformPhones :: RandomGen g => [AreaCode] -> g -> IO ([Phone], g)
uniformPhones areaCodes g = do
  (phones, AtomicGen g') <-
    withMutableGen (AtomicGen g) $ \atomicGen -> do
      n <- uniformRM (1, 5) atomicGen
      replicateConcurrently n (uniformPhoneM areaCodes atomicGen)
  pure (phones, g')



uniformPhoneReaderStateM :: (MonadReader [AreaCode] m, StatefulGen g m) => g -> m Phone
uniformPhoneReaderStateM gen = do
  areaCodes <- ask
  uniformPhoneM areaCodes gen

uniformPhoneMTL :: RandomGen g => [AreaCode] -> g -> (Phone, g)
uniformPhoneMTL = runState . runReaderT (uniformPhoneReaderStateM StateGenM)


withStoredGen :: (Serialise f, ThawedGen f IO) => FilePath -> (MutableGen f IO -> IO a) -> IO a
withStoredGen filepath action = do
  frozenGen <- readFileDeserialise filepath
  (result, frozenGen') <- withMutableGen frozenGen action
  writeFileSerialise filepath frozenGen'
  pure result


instance Serialise MWC.Seed where
  encode = encode . MWC.fromSeed
  decode = (MWC.toSeed :: VU.Vector Word32 -> MWC.Seed) <$> decode




randomByteStringNaive :: RandomGen g => Int -> g -> B.ByteString
randomByteStringNaive n = B.pack . take n . randoms




-- Generate a list of length n:

-- Direct recursion:

randomListDirect :: (Random a, RandomGen g) => Int -> g -> ([a], g)
randomListDirect = go []
  where
    go !xs i g
      | i <= 0 = (xs, g)
      | otherwise =
        case random g of
          (x, g') -> go (x : xs) (i - 1) g'

-- λ> :t (state random)
-- (state random) :: (Monad m, Random a, RandomGen s) => StateT s m a

randomListState :: (Random a, RandomGen s) => Int -> s -> ([a], s)
randomListState n = runState $ replicateM n $ state random


-- RandomGenM: A way to use `Random` and `RandomGen` classed with monadic


-- λ> :t randomM
-- randomM :: (RandomGenM g r m, Random a) => g -> m a

randomList :: (Random a, RandomGen r) => Int -> r -> ([a], r)
randomList n g = runStateGen g (replicateM n . randomM)



randomListM :: (Random a, RandomGen g, FrozenGen g m) => Int -> MutableGen g m -> m [a]
randomListM n = replicateM n . randomM


data Tree a = Node a (Tree a) | Leaf a
  deriving (Show, Eq)


class Random1 f where
  liftRandom :: RandomGen g => (g -> (a, g)) -> Int -> g -> (f a, g)


instance Random1 [] where
  liftRandom f n = runState $ replicateM n $ state f
