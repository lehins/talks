{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module      : Parallel
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Common where

import Control.Monad.ST
import Control.Parallel (par)
import Control.Parallel.Strategies (parTraversable, rpar, runEval)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Primitive as VP


makeNestedL :: (Int, Int) -> ((Int, Int) -> e) -> [[e]]
makeNestedL (m, n) f =
  [ [ f (i, j)
    | j <- [0 .. n - 1]
    ]
  | i <- [0 .. m - 1]
  ]
{-# INLINE makeNestedL #-}

gauss :: Floating a => a -> a -> a
gauss x y = exp (- (x ^ (2 :: Int) + y ^ (2 :: Int)))
{-# INLINE gauss #-}

makeNestedV ::
  VP.Prim e => (Int, Int) -> ((Int, Int) -> e) -> V.Vector (VP.Vector e)
makeNestedV (m, n) f = V.generate m $ \i -> VP.generate n $ \j -> f (i, j)
{-# INLINE makeNestedV #-}

makeNestedVpar ::
  VP.Prim e => (Int, Int) -> ((Int, Int) -> e) -> V.Vector (VP.Vector e)
makeNestedVpar (m, n) f = V.unfoldrExactN m inner 0
  where
    inner i =
      let v = VP.generate n $ \j -> f (i, j)
       in v `par` (v, i + 1)
{-# INLINE makeNestedVpar #-}

makeNestedVrpar ::
  VP.Prim e => (Int, Int) -> ((Int, Int) -> e) -> V.Vector (VP.Vector e)
makeNestedVrpar sz f = runEval $ parTraversable rpar $ makeNestedV sz f
{-# INLINE makeNestedVrpar #-}

makeMatrixVP :: VP.Prim e => (Int, Int) -> ((Int, Int) -> e) -> VP.Vector e
makeMatrixVP (m, n) f = VP.generate (m * n) $ \ !k -> f (k `quotRem` m)
{-# INLINE makeMatrixVP #-}


makeMatrixMutV :: VG.Vector v e => (Int, Int) -> ((Int, Int) -> e) -> v e
makeMatrixMutV (m, n) f =
  runST $ do
    mutArr <- VGM.unsafeNew (m * n)
    let fillRows i
          | i < m = do
            let fillColumns j
                  | j < n = do
                    VGM.unsafeWrite mutArr (n * i + j) $! f (i, j)
                    fillColumns (j + 1)
                  | otherwise = pure ()
            fillColumns 0
            fillRows (i + 1)
          | otherwise = pure ()
    fillRows 0
    VG.unsafeFreeze mutArr
{-# INLINE makeMatrixMutV #-}

