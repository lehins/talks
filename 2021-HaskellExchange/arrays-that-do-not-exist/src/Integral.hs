{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module      : Integral
-- Copyright   : (c) Alexey Kuleshevich 2021
-- License     : BSD3
-- Maintainer  : Alexey Kuleshevich <alexey@kuleshevi.ch>
-- Stability   : experimental
-- Portability : non-portable
--
module Integral where

import Control.DeepSeq
import Control.Scheduler
import Data.Massiv.Array as A
import Data.List as L
import Data.Massiv.Array.Numeric.Integral as A
import Data.Foldable as F
import Control.Parallel (par, pseq)
import Common

integrate :: Fractional e => (e -> e -> e) -> e -> e -> e
integrate f x0 y0 =
  let x0' = x0 - 0.5
      y0' = y0 - 0.5
      n = 100
      nf = fromIntegral n
      delta = 1 / 100
      delta2 = delta * delta
   in A.sum $
      makeArrayR D Seq (Sz2 n n) $ \(i :. j) ->
        let x = x0' + fromIntegral j / nf
            y = y0' + fromIntegral i / nf
         in delta2 * f (x + delta / 2) (y + delta / 2)
{-# INLINE integrate #-}


makeSquareMatrix :: (Load r Ix2 e, Num e) => (e -> e -> e) -> r -> Comp -> Int -> Matrix r e
makeSquareMatrix f _ comp n =
  let n2 = n `div` 2
   in makeArray comp (Sz2 n n) $ \(i :. j) ->
        let x = fromIntegral (j - n2)
            y = fromIntegral (i - n2)
         in f x y
{-# INLINE makeSquareMatrix #-}

makeNestedLists :: Num e => (e -> e -> e) -> Int -> [[e]]
makeNestedLists f n =
  let n2 = n `div` 2
   in [ [ let x = fromIntegral (j - n2)
              y = fromIntegral (i - n2)
           in f x y
        | j <- [0 .. n - 1]
        ]
      | i <- [0 .. n - 1]
      ]
{-# INLINE makeNestedLists #-}

makeNestedListsPar :: (NFData e, Num e) => (e -> e -> e) -> Int -> [[e]]
makeNestedListsPar f n =
  let n2 = n `div` 2
      g i
        | i < n =
          let row =
                [ let x = fromIntegral (j - n2)
                      y = fromIntegral (i - n2)
                   in f x y
                | j <- [0 .. n - 1]
                ]
              i' = i + 1
           in
            rnf row `par` i' `pseq` Just (row, i')
        | otherwise = Nothing
   in L.unfoldr g 0
{-# INLINE makeNestedListsPar #-}


gaussMatrixNaive :: (Load r Ix2 e, Floating e) => r -> Int -> Matrix r e
gaussMatrixNaive r = makeSquareMatrix gauss r Seq


gaussMatrix :: (Load r Ix2 e, Floating e) => r -> Int -> Matrix r e
gaussMatrix r = makeSquareMatrix (integrate gauss) r Seq




gaussMatrixSupplied :: (Prim a, Floating a) => Comp -> Int -> Matrix P a
gaussMatrixSupplied comp n =
  compute $ midpointRule comp P (\f (i :. j) -> gauss (f i) (f j)) a0 1 (Sz2 n n) 100
  where
    a0 = -fromIntegral n / 2
{-# INLINE gaussMatrixSupplied #-}


kahanSum :: (Foldable t, RealFloat a) => t a -> a
kahanSum = fst . F.foldl' ksum (0, 0)
  where
    ksum (s, c) x =
      let !y = x - c
          !s' = s + y
          !c' = (s' - s) - y
       in (s', c')

kahanBabushkaSum :: (Foldable t, RealFloat a) => t a -> a
kahanBabushkaSum = uncurry (+) . F.foldl' ksum (0, 0)
  where
    ksum (s, c) x =
      let !s' = s + x
          !c'
            | abs s >= abs x = c + (s - s') + x
            | otherwise = c + (x - s') + s
       in (s', c')


mkGaussian2 :: Int -> Int -> Array D Ix2 Double
mkGaussian2 n side  =
  let f scale (i :. j) = gauss (scale i) (scale j)
      {-# INLINE f #-}
      sz = Sz (side :. side)
      a = fromIntegral side / 2 - fromIntegral side
      d = 1
  in simpsonsRule Par P f a d sz n
{-# INLINE mkGaussian2 #-}
