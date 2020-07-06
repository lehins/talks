{-# LANGUAGE FlexibleContexts #-}
module MassivTalk.Day1.Intro03_DelayedVsManifest where

import Prelude as P
import Data.Massiv.Array as A

---------------------
-- Class Hierarchy --
---------------------



-- class (Typeable r, Index ix) => Construct r ix e where

-- class (Typeable r, Index ix) => Load r ix e where

-- class Load r ix e => Source r ix e where

-- class (Load r ix e, Source r ix e) => Manifest r ix e where

-- class Manifest r ix e => Mutable r ix e where


-- | Fuse computation and avoid array allocations

paraboloid :: Int -> Array D Ix2 Double
paraboloid n = makeArrayR D Par (Sz2 n n) $ \(i :. j) -> f (i - n2) + f (j - n2)
  where
    n2 = n `div` 2
    f x = fromIntegral (x * x)

--
-- >>> arr = paraboloid 5
-- >>> A.sum $ A.map (+10) arr
-- 350.0


-- | Intermediate large size arrays

ridiculousSquare :: Int -> Array D Ix2 Double
ridiculousSquare k =
  makeArray Seq (Sz2 k k) $ \(i :. j) -> fromIntegral i ** sin (fromIntegral j)

--
-- >>> arr = ridiculousSquare maxBound
-- >>> computeAs U arr ! 0
-- >>> evaluateM arr (123456765432134 :. 2345677654345678)


-- | Loading of such array into memory in full is infeasible

--
-- >>> stride = Stride (maxBound `div` 3 :. maxBound `div` 2)
-- >>> print stride
-- >>> computeWithStrideAs U stride $ ridiculousSquare maxBound


-- | Indices that are too large are dangerous
-- | As long as the total number of elements is below `maxBound :: Int`, we are ok.

--
-- >>> arr = ridiculousSquare maxBound
-- >>> extractM 0 (Sz2 5 6) arr


-- | Computation


--
-- >>> :t computeAs

-- | The process of computation:
--
-- * Take a loadable array
-- * Allocate a mutable array of the same size
-- * Load each element of the loadable array into the new mutable according to
--   the computation strategy
-- * Freeze the mutable array and get as a result the pure manifest array.



-- | Fusion of computation and some pitfalls

--
-- >>> arr = A.map (sin . (+10)) $ paraboloid 3
-- >>> computeAs P $ A.zipWith (+) arr (A.map cos arr)

--
-- >>> arr = computeAs P $ A.map (sin . (+10)) $ paraboloid 3
-- >>> computeAs P $ A.zipWith (+) arr (A.map cos arr)

{- Rule of thumb. If delayed array is used more than once, compute it. -}

--
-- >>> arr = computeAs P $ A.map (sin . (+10)) $ paraboloid 3
-- >>> arr
-- >>> arr ! 1 :. 1






----------------
-- Push array --
----------------



identityD :: Int -> Array D Ix2 Int
identityD n =
  makeArray Seq (Sz2 n n) $ \(i :. j) ->
    if i == j
      then 1
      else 0

--
-- >>> computeAs P $ identityD 5


-- >>> :t makeLoadArrayS

identityDL :: Int -> Array DL Ix2 Int
identityDL n = makeLoadArrayS (Sz2 n n) 0 $ \ writeCell -> do
  let f i = writeCell (i :. i) 1
  A.mapM_ f (0 ... n - 1)
  -- Same as:
  -- P.mapM_ f [0 .. n - 1]

-- >>> identityDL 5

