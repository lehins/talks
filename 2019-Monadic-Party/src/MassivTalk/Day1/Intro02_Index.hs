{-# OPTIONS_GHC -Wno-unused-imports #-}
module MassivTalk.Day1.Intro02_Index where

import Data.Massiv.Array as A

-------------
-- Indices --
-------------


-- | Started simple, with something that we are all familar with: tuples

--
-- >>> (1,1,1) ... (2,3,4) :: Array D (Int, Int, Int) (Int, Int, Int)

--
-- >>> (1,1,1) ... (2,3,4) :: Array D Ix3T Ix3T

--
-- >>> (1, 2, 3, 4, 5) :: Ix5T


-- | Problems with Tuples:
-- * lazy
-- * too polymorphic
-- * don't scale to an arbitrary dimension:



--
-- >>> (4 :. 5)

--
-- >>> (1 :> 2 :> 3 :> 4 :. 5)

--
-- >>> 5

--
-- >>> 5 :: Ix1

--
-- >>> Ix1 5

--
-- >>> (1 :> 1 :. 1) ... (2 :> 3 :. 4)


-- |
--
-- data Ix2 = Int :. Int
--
-- infixr 5 :.
--
-- data IxN (n :: Nat)
--   = Int :> Ix (n - 1)
--
-- infixr 5 :>
--
-- type family Ix (n :: Nat) = r | r -> n where
--   Ix 0 = Ix0
--   Ix 1 = Ix1
--   Ix 2 = Ix2
--   Ix n = IxN n
--


--
-- >>> 1 :> 10 :. 20 * 2 + 7

--
-- >>> (1 :> 10 :. 20) * 2 + 7



-- | Many functions that operate on indices of any dimension:

--
-- >>> :t foldlIndex

--
-- >>> foldlIndex (flip (:)) [] (2 :> 3 :> 4 :. 5)


-- >>> zeroIndex :: Ix3
-- >>> oneIndex :: Ix3
-- >>> pureIndex 4 :: Ix3

--
-- >>> :t iter

--
-- >>> iter zeroIndex (3 :> 2 :. 1) oneIndex (<) [] (:)


-- | Reducing dimension by one:

--
-- >>> unconsDim (3 :> 2 :. 1)

--
-- >>> unsnocDim (3 :> 2 :. 1)

-- | Working with specific dimensions:
--
-- * Order of dimensions:

--
-- >>> getDim' (5 :> 4 :> 3 :> 2 :. 1) 4

-- * Partial functions:

--
-- >>> getDim' (5 :> 4 :> 3 :> 2 :. 1) 6

-- * Safer alternatives:

--
-- >>> getDimM (5 :> 4 :> 3 :> 2 :. 1) 6 :: Either SomeException Int

-- * Even safer alternatives:

--
-- >>> getDimension (5 :> 4 :> 3 :> 2 :. 1) Dim4
-- 4

--
-- >>> :set -XDataKinds
-- >>> getDimension (5 :> 4 :> 3 :> 2 :. 1) (DimN :: Dimension 6)



----------
-- Size --
----------

--
-- >>> Sz (3 :> 2 :. 1)

-- * Promote type safety

--
-- >>> :t makeArrayR

--
-- >>> makeArrayR D Seq (2 :. 3) $ \ (i :. j) -> i + j

-- * Prevent easy mistakes of mixing index and size

--
-- >>> Sz (3 :> 2 :. 1) + (1 :> 2 :. 3)

--
-- * Enforce valid size

--
-- >>> Sz (-3 :> 2 :. 1)

-- * Helper pattern synonyms

--
-- >>> Sz3 3 2 1

--
-- >>> Sz4 4 3 2 1 == Sz (4 :> 3 :> 2 :. 1)

------------
-- Stride --
------------

--
-- >>> Ix5 7 4 4 0 0 ... 10
-- >>> computeWithStrideAs P (Stride 2) (Ix1 0 ... 10)


-- | Stride is similar to Sz, just a special index

--
-- >>> :t Stride

-- >>> Stride (3 :> 0 :. -2)
-- Stride (3 :> 1 :. 1)

