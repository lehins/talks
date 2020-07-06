{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE FlexibleContexts #-}
module MassivTalk.Day1.Intro04_Slicing where

import Prelude as P
import Data.Massiv.Array as A


---------------------
-- Class Hierarchy --
---------------------

-- class Index ix => Resize r ix where

-- class Load r ix e => Extract r ix e where


-- class Load r ix e => OuterSlice r ix e where

-- class Load r ix e => InnerSlice r ix e where

-- class Load r ix e => Slice r ix e where



sampleArr = computeAs U $ fromIx3 <$> (0 :> 0 :. 0 ... 4 :> 2 :. 6)

sampleArr' = computeAs U (fmap fromIx3 (rangeInclusive Seq (0 :> 0 :. 0) (4 :> 2 :. 6)))


--
-- >>> sampleArr
-- >>> sampleArr == sampleArr'

-- | Slice from both sides and from within

--
-- >>> sampleArr !> 1

--
-- >>> sampleArr <! 0

--
-- >>> sampleArr <!> (2, 1)

-- | Slice chaining

--
-- >>> sampleArr !> 1 !> 2

--
-- >>> sampleArr !> 1 !> 2 !> 0

-- | Safe slicing

--
-- >>> :t (!>)

--
-- >>> sampleArr !> 10

-- >>> :t (!?>)
-- >>> :t (??>)

--
-- >>> sampleArr !?> 10

--
-- >>> import Data.Maybe
-- >>> fromMaybe A.empty $ sampleArr !?> 10


--
-- >>> x <- sampleArr !?> 1 ??> 2 ??> 10
-- >>> print x

--
-- >>> let x = sampleArr !> 1 !> 2 !> 10
-- >>> print x


{-
ocut ::
     (MonadThrow m, OuterSlice r ix e) => Int -> Array r ix e -> m (Elt r ix e)
ocut = flip (!?>)
λ> sampleArr !?> 1 >>= ocut 2 >>= ocut 0
(1,2,0)
-}


-- | Matrix multiplication

multArrays arr1 arr2
  | n1 == m2 =
    makeArray (getComp arr1 <> getComp arr2) (Sz2 m1 n2) $ \(i :. j) ->
      dot (arr1 !> i) (arr2 <! j)
  | otherwise = error "multArrays: Inner size mismatch"
  where
    Sz2 m1 n1 = size arr1
    Sz2 m2 n2 = size arr2


dot v1 v2 = A.sum $ A.zipWith (*) v1 v2


--
-- >>> a1 = makeArrayR P Seq (Sz2 2 3) $ \ (i :. j) -> i + j
-- >>> a2 = makeArrayR P Seq (Sz2 3 2) $ \ (i :. j) -> i * j + 1
-- >>> multArrays a1 a2 :: Array P Ix2 Int
-- >>> a1 |*| a2 :: Array P Ix2 Int



-- | Extracting

--
-- >>> sampleArr

--
-- >>> :t extractM

--
-- >>> extractM (1 :> 0 :. 1) (Sz3 3 2 5) sampleArr


-- | Extract and lower the dimensionality

--
-- >>> extractM (1 :> 0 :. 1) (Sz3 3 1 5) sampleArr

--
-- >>> subArr <- extractM (1 :> 0 :. 1) (Sz3 3 1 5) sampleArr
-- >>> resizeM (Sz2 3 5) subArr

