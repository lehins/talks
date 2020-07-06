module MassivTalk.Integral where

import Data.Foldable as F
import Data.Massiv.Array as A
import Data.Massiv.Array.Numeric.Integral
import Data.Massiv.Array.Unsafe
import Prelude as P

-- StackOverflow questions:
-- https://stackoverflow.com/questions/56332713/how-do-i-add-parallel-computation-to-this-example
-- https://stackoverflow.com/questions/56395599/how-to-correctly-add-the-runge-error-estimation-rule-to-this-example


-- | Overview of integral approximation with trapezoid rule

-- >>> import Data.Massiv.Array.IO
-- >>> displayImageFile defaultViewer "files/TrapezoidRule.png"


-- | Built-in solution:


integrateTrapezoid :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateTrapezoid n f a b =
  trapezoidRule Seq P (\scale x -> f (scale x)) a (b - a) (Sz1 1) n ! 0

-- >>> integrateTrapezoid 1024 (\x -> x * x) 10 20

-- Normal use cases is a lot more complex:
-- >>> let f x y = exp (- (x ** 2 + y ** 2)) :: Float
-- >>> trapezoidRule Seq P (\scale (i :. j) -> f (scale i) (scale j)) (-2) 1 (Sz2 4 4) 100

-- | Implementation from the SO question using lists

integrate :: Int -> (Double -> Double) -> Double -> Double -> Double
integrate n f a b =
  let step     = (b - a) / fromIntegral n
      segments = fmap (\ x -> a + fromIntegral x * step) [0 .. n-1]
      area x   = step * (f x + f (x + step)) / 2
  in P.sum $ fmap area segments

-- Sanity check:
--
-- >>> integrate 1 (\x -> x) 0 10 == 10 * 10 / 2


-- Something more complicated, how about a parabola:
--
-- >>> integrate 1024 (\x -> x * x) 10 20


-- Exact solution:
--
-- >>> let f x = x ** 3 / 3 :: Double
-- >>> f 20 - f 10



-- | Direct naive translation into massiv arrays

integrateNaive :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNaive n f a b =
  let step     = (b - a) / fromIntegral n
      segments = fmap (\x -> a + fromIntegral x * step) (0 ... n - 1)
      area x   = step * (f x + f (x + step)) / 2
  in P.sum $ fmap area segments

-- range sugar:
-- >>> Ix1 0 ... 10

-- Which is a synonym for
-- >>> rangeInclusive Seq (Ix1 0) 10

-- range exclusive or simply `range`:
-- >>> Ix1 0 ..: 10

-- Which is a synonym for
-- >>> range Seq (Ix1 0) 10


-- >>> integrateNaive 1024 (\x -> x * x) 10 20



-- | What is needed for parallelization:

integrateNaivePar :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNaivePar n f a b =
  let step     = (b - a) / fromIntegral n
      segments = fmap (\x -> a + fromIntegral x * step) (range Par 0 n)
      area x   = step * (f x + f (x + step)) / 2
  in P.sum $ fmap area segments


-- >>> integrateNaivePar 1024 (\x -> x * x) 10 20

-- Checkout benchmarks:
-- :! stack bench :integral --ba '--match prefix Naive'


-- Simplify a bit first
-- >>> A.map (\x -> 10 + fromIntegral x * 0.1) (range Seq 0 5) :: Array D Ix1 Double

-- >>> enumFromStepN Seq 10 0.1 5 :: Array D Ix1 Double




integrateNoDuplicateList :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoDuplicateList n f a b =
  let step       = (b - a) / fromIntegral n
      ys         = [f (a + fromIntegral x * step) | x <- [0 .. n]]
      area y0 y1 = step * (y0 + y1) / 2
  in P.sum $ P.zipWith area ys (tail ys)

-- >>> integrateNoDuplicateList 1024 (\x -> x * x) 10 20


-- | First optimization. Avoid duplicate calls to `f`

-- For simplicity we'll use n = 10

-- >>> n = 10 :: Int
-- >>> (a, b) = (10, 20) :: (Double, Double)
-- >>> step = (b - a) / fromIntegral n
-- >>> f = (^ (2 :: Int))
-- >>> segments = f <$> enumFromStepN Seq a step (Sz n + 1) :: Array D Ix1 Double
-- >>> segments


integrateNoDuplicateBad :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoDuplicateBad n f a b =
  let step = (b - a) / fromIntegral n
      sz = size segments - 1
      -- this is still a delayed array
      segments = fmap f (enumFromStepN Seq a step (Sz (n + 1)))
      area y0 y1 = step * (y0 + y1) / 2
      areas = A.zipWith area segments (extract' 1 sz segments)
   in P.sum areas

-- >>> integrateNoDuplicateBad 1024 (\x -> x * x) 10 20

integrateNoDuplicate :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoDuplicate n f a b =
  let step = (b - a) / fromIntegral n
      sz = size segments - 1
      segments = computeAs P $ fmap f (enumFromStepN Seq a step (Sz (n + 1)))
      area y0 y1 = step * (y0 + y1) / 2
      areas = A.zipWith area segments (extract' 1 sz segments)
   in A.sum areas

-- >>> integrateNoDuplicate 1024 (\x -> x * x) 10 20

-- check the benchmarks:
-- :! stack bench :integral --ba '--match prefix NoDuplicate/Seq'





-- Fixed and parallelized:


integrateNoDuplicatePar :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoDuplicatePar n f a b =
  let step = (b - a) / fromIntegral n
      sz = size segments - 1
      segments = computeAs P $ fmap f (enumFromStepN Par a step (Sz (n + 1)))
      area y0 y1 = step * (y0 + y1) / 2
      areas = A.zipWith area (extract' 0 sz segments) (extract' 1 sz segments)
   in A.sum areas

-- >>> integrateNoDuplicatePar 1024 (\x -> x * x) 10 20



-- :! stack bench :integral --ba '--match prefix NoDuplicate'


-- | Moral:
-- * Parallelization is easy (most of the time)
-- * Important to identify duplication of work. Easy solution is to `compute`.


-- | Can we avoid allocation completely? Yes we can! Even with lists:


integrateNoAllocateList :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoAllocateList n f a b =
  let step = (b - a) / fromIntegral n
      segments = fmap (\x -> f (a + fromIntegral x * step)) [1 .. n]
      area y0 y1 = step * (y0 + y1) / 2
      sumWith (acc, y0) y1 =
        let acc' = acc + area y0 y1
         in acc' `seq` (acc', y1)
   in fst $ F.foldl' sumWith (0, f a) segments

-- >>> integrateNoAllocateList 1024 (\x -> x * x) 10 20



integrateNoAllocate :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoAllocate n f a b =
  let step = (b - a) / fromIntegral n
      segments = A.map f (enumFromStepN Seq (a + step) step (Sz n))
      area y0 y1 = step * (y0 + y1) / 2
      sumWith (acc, y0) y1 =
        let acc' = acc + area y0 y1
         in acc' `seq` (acc', y1) -- folds are strict, but only to WHNF
   in fst $ A.foldlS sumWith (0, f a) segments


-- :! stack bench :integral --ba '--match prefix No'
-- Waaat?

-- Can we do even better?
-- Not too straighforward, but there is a bit of uglyness we can come up with.

-- | Parallelization for 8 capabilities (quad core with hyperthreading). If `n` is not
-- divisble by 8, this function will return bogus.
integrateNoAllocateN8 :: Int -> (Double -> Double) -> Double -> Double -> Double
integrateNoAllocateN8 n f a b =
  let k = 8
      n' = n `div` k
      step = (b - a) / fromIntegral n
      segments =
        makeArrayR D (ParN (fromIntegral k)) (Sz1 k) $ \i ->
          let start = a + step * fromIntegral n' * fromIntegral i + step
           in (f start, A.map f (enumFromStepN Seq (start + step) step (Sz (n' - 1))))
      area y0 y1 = step * (y0 + y1) / 2
      sumWith (acc, y0) y1 =
        let acc' = acc + area y0 y1
         in acc' `seq` (acc', y1)
      partialResults =
        computeAs U $ A.map (\(y0, arr) -> (y0, A.foldlS sumWith (0, y0) arr)) segments
      combine (acc, y0) (y1, (acci, yn)) =
        let acc' = acc + acci + area y0 y1
         in acc' `seq` (acc', yn)
   in fst $ foldlS combine (0, f a) partialResults

-- >>> f x = x * x :: Double
-- >>> k = 4 :: Int
-- >>> n = 16 :: Int
-- >>> n' = n `div` k
-- >>> (a, b) = (10, 20) :: (Double, Double)
-- >>> step = (b - a) / fromIntegral n
-- >>> mkStart i = a + step * fromIntegral n' * fromIntegral i + step
-- >>> mkArr start = A.map f (enumFromStepN Seq (start + step) step (Sz (n' - 1)))
-- >>> mkSubArr i = let start = mkStart i in (f start, mkArr start)
-- >>> comp = ParN (fromIntegral k)
-- >>> segments = makeArrayR D comp (Sz1 k) mkSubArr
-- >>> A.mapM_ print segments
-- >>> A.map f (enumFromStepN Seq a step (Sz n + 1))



----------------
-- Runge Rule --
----------------


-- >>> import Data.Massiv.Array.IO
-- >>> displayImageFile defaultViewer "files/RungeRule.png"


-- | Returns estimated integral up to a precision, or value estimated at max
-- number of steps
rungeRule ::
     Int -- ^ Maximum number of steps as an upper bound, to prevent unbounded computation
  -> Double -- ^ ε -- precision
  -> Int -- ^ Starting value of @n@
  -> Double -- ^ Θ -- ^ Either 1/3 for trapezoidal and midpoint or 1/15 for Simpson's
  -> (Int -> Double) -- ^ Integral estimator
  -> Either Double (Int, Double)
rungeRule nMax epsilon n0 theta integralEstimator =
  go (integralEstimator n0) (2 * n0)
  where
    go prevEstimate n
      | n >= nMax = Left prevEstimate
      | theta * abs (curEstimate - prevEstimate) < epsilon =
        Right (n, curEstimate)
      | otherwise = go curEstimate (2 * n)
      where
        curEstimate = integralEstimator n

trapezoidalRunge ::
     Double -- ^ ε -- precision
  -> (Double -> Double) -- ^ f(x) - function to integrate
  -> Double -- ^ a - from
  -> Double -- ^ b - to
  -> Either Double (Int, Double)
trapezoidalRunge epsilon f a b =
  rungeRule 131072 epsilon 2 (1 / 3) (\n -> integrateNoAllocate n f a b)


-- >>> trapezoidalRunge 0.0005 (\x -> x * x) 10 20

-- Memoized version


-- Helper
trapezoidalMemoized ::
     Int
  -> Array P Ix1 Double
  -> (Double -> Double)
  -> Double
  -> Double
  -> (Double, Array P Ix1 Double)
trapezoidalMemoized n prevSegments f a b =
  let step = (b - a) / fromIntegral n
      sz = size segments - 1
      curSegments =
        fmap f (enumFromStepN Seq (a + step) (2 * step) (Sz (n `div` 2)))
      segments =
        computeAs P $
        makeLoadArrayS (Sz (n + 1)) 0 $ \w -> do
          A.iforM_ prevSegments $ \i e -> w (i * 2) e
          A.iforM_ curSegments $ \i e -> w (i * 2 + 1) e
      area y0 y1 = step * (y0 + y1) / 2
      areas = A.zipWith area segments (extract' 1 sz segments)
   in (A.sum areas, segments)


trapezoidalRungeMemo ::
     Double -- ^ ε -- precision
  -> (Double -> Double) -- ^ f(x) - function to integrate
  -> Double -- ^ a - from
  -> Double -- ^ b - to
  -> Either Double (Int, Double)
trapezoidalRungeMemo epsilon f a b = go initEstimate initSegments 4
  where
    (initEstimate, initSegments) =
      trapezoidalMemoized 2 (A.fromList Seq [f a, f b]) f a b
    nMax = 131072 -- 2 ^ 17
    theta = 1 / 3
    go prevEstimate prevSegments n
      | n >= nMax = Left prevEstimate
      | theta * abs (curEstimate - prevEstimate) < epsilon =
        Right (n, curEstimate)
      | otherwise = go curEstimate curSegments (2 * n)
      where
        (curEstimate, curSegments) =
          trapezoidalMemoized n prevSegments f a b

-- >>> trapezoidalRungeMemo 0.0005 (\x -> x * x) 10 20


trapezoidalMemoizedPar ::
     Int
  -> Array P Ix1 Double
  -> (Double -> Double)
  -> Double
  -> Double
  -> (Double, Array P Ix1 Double)
trapezoidalMemoizedPar n prevSegments f a b =
  let step = (b - a) / fromIntegral n
      sz = size segments - 1
      curSegments =
        fmap f (enumFromStepN Seq (a + step) (2 * step) (Sz (n `div` 2)))
      segments =
        computeAs P $
        unsafeMakeLoadArray Par (Sz (n + 1)) Nothing $ \scheduler _ w -> do
          splitLinearlyWith_
            scheduler
            (unSz (size prevSegments))
            (unsafeLinearIndex prevSegments) $ \i e -> w (i * 2) e
          splitLinearlyWith_
            scheduler
            (unSz (size curSegments))
            (unsafeLinearIndex curSegments) $ \i e -> w (i * 2 + 1) e
      area y0 y1 = step * (y0 + y1) / 2
      areas = A.zipWith area segments (extract' 1 sz segments)
   in (A.sum areas, segments)

-- >>> trapezoidalRungeMemoPar 0.0005 (\x -> x * x) 10 20

trapezoidalRungeMemoPar ::
     Double -- ^ ε -- precision
  -> (Double -> Double) -- ^ f(x) - function to integrate
  -> Double -- ^ a - from
  -> Double -- ^ b - to
  -> Either Double (Int, Double)
trapezoidalRungeMemoPar epsilon f a b = go initEstimate initSegments 4
  where
    (initEstimate, initSegments) =
      trapezoidalMemoizedPar 2 (A.fromList Seq [f a, f b]) f a b
    nMax = 131072 -- 2 ^ 17
    theta = 1 / 3
    go prevEstimate prevSegments n
      | n >= nMax = Left prevEstimate
      | theta * abs (curEstimate - prevEstimate) < epsilon =
        Right (n, curEstimate)
      | otherwise = go curEstimate curSegments (2 * n)
      where
        (curEstimate, curSegments) =
          trapezoidalMemoizedPar n prevSegments f a b

-- >>> trapezoidalRungeMemoPar 0.0005 (\x -> x * x) 10 20
