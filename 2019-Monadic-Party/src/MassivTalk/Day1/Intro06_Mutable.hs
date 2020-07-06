module MassivTalk.Day1.Intro06_Mutable where


import Control.Monad.ST
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Prelude as P


--
-- class Manifest r ix e => Mutable r ix e where
--   data family MArray s r ix e
--   msize :: MArray s r ix e -> Sz ix
--   unsafeThaw :: PrimMonad m => Array r ix e -> m (MArray (PrimState m) r ix e)


exampleArray :: Array P Ix2 Int
exampleArray = makeArray Seq sz (toLinearIndex sz)
  where
    sz = Sz2 2 3

--
-- >>> print exampleArray



-- | Swap two last elements in IO monad

--
-- >>> marr <- A.thaw exampleArray
-- >>> Just x1 <- A.read marr (1 :. 1)
-- >>> Just x2 <- A.read marr (1 :. 2)
-- >>> A.write marr (1 :. 1) x2
-- >>> A.write marr (1 :. 2) x1
-- >>> newArray <- A.freeze Seq marr
-- >>> print newArray
-- >>> print exampleArray
-- >>> A.swap marr (0 :. 0) (0 :. 1)
-- >>> newArray' <- A.freeze Seq marr
-- >>> print newArray'
-- >>> print newArray



-- | Multiple problems:
--
-- * An extraneous intermediate array is created
-- * Self-contained mutability is pure

--
-- >>> let newArray = withMArrayST exampleArray $ \ marr -> A.swap marr (0 :. 0) (0 :. 1)
-- >>> print newArray
-- >>> print exampleArray

-- >>> import Control.Monad.ST
-- >>> :t withMArrayST
-- withMArrayST
--   :: Mutable r ix e =>
--      Array r ix e
--      -> (forall s. MArray s r ix e -> ST s a) -> Array r ix e

-- | Partial synonyms

--
-- >>> withMArrayST exampleArray $ \ marr -> A.swap' marr (10 :. 0) (0 :. 1)
-- Array P *** Exception: IndexOutOfBoundsException: (10 :. 0) not safe for (Sz (2 :. 3))


-- | Dangerous stuff

--
-- >>> import Data.Massiv.Array.Unsafe as A
-- >>> print exampleArray
-- >>> marr <- A.unsafeThaw exampleArray
-- >>> write' marr (0 :. 0) 100
-- >>> newArray <- A.freeze Seq marr
-- >>> print newArray
-- >>> print exampleArray




-- | Couple of more examples

--
-- >>> :set -XTypeApplications
-- >>> marr <- initializeNew @P @_ @Int (Just 5) (Sz (2 :. 3))
-- >>> A.forM_ (0 :. 0 ... 0 :. 2) $ \ ix -> write' marr ix 1
-- >>> A.freeze Seq marr
-- Array P Seq (Sz (2 :. 3))
--   [ [ 1, 1, 1 ]
--   , [ 5, 5, 5 ]
--   ]


-- | How can we use mutation to implement computation.

computeArray ::
     (Source r2 ix e, Mutable r1 ix e, PrimMonad m)
  => Array r2 ix e
  -> m (Array r1 ix e)
computeArray arr = do
  let k = totalElem (size arr)
  marr <- unsafeNew (size arr)
  loopM_ 0 (< k) (+ 1) $ \i ->
    unsafeLinearWrite marr i (unsafeLinearIndex arr i)
  unsafeFreeze (getComp arr) marr


-- >>> :t loopM_
-- loopM_
--   :: Monad m =>
--      Int -> (Int -> Bool) -> (Int -> Int) -> (Int -> m a) -> m ()



-- | We can hide all of the effectful memory allocation, mutation and threads scheduling
-- as a safe and pure computation.

computeArrayPure ::
     (Mutable r1 ix e, Source r2 ix e) => Array r2 ix e -> Array r1 ix e
computeArrayPure arr = runST (computeArray arr)


--
-- >>> computeArrayPure (1 ... 5) :: Array P Ix1 Int
