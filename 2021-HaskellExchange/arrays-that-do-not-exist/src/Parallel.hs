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
module Parallel where

import Data.Function
import qualified Streamly.Prelude as Streamly
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.ST
import Control.Scheduler
import Data.IORef
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import System.IO.Unsafe
import UnliftIO.Async (pooledForConcurrently_)


makeMatrixS :: Manifest r a => Sz2 -> (Ix2 -> a) -> Matrix r a
makeMatrixS sz@(Sz2 m n) f =
  runST $ do
    mutArr <- unsafeNew sz
    loopM_ 0 (< m) (+ 1) $ \i ->
      loopM_ 0 (< n) (+ 1) $ \j ->
        unsafeWrite mutArr (i :. j) $ f (i :. j)
    unsafeFreeze Seq mutArr
{-# INLINE makeMatrixS #-}


makeMatrixWithFork :: Manifest r a => Sz2 -> (Ix2 -> a) -> Matrix r a
makeMatrixWithFork sz@(Sz2 m n) f =
  unsafePerformIO $ do
    mutArr <- unsafeNew sz
    when (m > 0) $ do
      mRef <- newIORef m
      lock <- newEmptyMVar
      loopM_ 0 (< m) (+ 1) $ \i ->
        forkIO $ do
          loopM_ 0 (< n) (+ 1) $ \j ->
            unsafeWrite mutArr (i :. j) $ f (i :. j)
          done <- atomicModifyIORef' mRef (\c -> (c - 1, c == 1))
          when done $ putMVar lock ()
      readMVar lock
    unsafeFreeze Seq mutArr
{-# INLINE makeMatrixWithFork #-}

makeMatrixWithAsync :: Manifest r a => Sz2 -> (Ix2 -> a) -> Matrix r a
makeMatrixWithAsync sz@(Sz2 m n) f =
  unsafePerformIO $ do
    mutArr <- unsafeNew sz
    forConcurrently_ [0 .. m - 1] $ \i ->
      loopM_ 0 (< n) (+ 1) $ \j ->
        unsafeWrite mutArr (i :. j) $ f (i :. j)
    unsafeFreeze Seq mutArr
{-# INLINE makeMatrixWithAsync #-}


makeMatrixWithPooled :: Manifest r a => Sz2 -> (Ix2 -> a) -> Matrix r a
makeMatrixWithPooled sz@(Sz2 m n) f =
  unsafePerformIO $ do
    mutArr <- unsafeNew sz
    pooledForConcurrently_ [0 .. m - 1] $ \i ->
      loopM_ 0 (< n) (+ 1) $ \j ->
        unsafeWrite mutArr (i :. j) $ f (i :. j)
    unsafeFreeze Seq mutArr
{-# INLINE makeMatrixWithPooled #-}

makeMatrixWithStreamly :: Manifest r a => Sz2 -> (Ix2 -> a) -> Matrix r a
makeMatrixWithStreamly sz@(Sz2 m n) f =
  unsafePerformIO $ do
    mutArr <- unsafeNew sz
    let fillRow !i =
          liftIO $ loopM_ 0 (< n) (+ 1) $ \j ->
            unsafeWrite mutArr (i :. j) $ f (i :. j)
        {-# INLINE fillRow #-}
    Streamly.drain $ Streamly.fromParallel $
      Streamly.enumerateFromTo 0 (m - 1) & Streamly.mapM fillRow
    unsafeFreeze Seq mutArr
{-# INLINE makeMatrixWithStreamly #-}


makeMatrix :: Manifest r a => Comp -> Sz Ix2 -> (Ix2 -> a) -> Matrix r a
makeMatrix comp sz@(Sz2 m n) f =
  unsafePerformIO $ do
    mutArr <- unsafeNew sz
    withScheduler_ comp $ \scheduler ->
      loopM_ 0 (< m) (+ 1) $ \i ->
        scheduleWork scheduler $
          loopM_ 0 (< n) (+ 1) $ \j -> unsafeWrite mutArr (i :. j) $ f (i :. j)
    unsafeFreeze comp mutArr
{-# INLINE makeMatrix #-}


-- makeNestedPar :: (Int, Int) -> ((Int, Int) -> e) -> [[e]]
-- makeNestedPar (m, n) f =
--   [ [ let x = f (i, j)
--        in x `par` j `pseq` x
--   | j <- [0 .. n - 1]
--   ]
--   | i <- [0 .. m - 1]
--   ]
-- {-# INLINE makeNestedPar #-}


-- makeMatrixWithForkExc :: Manifest r a => Sz2 -> (Ix2 -> a) -> Matrix r a
-- makeMatrixWithForkExc sz@(Sz2 m n) f =
--   unsafePerformIO $ do
--     mutArr <- unsafeNew sz
--     when (m > 0) $ do
--       mRef <- newIORef m
--       resVar <- newEmptyMVar
--       loopM_ 0 (< m) (+ 1) $ \i -> do
--         let innerLoop =
--               loopM_ 0 (< n) (+ 1) $ \j ->
--                 unsafeWrite mutArr (i :. j) $ f (i :. j)
--             whenDone :: Either SomeException () -> IO ()
--             whenDone = \case
--               Left exc -> void $ tryPutMVar resVar (Just exc)
--               Right () -> do
--                 done <- atomicModifyIORef' mRef (\c -> (c - 1, c == 1))
--                 when done $ putMVar resVar Nothing
--         forkFinally innerLoop whenDone
--       traverse_ throwIO =<< readMVar resVar
--     unsafeFreeze Seq mutArr
-- {-# INLINE makeMatrixWithForkExc #-}

-- makeMatrixWithForkAsyncExc :: Manifest r a => Sz2 -> (Ix2 -> a) -> Matrix r a
-- makeMatrixWithForkAsyncExc sz@(Sz2 m n) f =
--   unsafePerformIO $ do
--     mRef <- newIORef m
--     resVar <- newEmptyMVar
--     when (m == 0) $ putMVar resVar Nothing
--     mutArr <- unsafeNew sz
--     mask $ \restore ->
--       loopM_ 0 (< m) (+ 1) $ \i -> do
--         let innerLoop = do
--               loopM_ 0 (< n) (+ 1) $ \j ->
--                 unsafeWrite mutArr (i :. j) $ f (i :. j)
--               done <- atomicModifyIORef' mRef (\c -> (c - 1, c == 1))
--               when done $ putMVar resVar Nothing
--             onFailure :: SomeException -> IO ()
--             onFailure exc = void $ tryPutMVar resVar (Just exc)
--         forkIO (restore (innerLoop `catch` onFailure))
--     readMVar resVar >>= \case
--       Nothing  -> unsafeFreeze Seq mutArr
--       Just exc -> throwIO exc
-- {-# INLINE makeMatrixWithForkAsyncExc #-}


-- makeMatrixPar' :: Manifest r a => Sz Ix2 -> (Ix2 -> a) -> Matrix r a
-- makeMatrixPar' sz@(Sz2 m n) f =
--   unsafePerformIO $ do
--     let comp = Par
--     mutArr <- unsafeNew sz
--     withScheduler_ comp $ \scheduler -> do
--       let numChunks = numWorkers scheduler * 10
--           chunkLength = m `quot` numChunks
--       loopM_ 0 (< numChunks) (+ 1) $ \k ->
--         let s = k * chunkLength
--          in loopM_ s (< s + chunkLength) (+ 1) $ \i ->
--               scheduleWork_ scheduler $
--               loopM_ 0 (< n) (+ 1) $ \j ->
--                 unsafeWrite mutArr (i :. j) $ f (i :. j)
--     unsafeFreeze comp mutArr
-- {-# INLINE makeMatrixPar' #-}


-- newtype Iterator ix =
--   Iterator
--     { runIterator ::
--         forall s a. Scheduler s ()
--                  -> ix
--                  -> Stride ix
--                  -> Sz ix
--                  -> a
--                  -> (Ix1 -> ix -> a -> ST s a)
--                  -> ST s a
--     }

-- makeMatrixPar' :: Manifest r a => Sz Ix2 -> (Ix2 -> a) -> Matrix r a
-- makeMatrixPar' sz f =
--   unsafePerformIO $ do
--     let comp = Par
--     mutArr <- unsafeNew sz
--     withScheduler_ comp $ \scheduler -> do
--       let totalLength = totalElem sz
--           numChunks = numWorkers scheduler
--           !chunkLength = totalLength `quot` numChunks
--           !slackStart = chunkLength * numChunks
--       loopNextM_ 0 (< slackStart) (+ chunkLength) $ \start end ->
--         scheduleWork_ scheduler $
--         loopM_ start (< end) (+ 1) $ \k ->
--           unsafeLinearWrite mutArr k $ f $ fromLinearIndex sz k
--       when (slackStart < totalLength) $
--         scheduleWork_ scheduler $
--         loopM_ slackStart (< totalLength) (+ 1) $ \k ->
--           unsafeLinearWrite mutArr k $ f $ fromLinearIndex sz k
--     unsafeFreeze comp mutArr
-- {-# INLINE makeMatrixPar' #-}



-- makeMatrixPar' :: Manifest r a => Sz Ix2 -> (Ix2 -> a) -> Matrix r a
-- makeMatrixPar' sz f =
--   unsafePerformIO $ do
--     let comp = Par
--     mutArr <- unsafeNew sz
--     withScheduler_ comp $ \scheduler ->
--       splitLinearlyWith_
--         scheduler
--         (totalElem sz)
--         (f . fromLinearIndex sz)
--         (unsafeLinearWrite mutArr)
--     unsafeFreeze comp mutArr
-- {-# INLINE makeMatrixPar' #-}







-- makeMatrixS' :: Manifest r a => Sz2 -> (Ix2 -> a) -> Matrix r a
-- makeMatrixS' sz f = runST $ do
--   mutArr <- unsafeNew sz
--   iterRowMajorST_ 0 trivialScheduler_ zeroIndex oneIndex sz $ \i ix ->
--     unsafeLinearWrite mutArr i $ f ix
--   -- rowMajorIterator trivialScheduler_ zeroIndex oneIndex sz 0 () $ \i ix () ->
--   --   unsafeLinearWrite mutArr i $ f ix
--   unsafeFreeze Seq mutArr
-- {-# INLINE makeMatrixS' #-}

-- makeMatrix' :: Manifest r a => Comp -> Sz2 -> (Ix2 -> a) -> Matrix r a
-- makeMatrix' comp sz f = unsafePerformIO $ do
--   mutArr <- unsafeNew sz
--   withScheduler_ comp $ \scheduler ->
--     stToIO $ iterRowMajorST_ 0 scheduler zeroIndex oneIndex sz $ \i ix ->
--       unsafeLinearWrite mutArr i $ f ix
--   unsafeFreeze comp mutArr
-- {-# INLINE makeMatrix' #-}


-- makeMatrix'' :: Manifest r a => Comp -> Sz Ix2 -> (Ix2 -> a) -> Matrix r a
-- makeMatrix'' comp sz f =
--   unsafePerformIO $ do
--     mutArr <- unsafeNew sz
--     withScheduler_ comp $ \scheduler ->
--       stToIO $
--       iterST_ (RowMajor 8) 0 scheduler zeroIndex oneIndex sz $ \i ix ->
--         unsafeLinearWrite mutArr i $ f ix
--     unsafeFreeze comp mutArr
-- {-# INLINE makeMatrix'' #-}


-- newtype RowMajor = RowMajor Int

-- class Index ix => Iterator i ix where
--   iterST_ :: i
--           -> Int -- ^ Linear index accumulator
--           -> Scheduler s () -- ^ Scheduler to use
--           -> ix -- ^  Start index
--           -> ix -- ^ Stride
--           -> Sz ix -- ^ Size
--           -> (Ix1 -> ix -> ST s ()) -- ^ Action
--           -> ST s ()
-- instance Iterator RowMajor Int where
--   iterST_ (RowMajor factor) !ixAcc scheduler !start !s (SafeSz n) f
--     | numWorkers scheduler == 1 = loopM_ start (< n) (+ s) $ \i -> f (ixAcc' + i) i
--     | otherwise =
--       splitLinearly nw n $ \chunkLength slackStart -> do
--         loopM_ 0 (< slackStart) (+ chunkLength) $ \chunkStart ->
--           scheduleWork_ scheduler $
--           loopM_ chunkStart (< chunkStart + chunkLength) (+ s) $ \i -> f (ixAcc' + i) i
--         when (slackStart < n) $
--           scheduleWork_ scheduler $ loopM_ slackStart (< n) (+ s) $ \i -> f (ixAcc' + i) i
--     where
--       !ixAcc' = ixAcc * n
--       !nw = numWorkers scheduler * factor
--   {-# INLINE iterST_ #-}
-- instance Iterator RowMajor Ix2 where
--   iterST_ it@(RowMajor factor) !ixAcc scheduler (i2 :. i1) (s2 :. s1) (SafeSz (k2 :. k1)) f
--     | numWorkers scheduler == 1 || k2 - i2 < nw =
--       loopM_ i2 (< k2) (+ s2) $ \j2 -> do
--         scheduleWork_ scheduler $
--           iterST_ it (ixAcc2 + j2) scheduler i1 s1 (SafeSz k1) $ \ixAcc1 j1 ->
--             f ixAcc1 (j2 :. j1)
--     | otherwise = do
--       let !chunkLength = k2 `quot` nw
--           !slackStart = chunkLength * nw
--       loopM_ i2 (< slackStart) (+ chunkLength) $ \chunkStart ->
--         scheduleWork_ scheduler $
--         loopM_ chunkStart (< chunkStart + chunkLength) (+ s2) $ \j2 ->
--           iterST_ it (ixAcc2 + j2) trivialScheduler_ i1 s1 (SafeSz k1) $ \ixAcc1 j1 ->
--             f ixAcc1 (j2 :. j1)
--       when (slackStart < k2) $
--         scheduleWork scheduler $
--         loopM_ slackStart (< k2) (+ s2) $ \j2 ->
--           iterST_ it (ixAcc2 + j2) trivialScheduler_ i1 s1 (SafeSz k1) $ \ixAcc1 j1 ->
--             f ixAcc1 (j2 :. j1)
--     where
--       !ixAcc2 = ixAcc * k2
--       !nw = numWorkers scheduler * factor
--   {-# INLINE iterST_ #-}


-- newtype Linear = Linear

-- instance Iterator Linear Ix2 where
--   iterST_ _ !ixAcc scheduler (i2 :. i1) (s2 :. s1) sz f =
--     splitLinearlyM_ scheduler (totalElems sz)
--   splitLinearly (numWorkers scheduler) totalLength $ \chunkLength slackStart -> do
--     loopM_ startAt (< (slackStart + startAt)) (+ chunkLength) $ \ !start ->
--       scheduleWork_ scheduler $
--       loopM_ start (< (start + chunkLength)) (+ 1) $ \ !k -> make k >>= write k
--     when (slackStart < totalLength) $
--       scheduleWork_ scheduler $
--         loopM_ (slackStart + startAt) (< (totalLength + startAt)) (+ 1) $ \ !k -> make k >>= write k
--   {-# INLINE iterST_ #-}



-- --makeVectorBoxedPar :: NFData a => Sz2 -> (Ix2 -> a) -> V.Vector a
-- makeVectorBoxedPar :: V.Prim a => Sz2 -> (Ix2 -> a) -> V.Vector a
-- makeVectorBoxedPar sz@(Sz2 m n) f =
--   -- runEval $
--   -- parVector m $
--   runST $ do
--     mutArr <- VM.unsafeNew (totalElem sz)
--     let goN !k !i !j =
--           when (j < n) $ do
--             VM.unsafeWrite mutArr k $ f (i :. j)
--             goN (k + 1) i (j + 1)
--         goM !k !i =
--           when (i < m) $ do
--             goN k i 0
--             goM (k + n) (i + 1)
--     goM 0 0
--     V.unsafeFreeze mutArr
-- {-# INLINE makeVectorBoxedPar #-}
