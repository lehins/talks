{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BinaryLiterals #-}
module MassivTalk.Day1.Intro07_Computation where

import Prelude as P
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Control.Scheduler
import Graphics.ColorSpace
import Control.Concurrent
import Data.Bits

-- | Computation Startegies:
--
-- * Seq
-- * Par
-- * Par'
-- * ParOn
-- * ParN



scheduleSums :: IO [Int]
scheduleSums =
  withScheduler (ParN 2) $ \ scheduler -> do
    scheduleWork scheduler $ pure (10 + 1)
    scheduleWork scheduler $ pure (20 + 2)
    scheduleWork scheduler $ pure (30 + 3)
    scheduleWork scheduler $ pure (40 + 4)
    scheduleWork scheduler $ pure (50 + 5)

--
-- >>> scheduleSums

--
-- >>> :t withScheduler

-- >>> :t scheduleWork


computeArrayIO :: (Source r ix e, Mutable r' ix e) => Array r ix e -> IO (Array r' ix e)
computeArrayIO arr = do
  marr <- new (size arr)
  withScheduler_ (getComp arr) $ \scheduler -> do
    let k = totalElem (size arr)
        w = numWorkers scheduler
        (chunkLength, slack) = k `quotRem` w
        slackStart = k - slack
        writeComputed i = unsafeLinearWrite marr i (unsafeLinearIndex arr i)
    loopM_ 0 (< slackStart) (+ chunkLength) $ \chunkStart ->
      scheduleWork_ scheduler $
      loopM_ chunkStart (< chunkStart + chunkLength) (+ 1) writeComputed
    scheduleWork scheduler $
      loopM_ slackStart (< k) (+ 1) writeComputed
  freeze (getComp arr) marr



paintThreads ::
     (Load r Ix2 Int)
  => Array r Ix2 Int
  -> IO (Array S Ix2 (Pixel RGB Word8))
paintThreads arr = do
  mArr <- A.new (size arr)
  withScheduler_ (getComp arr) $ \ scheduler ->
    loadArrayM scheduler arr $ \ i _ -> do
      (cInt, _) <- threadCapability =<< myThreadId
      let c = fromIntegral cInt
      unsafeLinearWrite mArr i (255 * PixelRGB (c .&. 0b100) (c .&. 0b010) (c .&. 0b001))
  A.freeze Seq mArr
