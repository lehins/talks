{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MassivTalk.Day1.Intro08_Random where

import Control.Monad.ST (ST, runST)
import Control.Scheduler
import Data.Massiv.Array as A
import Prelude as P
import System.Random as System
import System.Random.MWC as MWC
import System.Random.PCG.Pure as PCG

randomArrayPureDL :: Index ix => g -> (g -> (Double, g)) -> Sz ix -> Array DL ix Double
randomArrayPureDL gen nextRandom sz = unfoldrS_ Seq sz nextRandom gen
{-# INLINE randomArrayPureDL #-}

--
-- >>> gen = System.mkStdGen 217
-- >>> randomArrayPureDL gen System.random (Sz2 2 3)


-- >>> gen <- System.newStdGen

--
-- >>> gen = System.mkStdGen 217
-- >>> randomArray gen System.split System.random Seq (Sz2 2 3) :: Array DL Ix2 Double


randomArrayPureSeq ::
  Index ix => g -> (g -> (Double, g)) -> Sz ix -> Array DL ix Double
randomArrayPureSeq gen nextRandom = randomArray gen (\g -> (g, g)) nextRandom Seq
{-# INLINE randomArrayPureSeq #-}

--
-- >>> gen = System.mkStdGen 217
-- >>> randomArrayPureSeq gen System.random (Sz2 2 3)


randomArrayPurePar ::
     Index ix => g -> (g -> (g, g)) -> (g -> (Double, g)) -> Sz ix -> Array DL ix Double
randomArrayPurePar gen splitGen nextRandom = randomArray gen splitGen nextRandom Par
{-# INLINE randomArrayPurePar #-}

--
-- >>> gen = System.mkStdGen 217
-- >>> randomArrayPurePar gen System.split System.random (Sz2 2 3)


-- Mutable

--
-- >>> gen <- MWC.createSystemRandom
-- >>> generateArray Par (Sz2 2 3) (const (MWC.uniform gen)) :: IO (Array P Ix2 Double)

randomArrayPureST ::
     Index ix => g -> (g -> (Double, g)) -> Sz ix -> (g, Array P ix Double)
randomArrayPureST gen nextRandom sz = runST $ unfoldrPrimM Seq sz (pure . nextRandom) gen
{-# INLINE randomArrayPureST #-}

-- >>> gen = System.mkStdGen 217
-- >>> snd $ randomArrayPureST gen System.random (Sz2 2 3)

randomArrayMWC ::
     Index ix
  => MWC.Seed
  -> (forall s. MWC.Gen s -> ST s Double)
  -> Sz ix
  -> (MWC.Seed, Array P ix Double)
randomArrayMWC mwcSeed mkRandom sz = runST $ do
  gen <- MWC.restore mwcSeed
  arr <- generateArrayS Seq sz (const (mkRandom gen))
  mwcSeed' <- MWC.save gen
  pure (mwcSeed', arr)
{-# INLINE randomArrayMWC #-}

-- >>> mwcSeed <- MWC.save =<< MWC.create
-- >>> snd $ randomArrayMWC mwcSeed MWC.uniform (Sz2 2 3)

randomArrayPCG ::
     Index ix
  => PCG.FrozenGen
  -> (forall s. PCG.Gen s -> ST s Double)
  -> Sz ix
  -> (PCG.FrozenGen, Array P ix Double)
randomArrayPCG pcgSeed mkRandom sz = runST $ do
  gen <- PCG.restore pcgSeed
  arr <- generateArrayS Seq sz (const (mkRandom gen))
  pcgSeed' <- PCG.save gen
  pure (pcgSeed', arr)
{-# INLINE randomArrayPCG #-}

-- >>> pcgSeed <- PCG.save =<< PCG.create
-- >>> snd $ randomArrayPCG pcgSeed PCG.uniform (Sz2 2 3)


randomArrayIO ::
     Index ix
  => WorkerStates g
  -> Sz ix
  -> (g -> IO Double)
  -> IO (Array S ix Double)
randomArrayIO = randomArrayWS
{-# INLINE randomArrayIO #-}

-- >>> pcgWorkerState <- initWorkerStates (ParN 2) (\ _ -> PCG.create)
-- >>> randomArrayIO pcgWorkerState (Sz2 2 3) PCG.uniform

---------------------------------
-- Benchmark Random Generators --
---------------------------------

