{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Scheduler
import Criterion.Main
import Data.Massiv.Array
import MassivTalk.Day1.Intro01_Representations
import MassivTalk.Day1.Intro08_Random
import System.Random as System
import System.Random.Mersenne.Pure64 as MT
import System.Random.MWC as MWC
import System.Random.PCG.Pure as PCG
import System.Random.SplitMix as SplitMix
import System.Random.TF as TF


main :: IO ()
main = do
  let !k = 500000
      !n = 5000
      !sz = Sz1 1048576
  sysGen <- System.getStdGen
  smGen <- SplitMix.newSMGen
  tfGen <- TF.newTFGen
  mwcGen <- MWC.create
  mwcSeed <- MWC.save mwcGen
  pcgGen <- PCG.create
  pcgSeed <- PCG.save pcgGen
  mtGen <- newPureMT
  mwcState <- initWorkerStates Par (\_ -> MWC.createSystemRandom)
  mwcStateSeq <- initWorkerStates Seq (\_ -> MWC.createSystemRandom)
  pcgState <-
    initWorkerStates Par (\(WorkerId w) -> PCG.initialize (fromIntegral w) 5)
  pcgStateSeq <- initWorkerStates Seq (\_ -> PCG.createSystemRandom)
  defaultMain
    [ bgroup
        "Intro01" -- Representations
        [ bgroup
            "Double"
            [ bench "primitive" $ whnf primitive k
            , bench "unboxed" $ whnf unboxed k
            , bench "storable" $ whnf storable k
            , bench "boxedWHNF" $ whnf boxedWHNF k
            , bench "boxedNF" $ whnf boxedNF k
            ]
        , bgroup
            "Maybe"
            [ bench "B - whnf" $ whnf boxedMaybeWHNF n
            , bench "B - nf" $ nf boxedMaybeWHNF n
            , bench "N - whnf" $ whnf boxedMaybeNF n
            , bench "N - whnf (Par)" $ whnf boxedMaybeParNF n
            ]
        ]
    , bgroup
        "Intro08" -- Random
        [ bgroup
            "Pure"
            [ bgroup
                "Seq"
                [ bench "random" $
                  whnf
                    (computeAs P . randomArrayPureSeq sysGen System.random)
                    sz
                , bench "tf-random" $
                  whnf (computeAs P . randomArrayPureSeq tfGen System.random) sz
                , bench "pcg-random" $
                  whnf
                    (computeAs P . randomArrayPureSeq pcgSeed System.random)
                    sz
                , bench "mersenne-random-pure64" $
                  whnf
                    (computeAs P . randomArrayPureSeq mtGen MT.randomDouble)
                    sz
                , bench "splitmix" $
                  whnf
                    (computeAs P . randomArrayPureSeq smGen SplitMix.nextDouble)
                    sz
                ]
            , bgroup
                "Par"
                [ bench "random" $
                  whnf
                    (computeAs P .
                     randomArrayPurePar sysGen System.split System.random)
                    sz
                , bench "tf-random" $
                  whnf
                    (computeAs P .
                     randomArrayPurePar tfGen System.split System.random)
                    sz
                , bench "pcg-random" $
                  whnf
                    (computeAs P .
                     randomArrayPurePar pcgSeed System.split System.random)
                    sz
                , bench "splitmix" $
                  whnf
                    (computeAs P .
                     randomArrayPurePar
                       smGen
                       SplitMix.splitSMGen
                       SplitMix.nextDouble)
                    sz
                ]
            ]
        , bgroup
            "ST"
            [ bgroup
                "Seq"
                [ bench "splitmix" $
                  whnf (snd . randomArrayPureST smGen SplitMix.nextDouble) sz
                , bench "mersenne-random-pure64" $
                  whnf (snd . randomArrayPureST mtGen MT.randomDouble) sz
                , bench "mwc-random" $
                  whnf (snd . randomArrayMWC mwcSeed MWC.uniform) sz
                , bench "pcg-random" $
                  whnf (snd . randomArrayPCG pcgSeed PCG.uniform) sz
                ]
            ]
        , bgroup
            "IO"
            [ bgroup
                "Seq"
                [ bench "mwc-random" $
                  nfIO (randomArrayIO mwcStateSeq sz MWC.uniform)
                , bench "pcg-random" $
                  nfIO (randomArrayIO pcgStateSeq sz PCG.uniform)
                ]
            , bgroup
                "Par"
                [ bench "mwc-random" $
                  nfIO (randomArrayIO mwcState sz MWC.uniform)
                , bench "pcg-random" $
                  nfIO (randomArrayIO pcgState sz PCG.uniform)
                ]
            ]
        ]
    ]
