{-# LANGUAGE FlexibleContexts #-}
module Main where

import Criterion.Main
import MassivTalk.Integral
import Prelude as P

main :: IO ()
main = do
  let b = 1000
      k = 131072
      epsilon = 0.00000000005
      g x = sin x * cos (x * x * x)
  defaultMain
    [ bgroup
        "Naive"
        [ bench "trapezoidRule" $ whnf (integrateTrapezoid k g 0) b
        , bench "List" $ whnf (integrate k g 0) b
        , bench "Array-Seq" $ whnf (integrateNaive k g 0) b
        , bench "Array-Par" $ whnf (integrateNaivePar k g 0) b
        ]
    , bgroup
        "NoDuplicate"
        [ bench "List" $ whnf (integrateNoDuplicateList k g 0) b
        , bench "Array-Seq Bad" $ whnf (integrateNoDuplicateBad k g 0) b
        , bench "Array-Seq" $ whnf (integrateNoDuplicate k g 0) b
        , bench "Array-Par" $ whnf (integrateNoDuplicatePar k g 0) b
        ]
    , bgroup
        "NoAllocate"
        [ bench "List " $ whnf (integrateNoAllocateList k g 0) b
        , bench "Array-Seq" $ whnf (integrateNoAllocate k g 0) b
        , bench "Array-ParN 8" $ whnf (integrateNoAllocateN8 k g 0) b
        ]
    , bgroup
        "RungeRule"
        [ bench "trapezoidalRunge" $ nf (trapezoidalRunge epsilon g 0) b
        , bench "trapezoidalRungeMemo" $ nf (trapezoidalRungeMemo epsilon g 0) b
        , bench "trapezoidalRungeMemoPar" $
          nf (trapezoidalRungeMemoPar epsilon g 0) b
        ]
    ]
