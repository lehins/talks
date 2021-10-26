{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Integral
import Parallel
import Common
import Data.Massiv.Array as A
import qualified Data.Vector.Primitive as VP
import Criterion.Main
import Control.Parallel.Strategies


main :: IO ()
main = do
  let n = 11
      !sz@(Sz2 r c) = Sz2 2001 2001
      !szT = (r, c)
      r2 = r `div` 2
      c2 = c `div` 2
      gaussCentered :: Ix2 -> Double
      gaussCentered (i :. j) =
        gauss (fromIntegral (j - c2)) (fromIntegral (i - r2))
      {-# INLINE gaussCentered #-}
      gaussCenteredT :: (Int, Int) -> Double
      gaussCenteredT (i, j) =
        gauss (fromIntegral (j - c2)) (fromIntegral (i - r2))
      {-# INLINE gaussCenteredT #-}
      -- gaussCentered (i :. j) = sin (fromIntegral (i * i + j * j))
  --     g :: Double -> Double -> Double
  --     g x y = integrate gauss x y
  --     {-# INLINE g #-}
  -- defaultMain
  --   [ bgroup
  --       "Gaussian"
  --       [ bgroup
  --           "Seq"
  --           [ bench "nestedLists" $ nf (makeNestedLists g) n
  --           , bench "squareMatrix" $ nf (makeSquareMatrix g P Seq) n
  --           ]
  --       ,  bgroup
  --           "Par"
  --           [ bench "nestedLists" $
  --             nf (\x -> makeNestedLists g x `using` parList rdeepseq) n
  --           , bench "squareMatrix" $ nf (makeSquareMatrix g P Par) n
  --           ]
  --       ]
  --   , bgroup
  --       "Pi"
  --       [ bgroup
  --           "Seq"
  --           [ bench "nestedLists" $ nf (F.sum . fmap F.sum . makeNestedLists g) n
  --           , bench "squareMatrix" $ nf (A.sum . makeSquareMatrix g D Seq) n
  --           ]
  --       ,  bgroup
  --           "Par"
  --           [ bench "nestedLists" $
  --             nf (F.sum . (\x -> fmap F.sum (makeNestedLists g x) `using` parList rdeepseq)) n
  --           , bench "squareMatrix" $ nf (A.sum . makeSquareMatrix g D Par) n
  --           ]
  --       ]
  --   ]
  defaultMain
    [ -- stack bench --benchmark-arguments="--match pattern Common --output common.html --template $HOME/github/haskell-benchmarks/template-new.tpl +RTS -N1"
      bgroup
      "Common"
        [ -- bench "makeNestedL" $ nf (\s -> makeNestedL s gaussCenteredT) szT
        -- ,
          bench "makeNestedV" $ nf (\s -> makeNestedV s gaussCenteredT) szT
        , bench "makeMatrixMutV (Primitive)" $
          nf (\s -> makeMatrixMutV s gaussCenteredT :: VP.Vector Double) szT
        ]
      -- stack bench --benchmark-arguments="--match pattern \"Vector vs Massiv\" --output vector-massiv.html --template $HOME/github/haskell-benchmarks/template-new.tpl +RTS -N1"
      , bgroup
        "Vector vs Massiv"
        [ bench "makeMatrixMutV" $
          nf (\s -> makeMatrixMutV s gaussCenteredT :: VP.Vector Double) szT
        , bench "makeMatrixS" $
          nf (\s -> makeMatrixS @P s gaussCentered) sz
        ]
      , bgroup
        "Massiv (Seq vs Fork)"
        [ bench "makeMatrixS" $ nf (\s -> makeMatrixS @P s gaussCentered) sz
        , bench "makeMatrixWithFork" $ nf (\s -> makeMatrixWithFork @P s gaussCentered) sz
        ]
      , bgroup
        -- stack bench --benchmark-arguments="--match pattern \"Massiv (Seq vs Fork vs Async)\" --output massiv-seq-vs-fork-vs-async.html --template $HOME/github/haskell-benchmarks/template-new.tpl +RTS -N"
        "Massiv (Seq vs Fork vs Async)"
        [ bench "makeMatrixS" $ nf (\s -> makeMatrixS @P s gaussCentered) sz
        , bench "makeMatrixWithFork" $ nf (\s -> makeMatrixWithFork @P s gaussCentered) sz
        , bench "makeMatrixWithAsync" $ nf (\s -> makeMatrixWithAsync @P s gaussCentered) sz
        ]
      , bgroup
        -- stack bench --benchmark-arguments="--match pattern \"Massiv (Seq vs Fork vs Async vs Pooled)\" --output massiv-seq-vs-fork-vs-async-vs-pooled.html --template $HOME/github/haskell-benchmarks/template-new.tpl +RTS -N"
        "Massiv (Seq vs Fork vs Async vs Pooled)"
        [ bench "makeMatrixS" $ nf (\s -> makeMatrixS @P s gaussCentered) sz
        , bench "makeMatrixWithFork" $ nf (\s -> makeMatrixWithFork @P s gaussCentered) sz
        , bench "makeMatrixWithAsync" $ nf (\s -> makeMatrixWithAsync @P s gaussCentered) sz
        , bench "makeMatrixWithPooled" $ nf (\s -> makeMatrixWithPooled @P s gaussCentered) sz
        ]
      , bgroup
        -- stack bench --benchmark-arguments="--match pattern \"Massiv (Seq vs Fork vs Async vs Pooled vs Streamly)\" --output massiv-seq-vs-fork-vs-async-vs-pooled-vs-streamly.html --template $HOME/github/haskell-benchmarks/template-new.tpl +RTS -N"
        "Massiv (Seq vs Fork vs Async vs Pooled vs Streamly)"
        [ bench "makeMatrixS" $ nf (\s -> makeMatrixS @P s gaussCentered) sz
        , bench "makeMatrixWithFork" $ nf (\s -> makeMatrixWithFork @P s gaussCentered) sz
        , bench "makeMatrixWithAsync" $ nf (\s -> makeMatrixWithAsync @P s gaussCentered) sz
        , bench "makeMatrixWithStreamly" $ nf (\s -> makeMatrixWithStreamly @P s gaussCentered) sz
        , bench "makeMatrixWithPooled" $ nf (\s -> makeMatrixWithPooled @P s gaussCentered) sz
        ]
      , bgroup
        -- stack bench --benchmark-arguments="--match pattern \"Massiv (Seq vs Par vs Pooled)\" --output massiv-seq-vs-par-vs-pooled.html --template $HOME/github/haskell-benchmarks/template-new.tpl +RTS -N"
        "Massiv (Seq vs Par vs Pooled)"
        [ bench "makeMatrix (Seq)" $ nf (\s -> makeMatrix @P Seq s gaussCentered) sz
        , bench "makeMatrix (Par)" $ nf (\s -> makeMatrix @P Par s gaussCentered) sz
        , bench "makeMatrixWithPooled" $ nf (\s -> makeMatrixWithPooled @P s gaussCentered) sz
        ]
      , bgroup
        -- stack bench --benchmark-arguments="--match pattern \"Massiv (Seq vs par vs rpar)\" --output massiv-seq-vs-par-vs-rpar.html --template $HOME/github/haskell-benchmarks/template-new.tpl +RTS -N"
        "Massiv (Seq vs par vs rpar)"
        [ bgroup "massiv"
          [ bench "makeMatrix (Seq)" $ nf (\s -> makeMatrix @P Seq s gaussCentered) sz
          , bench "makeMatrix (Par)" $ nf (\s -> makeMatrix @P Par s gaussCentered) sz
          ]
        , bgroup "parallel"
          [ bench "makeNestedV" $ nf (\s -> makeNestedV s gaussCenteredT) szT
          , bench "makeNestedVpar" $ nf (\s -> makeNestedVpar s gaussCenteredT) szT
          -- , bench "makeNestedVrpar" $ nf (\s -> makeNestedVrpar s gaussCenteredT) szT
          ]
        ]
      , bgroup
        "Massiv"
        [ bgroup "Seq"
          [ bench "makeMatrixMutV (Primitive)" $
            nf (\s -> makeMatrixMutV s gaussCenteredT :: VP.Vector Double) szT
          , bench "makeMatrixS (simple)" $
            nf (\s -> makeMatrixS @P s gaussCentered) sz
          , bench "makeMatrixS (simple)" $
            nf (\s -> makeMatrixS @B s gaussCentered) sz
          , bench "makeArray Seq" $
            nf (\s -> makeArray @P Seq s gaussCentered) sz
          ]
        , bgroup "Par"
          -- , bench "makeMatrixBoxedPar (par)" $ nf (\s -> makeMatrixBoxedPar s gaussCentered) sz
          -- ,
          [ bench "makeMatrixWithFork" $ nf (\s -> makeMatrixWithFork @P s gaussCentered) sz
          , bench "makeMatrixWithAsync" $ nf (\s -> makeMatrixWithAsync @P s gaussCentered) sz
          , bench "makeMatrix (simple)" $ nf (\s -> makeMatrix @P Par s gaussCentered) sz
          , bench "makeArray (Par)" $ nf (\s -> makeArray @P Par s gaussCentered) sz
          , bench "makeMatrixWithPooled" $ nf (\s -> makeMatrixWithPooled @P s gaussCentered) sz
            --   bench "makeMatrixPar" $ nf (flip (makeMatrixPar @P) gaussCentered) sz
            -- , bench "makeMatrixPar'" $ nf (flip (makeMatrixPar' @P) gaussCentered) sz
            -- , bench "makeArray" $ nf (flip (makeArray @P Par) gaussCentered) sz
          ]
        ]
    , bgroup
        "Precise"
        [ bgroup
            "Seq"
            [ bench "nestedLists" $ nf (makeNestedLists (integrate @Double gauss)) n
            , bench "squareMatrix" $ nf (makeSquareMatrix (integrate @Double gauss) B Seq) n
            ]
        ,  bgroup
            "Par"
            [ bench "nestedListsStrat" $
              nf (\x -> makeNestedLists (integrate @Double gauss) x `using` parList rdeepseq) n
            , bench "nestedListsPar" $ nf (makeNestedListsPar (integrate @Double gauss)) n
            , bench "squareMatrix" $ nf (makeSquareMatrix (integrate @Double gauss) B Par) n
            ]
        ]
    ]
