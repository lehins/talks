{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Massiv.Array
import System.Environment
import MassivTalk.Day1.Intro09_Stencil
import MassivTalk.Day1.Intro07_Computation
import Data.Massiv.Array.IO
import Data.List as L

main :: IO ()
main =
  getArgs >>= \case
    ["langton"] -> do
      let (r, c, n) = (12, 16, 1000)
      runLangton (Sz (r * 11 :. c * 11)) n
    ["workers"] -> do
      a <- paintThreads (makeArrayR D Par (Sz2 110 211) $ const 0)
      displayImageUsing defaultViewer True $
        computeAs S $ zoomWithGrid 128 (Stride 5) a
    args ->
      putStrLn $ "Unrecognized args: " ++ L.intercalate ", " args
