{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Helpers
  ( showBench
  , showMatrix
  , showZoomedMatrix
  , showZoomedLists
  , showImage
  , showAnimation
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.List.NonEmpty as NE
import Data.Massiv.Array as A
import Data.Massiv.Array.IO
import IHaskell.Display

showBench :: String -> Int -> IO ()
showBench name height =
  printDisplay $
  html $
  concat
    [ "<iframe id=\"discussion_iframe\" "
    , "frameborder=\"0\" "
    , "name=\"discussion\" "
    , "src=\"" <> name <> ".html\" "
    , "style=\"overflow: hidden; width: 100%; height: " <>
      show height <> "px;\" "
    , "scrolling=\"auto\">"
    , "Here should be benchmarks"
    , "</iframe>"
    ]

showMatrix ::
     (Elevator e, Load r Ix2 e, Size r)
  => Matrix r e
  -> IO ()
showMatrix arr = do
  bsl <- encodeM PNG () $
    A.computeAs S (fmap (PixelY' . toWord8) (toLoadArray arr) :: Matrix DL (Pixel (Y' SRGB) Word8))
  let Sz2 h w = A.size arr
  printDisplay $ png w h (base64 (BSL.toStrict bsl))

showZoomedMatrix ::
     (Elevator e, Source r e, Load r Ix2 e)
  => Matrix r e
  -> IO ()
showZoomedMatrix = showMatrix . A.zoomWithGrid (fromDouble 0.5) (Stride 25)

showZoomedLists :: Elevator e => [[e]] -> IO ()
showZoomedLists = showZoomedMatrix . fromLists' @U Seq

showImage ::
     (Writable (Auto PNG) (Array r Ix2 e), Size r) => Matrix r e -> IO ()
showImage img = do
  bsl <- encodeM (Auto PNG) () img
  let Sz2 h w = A.size img
  printDisplay $ png w h (base64 (BSL.toStrict bsl))


showAnimation :: forall r . Source r Word8 => Int -> NE.NonEmpty (Matrix r Word8) -> IO ()
showAnimation d mats = do
  let opts =
        SequenceGifOptions
          { sequenceGifPaletteOptions =
              PaletteOptions
                { paletteCreationMethod = Uniform
                , enableImageDithering = False
                , paletteColorCount = 255
                }
          , sequenceGifLooping = LoopingForever
          }
      toImg :: Matrix r Word8 -> Image S (Y' SRGB) Word8
      toImg = A.compute . A.zoomWithGrid 127 (Stride 25) . A.map (PixelY' . (* 255))
      imgs@(img NE.:| _) = toImg <$> mats
  bsl <- encodeM (Auto (Sequence GIF)) opts ((,) d <$> imgs )
  let Sz2 h w = A.size img
  printDisplay $ gif w h (base64 (BSL.toStrict bsl))

