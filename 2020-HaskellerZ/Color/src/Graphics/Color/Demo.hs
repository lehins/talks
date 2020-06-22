{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Color.Demo
  ( displayColor
  , displayColorModelRGB
  , displayColorModelHSV
  ) where

import IHaskell.Display.Hip
import qualified Graphics.Pixel as CM
import Graphics.Pixel.ColorSpace
import Graphics.Image as I


displayColor :: (IHaskellDisplay (Image cs e), ColorModel cs e) => Color cs e -> IO Display
displayColor c = display $ makeImage (Sz2 100 200) (const (Pixel c))


displayColorModelRGB :: Elevator e => Color CM.RGB e -> IO Display
displayColorModelRGB c =
  displayColor (fromBaseModel (fmap toWord8 c) :: Color (SRGB 'NonLinear) Word8)

displayColorModelHSV :: Elevator e => Color CM.HSV e -> IO Display
displayColorModelHSV c =
  displayColor (fromBaseModel (fmap toWord8 c) :: Color (HSV (SRGB 'NonLinear)) Word8)





-- WIP
-- makeGradient3D ::
--      (ColorModel cs e)
--   => (Double -> Double -> Double -> Color cs e)
--   -> Sz2
--   -> Image cs e
-- makeGradient3D f sz@(Sz2 m n) =
--   makeImage sz $ \ (i :. j) -> Pixel (g (fromIntegral j - nd / 2) (fromIntegral i - md / 2))
--   where
--     (md, nd) = (fromIntegral m, fromIntegral n) :: (Double, Double)
--     g x y
--       | c > 0 && s > 1/2 = f 0 x (y - (1/2))
--       | c < 0 && s > 1/2 = f x (y - (1/2)) 0
--       | c < 0 && s > 1/2 && s > (-1/2) = f x (y - (1/2)) 0
--       | c < 0 && s < (-1/2) = 3
--       | c > 0 && s < (-1/2) = 4
--       | otherwise = f 0 x (y - (1/2))
--         where c = x / y
--               s = y / x


-- makeGradient3D ::
--      (ColorModel cs e)
--   => (Float -> Float -> Float -> Pixel cs e)
--   -> Sz2
--   -> I.Image cs e
-- makeGradient3D f sz@(Sz2 m n) =
--   makeImage sz $ \ (i :. j) -> g ((fromIntegral j - nd / 2) / (nd / 2)) ((- (fromIntegral i - md / 2)) / (md / 2))
--   where
--     (md, nd) = (fromIntegral m, fromIntegral n) :: (Float, Float)
--     --g' x y = f 0 x y
--     g x y
--       | x >= 0 && t > 1 / sqrt 3 = f 0 x (y + x * (1/2))
--       | x >= 0 && t > (- 1 / sqrt 3) = f 0 x (y + x * (1/2))
--       | x < 0 && t < (- 1 / sqrt 3) = f (-x) 0 (y + (-x) * (1/2))
--       | x < 0 && t < (1 / sqrt 3) = f (-x) 0 (y + (-x) * (1/2))
--       | x >= 0 = f y (-x + (-y) * (1/2)) 0
--       | otherwise = 0 --f x (-y + x * (1/2)) 0
--         where t = y / x

-- makeGradient3D PixelSRGB (Sz 200)


-- :set -XDataKinds

-- makeGradient3D ::
--      Sz2
--   -> I.Image (I.HSI (SRGB 'NonLinear)) Float
-- makeGradient3D sz@(Sz2 m n) =
--   makeImage sz $ \ (i :. j) -> g ((fromIntegral j - nd / 2) / (nd / 2)) ((- (fromIntegral i - md / 2)) / (md / 2))
--   where
--     (md, nd) = (fromIntegral m, fromIntegral n) :: (Float, Float)
--     g x y
--       | x > 0 = PixelHSI (atan2 y x / pi) 1 0.5
--       | otherwise = PixelHSI (atan2 y x / pi) 1 0.5
--         where t = y / x

-- makeGradient3D :: Sz2 -> I.Image (I.HSI (SRGB 'NonLinear)) Float
-- makeGradient3D sz@(Sz2 m n) =
--   I.makeImage sz $ \ (i :. j) -> g ((fromIntegral j / nd) - 0.5) (-(fromIntegral i / md) - 0.5)
--   where
--     (md, nd) = (fromIntegral m, fromIntegral n) :: (Float, Float)
--     g x y = PixelHSI (atan2 (adj y) (sdj x) / (pi / 2)) 1 0.5)

-- makeGradient3D (Sz 200)
