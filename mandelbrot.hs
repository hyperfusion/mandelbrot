module Main where

import BMP
import Data.Binary
import Complex

maxiter :: Int
maxiter = 255

mandel :: Double -> Double -> Int
mandel x y = mandel' (x :+ y) (0 :+ 0) 0
    where mandel' c z i | magnitude z > 2 = i
                        | i > maxiter     = maxiter
                        | otherwise       = mandel' c (c + (z * z)) (i + 1)

draw :: Double -> Double -> Double -> Double -> Integer -> Integer -> [[Int]]
draw x1 x2 y1 y2 w h = [[ mandel x y | x <- map xpx [0..w-1]] | y <- map ypx [0..h-1]]
    where xpx x = x1 + (fromInteger x * (x2 - x1) / fromInteger w)
          ypx y = y1 + (fromInteger y * (y2 - y1) / fromInteger h)

colorize i = RGB x x x where x = fromIntegral (maxiter - i)

plot = BMP 700 700 (map colorize (concat (draw (-2.0) 2.0 (-2.0) 2.0 700 700)))
main = encodeFile "mandelbrot.bmp" plot
