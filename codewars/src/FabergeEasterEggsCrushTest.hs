-- https://www.codewars.com/kata/54cb771c9b30e8b5250011d4/

module FabergeEasterEggsCrushTest
    ( heigth
    ) where


import Data.List

memo :: (Integer -> Integer -> a) -> [[a]]
memo f = map (\x -> map (f x) [0..]) [0..]

heigth1 :: Integer -> Integer -> Integer
heigth1 _ 0 = 0
heigth1 0 _ = 0
heigth1 1 m = m
heigth1 n 1 = 1
heigth1 n m = (heigth n (m - 1)) + (heigth (n - 1) (m - 1)) + 1

         
gwstore :: [[Integer]]
gwstore = memo heigth1

heigth :: Integer -> Integer -> Integer
heigth x y = gwstore `genericIndex` x `genericIndex` y
