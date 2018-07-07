module PrimeNumbers ( numbers ) where

-- Prime numbers generator --
numbers :: [Int]
numbers = filter (\x -> (length $ filter (\y -> x `mod` y == 0) [2 .. x]) == 1) [2 ..]
