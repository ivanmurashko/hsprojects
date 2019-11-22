module Lib
    ( fib, minlist
    ) where

fib :: Word -> Integer
fib n =  fst $ helper n (0, 1) where 
    helper 0 (x, y) = (x, y)
    helper n (x, y) = helper (n-1) (y, x + y)

minlist :: [Int] -> Maybe Int
minlist [] = Nothing
minlist [x] = Just x
minlist (x:xs) = minlist xs >>= \y -> Just (min x y)

queueTime :: [Int] -> Int -> Int
queueTime = undefined
