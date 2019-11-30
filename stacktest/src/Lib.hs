module Lib
    ( luk, fib, minlist
    ) where


-- Lucas numbers
luk :: Word -> Integer
luk n = snd $ help n (1,2) where 
    help 0 (x, y) = (x, y)
    help n (x,y) = help (n - 1) (x+y, x)


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
