module Lib
    ( fib
    ) where

fib :: Word -> Integer
fib n =  fst $ helper n (0, 1) where 
    helper 0 (x, y) = (x, y)
    helper n (x, y) = helper (n-1) (y, x + y)
