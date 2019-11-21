import Test.QuickCheck
import Lib
import Data.Word

prop_fib :: Word -> Bool
prop_fib n
    | n == 0 = fib n == 0
    | n == 1 = fib n == 1
    | otherwise = (fib n + fib (n+1)) == fib (n + 2) 

main :: IO ()
main = quickCheck prop_fib
