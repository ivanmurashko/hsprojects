import Test.QuickCheck
import Lib

prop_luk :: Word -> Bool
prop_luk n
    | n == 0 = luk n == 2
    | n == 1 = luk n == 1
    | otherwise = (luk n + luk (n+1)) == luk (n + 2) 


prop_fib :: Word -> Bool
prop_fib n
    | n == 0 = fib n == 0
    | n == 1 = fib n == 1
    | otherwise = (fib n + fib (n+1)) == fib (n + 2) 

-- The element is less all others
prop_minlist1 :: [Int] -> Bool
prop_minlist1 xs = filter ( \x -> minlist_less x (minlist xs) ) xs == [] 
    where 
      minlist_less _ Nothing = False
      minlist_less x (Just y) = x < y

-- The element is in the lsit
prop_minlist2 :: [Int] -> Bool
prop_minlist2 xs
    | xs == [] = minlist xs == Nothing
    | otherwise = 
        (length $ filter ( \x -> minlist_eq x (minlist xs) ) xs) > 0 where
            minlist_eq _ Nothing = False
            minlist_eq x (Just y) = x == y



main :: IO ()
main = quickCheck prop_fib >> quickCheck prop_luk >> 
       quickCheck prop_minlist1 >> 
       quickCheck prop_minlist2
