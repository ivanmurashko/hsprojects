-- https://www.codewars.com/kata/the-supermarket-queue/train/haskell
module TheSupermarketQueue
    ( queueTime
    ) where

findmax xs = foldl ( \x y -> max x y ) 0 xs 

mintohead :: [Int] -> [Int]
mintohead [] = []
mintohead (x:xs) = let tmp = helper (x,xs) in (fst tmp) : (snd tmp) 
    where 
      helper (y, []) = (y, [])
      helper (y, ys) = (min t y, (max t y) : ts) 
          where (t, ts) = helper(head ys, tail ys)

queueHelper :: [Int] -> [Int] -> Int
queueHelper xs [] = findmax xs
queueHelper ys1 ys2 = case (mintohead ys1) of 
                      [] -> 0
                      (x:xs) -> queueHelper ((x + head ys2):xs) (tail ys2) 

queueTime :: [Int] -> Int -> Int
queueTime xs n = queueHelper (take n xs) (drop n xs)
