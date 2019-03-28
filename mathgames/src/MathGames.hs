module MathGames ( allbeginnum ) where

-- Begin number is a number gotten by removing all last digests --
allbeginnum :: Int -> [Int]
allbeginnum x = if x < 10 
                then [] 
                else let y = x `div` 10 in y : (allbeginnum y)
