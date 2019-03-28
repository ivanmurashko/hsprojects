module MathGames ( allbeginnum,  isgoodnumber) where

-- Begin number is a number gotten by removing all last digests --
allbeginnum :: Int -> [Int]
allbeginnum x = if x < 10 
                then [] 
                else let y = x `div` 10 in y : (allbeginnum y)

-- Is the digit devided by all begin numbers
isgoodnumber :: Int -> Bool
isgoodnumber x = all (\t -> x `mod` t == 0) $ allbeginnum x

-- Max 6 digit number that divided by all it begins number
max6digit = undefined
