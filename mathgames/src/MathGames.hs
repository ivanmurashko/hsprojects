module MathGames ( allbeginnum,  isgoodnumber, diffdigits) where

-- Numbers with different digits
nmbs :: Int -> [[Int]]
nmbs 1 = map return [1 .. 9]
nmbs n = [x : n | n <- nmbs (n - 1), x <- [0 .. 9], x `notElem` n]

allNmbs :: Int -> [[Int]]
allNmbs 1 = nmbs 1
allNmbs n = allNmbs (n - 1) ++ nmbs n

eval :: [Int] -> Int
eval = snd . foldl (\(m, v) x -> (m * 10, x * m + v)) (1, 0)

-- Generates list of number with different digits
diffdigits = map eval $ allNmbs 10


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
